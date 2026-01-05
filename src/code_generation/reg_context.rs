use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Write;

use koopa::ir::FunctionData;

use crate::array::get_type_size;

// 1. 定义寄存器分配/状态管理的上下文
#[derive(Debug, Clone)]
pub struct RegInfo {
    // 这个寄存器当前存放的是哪个 Koopa Value？
    pub value: Option<koopa::ir::Value>,
    // 脏标记：如果为 true，说明寄存器里的值被修改过，Spill 时必须写回内存
    // 如果为 false（比如刚从内存 load 出来没改过），Spill 时可以直接丢弃，不用 sw
    pub dirty: bool,
}

pub struct FuncContext {
    pub name: String,

    pub phys_regs: Vec<&'static str>,

    // 记录每个物理寄存器的状态 (index 对应 phys_regs)
    reg_status: Vec<RegInfo>,

    // 记录 Value 目前在哪个寄存器里 (用于快速查找)
    // Value -> PhysRegIndex
    value_in_reg: HashMap<koopa::ir::Value, usize>,

    // 记录 Value 在栈上的偏移量 (sp + offset)
    // Value -> Offset
    pub stack_slots: HashMap<koopa::ir::Value, i32>,

    // 当前栈帧大小 (用于分配新的 slot)
    stack_size: i32,
    pub total_frame_size: i32,

    // 最近使用记录 (用于 LRU 算法，决定谁被 Spill)
    // 存的是 PhysRegIndex
    lru_queue: VecDeque<usize>,

    // 正在处理的指令的操作数不能被 Spill
    locked_regs: HashSet<usize>,

    // 当前函数内所有 Call 指令中，最大的参数溢出空间
    // 如果所有 Call 的参数都 <= 8，这里就是 0
    max_outgoing_args_size: i32,
}

impl FuncContext {
    pub fn new(name: String) -> Self {
        // 定义可用的临时寄存器，根据 RISC-V 约定
        // 优先使用 t0-t6, a0-a7
        let regs = vec![
            "t0", "t1", "t2", "t3", "t4", "t5", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
        ];

        let reg_count = regs.len();

        // 初始化 LRU 队列
        let mut lru = VecDeque::new();
        for i in 0..reg_count {
            lru.push_back(i);
        }

        Self {
            name,
            phys_regs: regs,
            reg_status: vec![
                RegInfo {
                    value: None,
                    dirty: false
                };
                reg_count
            ],
            value_in_reg: HashMap::new(),
            stack_slots: HashMap::new(),
            stack_size: 0, // 初始栈偏移，视你的栈帧设计而定
            total_frame_size: 0,
            lru_queue: lru,
            locked_regs: HashSet::new(),
            max_outgoing_args_size: 0,
        }
    }

    // ==========================================
    // Public Interface: 获取/分配/锁定
    // ==========================================

    /// 获取操作数 (Operand) 所在的寄存器
    /// 如果 Value 已经在寄存器中，直接返回
    /// 如果不在，分配一个寄存器（可能触发 Spill），并生成 lw 指令
    pub fn get_reg_for_operand(
        &mut self,
        val: koopa::ir::Value,
        output: &mut String,
    ) -> &'static str {
        // 1. 检查是否已经在寄存器中 (Hit)
        if let Some(&reg_idx) = self.value_in_reg.get(&val) {
            self.touch_lru(reg_idx); // 更新 LRU
            return self.phys_regs[reg_idx];
        }

        // 2. 不在寄存器中 (Miss)，需要 Reload
        // 找一个空闲寄存器或者 Spill 一个受害者
        let reg_idx = self.find_free_reg_or_spill(output);

        // 3. 获取该 Value 在栈上的位置
        let offset = self.get_or_alloc_stack_slot(val);
        let reg_name = self.phys_regs[reg_idx];

        // 4. 生成 Load 指令: lw reg, offset(sp)
        if offset >= -2048 && offset <= 2047 {
            writeln!(output, "  lw    {}, {}(sp)", reg_name, offset).unwrap();
        } else {
            // 大偏移：借用 t6 计算地址
            // t6 = offset
            // t6 = sp + t6
            // lw reg, 0(t6)
            writeln!(output, "  li    t6, {}", offset).unwrap();
            writeln!(output, "  add   t6, sp, t6").unwrap();
            writeln!(output, "  lw    {}, 0(t6)", reg_name).unwrap();
        }

        // 5. 更新状态
        self.update_reg_map(reg_idx, val);
        // 从内存读上来的，和内存一致，所以 dirty = false
        self.reg_status[reg_idx].dirty = false;

        reg_name
    }

    /// 为结果 (Result) 分配一个新寄存器
    /// 这里的 Value 是还没有被计算出来的，所以不需要 Load
    /// 分配后会标记为 dirty，因为接下来马上要被写入
    pub fn alloc_reg_for_result(
        &mut self,
        val: koopa::ir::Value,
        output: &mut String,
    ) -> &'static str {
        // 1. 找一个空闲寄存器或者 Spill
        let reg_idx = self.find_free_reg_or_spill(output);

        // 2. 更新状态
        self.update_reg_map(reg_idx, val);
        // 这是新计算的结果，还没写回内存，所以 dirty = true
        self.reg_status[reg_idx].dirty = true;

        self.phys_regs[reg_idx]
    }

    /// 锁定某个寄存器（通过名字）
    /// 防止在处理同一个指令的其他操作数时，这个寄存器被 Spill 掉
    pub fn lock_reg(&mut self, reg_name: &str) {
        if let Some(idx) = self.phys_regs.iter().position(|&r| r == reg_name) {
            self.locked_regs.insert(idx);
        }
    }

    /// 解锁所有寄存器（通常在一条指令生成完毕后调用）
    pub fn unlock_all(&mut self) {
        self.locked_regs.clear();
    }

    pub fn get_temp_reg_for_address_calc(&mut self, _output: &mut String) -> &'static str {
        "t6"
    }

    // ==========================================
    // Internal Logic: Spilling & LRU
    // ==========================================

    /// 寻找可用寄存器，如果满了则根据 LRU 逐出 (Spill) 一个
    pub fn find_free_reg_or_spill(&mut self, output: &mut String) -> usize {
        // 1. 尝试直接找一个完全空闲的 (Value == None)
        // 优先分配空闲的可以避免不必要的 Spill
        for (i, status) in self.reg_status.iter().enumerate() {
            if status.value.is_none() && !self.locked_regs.contains(&i) {
                self.touch_lru(i);
                return i;
            }
        }

        // 2. 没有空闲，必须 Spill
        // 从 LRU 队列头部开始找，找到第一个【未被锁定】的寄存器
        let mut victim_idx = 0;
        let mut found = false;

        // 我们需要遍历队列来找，而不是简单 pop_front，因为队头可能被 lock 了
        for (i, &reg_idx) in self.lru_queue.iter().enumerate() {
            if !self.locked_regs.contains(&reg_idx) {
                victim_idx = reg_idx;
                // 将其从队列中移除并放到队尾 (Touch)
                self.lru_queue.remove(i);
                self.lru_queue.push_back(victim_idx);
                found = true;
                break;
            }
        }

        if !found {
            panic!("Register exhaustion: All registers are locked! (Instruction too complex?)");
        }

        // 3. 执行 Spill 操作
        let reg_name = self.phys_regs[victim_idx]; // &'static str 是 Copy 的，不会造成借用问题

        let (victim_value, is_dirty) = {
            let info = &self.reg_status[victim_idx];
            (info.value, info.dirty)
        };

        if let Some(old_val) = victim_value {
            // 如果是脏的，必须写回栈
            if is_dirty {
                let offset = self.get_or_alloc_stack_slot(old_val);
                if offset >= -2048 && offset <= 2047 {
                    writeln!(output, "  sw    {}, {}(sp)", reg_name, offset).unwrap();
                } else {
                    // 使用 t6
                    writeln!(output, "  li    t6, {}", offset).unwrap();
                    writeln!(output, "  add   t6, sp, t6").unwrap();
                    writeln!(output, "  sw    {}, 0(t6)", reg_name).unwrap();
                }
            }

            // 解除旧 Value 到这个寄存器的映射
            self.value_in_reg.remove(&old_val);
        }

        let info = &mut self.reg_status[victim_idx];
        info.value = None;
        info.dirty = false;

        victim_idx
    }

    /// 更新寄存器状态映射
    fn update_reg_map(&mut self, reg_idx: usize, new_val: koopa::ir::Value) {
        self.reg_status[reg_idx].value = Some(new_val);
        self.value_in_reg.insert(new_val, reg_idx);
        self.touch_lru(reg_idx);
    }

    /// 更新 LRU 队列：将 reg_idx 移到队尾 (表示最近被使用)
    fn touch_lru(&mut self, reg_idx: usize) {
        // 找到它在队列中的位置并移除
        if let Some(pos) = self.lru_queue.iter().position(|&x| x == reg_idx) {
            self.lru_queue.remove(pos);
        }
        // 插入到队尾
        self.lru_queue.push_back(reg_idx);
    }

    /// 获取 Value 的栈偏移，如果没有则分配一个新的
    fn get_or_alloc_stack_slot(&mut self, val: koopa::ir::Value) -> i32 {
        if let Some(&offset) = self.stack_slots.get(&val) {
            return offset;
        }

        // 分配新的 slot
        // 假设栈向下增长，这里简单模拟，具体根据你的 Prologue 调整
        // 比如：当前栈顶是 stack_size，分配 4 字节
        let offset = self.stack_size;
        self.stack_size += 4;

        self.stack_slots.insert(val, offset);
        offset
    }

    /// 将所有当前占用的寄存器强制写回栈，并清空状态
    pub fn spill_all(&mut self, output: &mut String) {
        for i in 0..self.reg_status.len() {
            // 我们不能直接在这里 borrow self.reg_status[i]，因为还要调用 methods
            // 所以先拷贝状态
            let (val_opt, is_dirty) = {
                let info = &self.reg_status[i];
                (info.value, info.dirty)
            };

            if let Some(val) = val_opt {
                let reg_name = self.phys_regs[i];

                // 如果是脏的，写回栈
                if is_dirty {
                    let offset = self.get_or_alloc_stack_slot(val);
                    if offset >= -2048 && offset <= 2047 {
                        writeln!(output, "  sw    {}, {}(sp)", reg_name, offset).unwrap();
                    } else {
                        // 使用 t6 计算地址，绝对安全
                        writeln!(output, "  li    t6, {}", offset).unwrap();
                        writeln!(output, "  add   t6, sp, t6").unwrap();
                        writeln!(output, "  sw    {}, 0(t6)", reg_name).unwrap();
                    }
                }

                // 无论是否 dirty，都要移除映射
                // 因为 call 返回后，寄存器里的值已经不可信了
                self.value_in_reg.remove(&val);
            }

            // 清理状态
            let info = &mut self.reg_status[i];
            info.value = None;
            info.dirty = false;
        }
    }

    pub fn analyze_frame(&mut self, func: &FunctionData) {
        self.stack_slots.clear();

        // 1. 预留参数区
        let mut max_args_space = 0;
        for (_, bb_node) in func.layout().bbs() {
            for (&inst, _) in bb_node.insts() {
                // 检查是否是 Call 指令
                if let koopa::ir::ValueKind::Call(call) = func.dfg().value(inst).kind() {
                    let arg_count = call.args().len();
                    if arg_count > 8 {
                        // 超过 8 个的参数需要存栈，每个 4 字节
                        let space = (arg_count - 8) as i32 * 4;
                        if space > max_args_space {
                            max_args_space = space;
                        }
                    }
                }
            }
        }

        self.max_outgoing_args_size = max_args_space;

        // ==========================================
        // 步骤 2: 为所有局部变量 (Alloc / Spill) 分配空间
        // 这个区域紧接着参数区之上
        // ==========================================
        let mut current_offset = self.max_outgoing_args_size;

        for (_, bb_node) in func.layout().bbs() {
            for (&inst, _) in bb_node.insts() {
                let ty = func.dfg().value(inst).ty();

                // 凡是有返回值的指令，都预留一个栈槽 (全栈策略)
                // 这样无论何时 Spill，都有地方去
                if !ty.is_unit() {
                    self.stack_slots.insert(inst, current_offset);
                    current_offset += 4; // 假设都是 4 字节
                }
            }
        }

        // 记录除去 RA/S0 之外的栈大小 (用于调试或特定计算)

        self.max_outgoing_args_size = max_args_space;
        current_offset += max_args_space;

        // 2. 预分配所有指令
        for (_, bb) in func.layout().bbs() {
            for (&inst, _) in bb.insts() {
                let value_data = func.dfg().value(inst);
                let ty = value_data.ty();

                // 如果是 Void 类型，不分配
                if ty.is_unit() {
                    continue;
                }

                // 记录当前分配的偏移量
                self.stack_slots.insert(inst, current_offset);

                // === 【核心修复】计算需要分配的大小 ===
                let size = match value_data.kind() {
                    koopa::ir::ValueKind::Alloc(_) => {
                        // 对于 Alloc 指令，ty 是指针类型 (例如 *[i32, 10])
                        // 我们需要分配的是它指向的数据的大小 (即 [i32, 10] 的大小)
                        if let koopa::ir::TypeKind::Pointer(base_ty) = ty.kind() {
                            get_type_size(base_ty) // 调用你之前写的 get_type_size
                        } else {
                            4 // 理论上不可能发生
                        }
                    }
                    _ => {
                        // 对于其他指令 (Load, Add, GetElemPtr...)
                        // 它们产生的是一个临时值 (i32 或 指针)
                        // 只需要分配 4 字节来存储这个“值”本身
                        // (假设 RV32 架构，所有临时变量/地址都占 4 字节)
                        get_type_size(ty)
                        // 或者简单写成 4，因为 SysY 中非 alloc 的结果通常都能塞进寄存器/4字节栈槽
                    }
                };

                // 累加偏移量 (对齐到 4 字节，虽然 i32 默认就是 4)
                current_offset += size as i32;
            }
        }

        self.stack_size = current_offset;

        // 3. 计算最终大小 (RA + S0)
        let total = self.stack_size + 8;
        self.total_frame_size = (total + 15) & !15;
    }
}
