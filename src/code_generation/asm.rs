use koopa::ir::{
    dfg::DataFlowGraph,
    layout::BasicBlockNode,
    values::{Alloc, Binary, Branch, Call, Jump, Load, Return, Store},
    BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind,
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Write;

// 1. 定义寄存器分配/状态管理的上下文
struct FuncContext {
    name: String,

    phys_regs: Vec<&'static str>,

    // 记录每个物理寄存器的状态 (index 对应 phys_regs)
    reg_status: Vec<RegInfo>,

    // 记录 Value 目前在哪个寄存器里 (用于快速查找)
    // Value -> PhysRegIndex
    value_in_reg: HashMap<Value, usize>,

    // 记录 Value 在栈上的偏移量 (sp + offset)
    // Value -> Offset
    stack_slots: HashMap<Value, i32>,

    // 当前栈帧大小 (用于分配新的 slot)
    stack_size: i32,
    total_frame_size : i32,

    // 最近使用记录 (用于 LRU 算法，决定谁被 Spill)
    // 存的是 PhysRegIndex
    lru_queue: VecDeque<usize>,

    // 正在处理的指令的操作数不能被 Spill
    locked_regs: HashSet<usize>,

    // 当前函数内所有 Call 指令中，最大的参数溢出空间
    // 如果所有 Call 的参数都 <= 8，这里就是 0
    max_outgoing_args_size: i32,
}

#[derive(Debug, Clone)]
struct RegInfo {
    // 这个寄存器当前存放的是哪个 Koopa Value？
    value: Option<Value>,
    // 脏标记：如果为 true，说明寄存器里的值被修改过，Spill 时必须写回内存
    // 如果为 false（比如刚从内存 load 出来没改过），Spill 时可以直接丢弃，不用 sw
    dirty: bool,
}

impl FuncContext {
    pub fn new(name: String) -> Self {
        // 定义可用的临时寄存器，根据 RISC-V 约定
        // 优先使用 t0-t6, a0-a7
        let regs = vec![
            "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6",
            "a7",
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
            total_frame_size : 0,
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
    pub fn get_reg_for_operand(&mut self, val: Value, output: &mut String) -> &'static str {
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

        // 4. 生成 Load 指令: lw reg, offset(sp)
        let reg_name = self.phys_regs[reg_idx];
        writeln!(output, "  lw    {}, {}(sp)", reg_name, offset).unwrap();

        // 5. 更新状态
        self.update_reg_map(reg_idx, val);
        // 从内存读上来的，和内存一致，所以 dirty = false
        self.reg_status[reg_idx].dirty = false;

        reg_name
    }

    /// 为结果 (Result) 分配一个新寄存器
    /// 这里的 Value 是还没有被计算出来的，所以不需要 Load
    /// 分配后会标记为 dirty，因为接下来马上要被写入
    pub fn alloc_reg_for_result(&mut self, val: Value, output: &mut String) -> &'static str {
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

    pub fn get_temp_reg_for_address_calc(&mut self, output: &mut String) -> &'static str {
        // 1. 调用现有的逻辑找一个可用的坑位
        // 如果所有寄存器都满了，find_free_reg_or_spill 会自动把最久没用的那个 Spill 到栈上
        // 并清空该寄存器的状态 (value=None, dirty=false)
        let reg_idx = self.find_free_reg_or_spill(output);

        // 2. 直接返回名字
        // 注意：我们【不】调用 update_reg_map，也不设置 reg_status.value
        // 因为这个寄存器马上就会被用作临时用途，用完就可以被当作"空闲"的再次分配
        self.phys_regs[reg_idx]
    }

    // ==========================================
    // Internal Logic: Spilling & LRU
    // ==========================================

    /// 寻找可用寄存器，如果满了则根据 LRU 逐出 (Spill) 一个
    fn find_free_reg_or_spill(&mut self, output: &mut String) -> usize {
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
                writeln!(output, "  sw    {}, {}(sp)", reg_name, offset).unwrap();
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
    fn update_reg_map(&mut self, reg_idx: usize, new_val: Value) {
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
    fn get_or_alloc_stack_slot(&mut self, val: Value) -> i32 {
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

                // 如果是 dirty，写回栈
                if is_dirty {
                    let offset = self.get_or_alloc_stack_slot(val);
                    writeln!(output, "  sw    {}, {}(sp)", reg_name, offset).unwrap();
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

    /// 分析整个函数的栈帧需求，计算总大小并为每个变量预分配栈槽
    pub fn analyze_frame(&mut self, func: &FunctionData) {
        // ==========================================
        // 步骤 1: 扫描最大的函数调用参数空间 (Outgoing Args)
        // 这个区域位于栈的最底部 (0(sp) ~ max_args_space(sp))
        // ==========================================
        let mut max_args_space = 0;

        for (_, bb_node) in func.layout().bbs() {
            for (&inst,_) in bb_node.insts() {
                // 检查是否是 Call 指令
                if let ValueKind::Call(call) = func.dfg().value(inst).kind() {
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
            for (&inst,_) in bb_node.insts() {
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
        self.stack_size = current_offset;

        // ==========================================
        // 步骤 3: 计算最终对齐的总大小
        // 加上 RA(4) + S0(4) = 8 字节
        // ==========================================
        let size_with_regs = self.stack_size + 8;
        
        // 16 字节对齐
        self.total_frame_size = (size_with_regs + 15) & !15;
    }
}

// 2. 定义代码生成器主结构体
pub struct AsmBuilder<'a> {
    // 最终输出的汇编字符串
    pub output: String,
    // Option 是因为在处理全局变量时可能不需要它，或者用来显式控制生命周期
    ctx: Option<FuncContext>,
    program: &'a Program,
}

impl<'a> AsmBuilder<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            output: String::new(),
            ctx: None,
            program,
        }
    }

    pub fn generate_program(&mut self, program: &Program) {
        for &value in program.inst_layout() {
            self.generate_global(program, value);
        }

        for (_func, func_data) in program.funcs() {
            // 如果只有声明没有定义的函数（比如库函数），通常 func_data.layout().entry_bb() 是 None
            if func_data.layout().entry_bb().is_some() {
                self.generate_function(func_data);
            }
        }
    }

    fn generate_global(&mut self, program: &Program, value: Value) {
        // 获取 Value 的详细数据
        let value_data = program.borrow_value(value);

        // 我们只关心 GlobalAlloc 指令
        if let ValueKind::GlobalAlloc(alloc) = value_data.kind() {
            // 1. 获取变量名 (去掉 @ 前缀)
            let name = value_data.name().as_ref().unwrap().replace("@", "");

            // 2. 获取初始值
            let init_val = alloc.init();
            let init_data = program.borrow_value(init_val);

            // 3. 根据初始值类型决定放入 .data 还是 .bss
            match init_data.kind() {
                // === 情况 A: 整数初始化 (int a = 10;) -> .data ===
                ValueKind::Integer(int) => {
                    let _ = writeln!(self.output, "  .data"); // 切换到数据段
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    let _ = writeln!(self.output, "  .word {}", int.value()); // 写入 4 字节整数
                    let _ = writeln!(self.output, ""); // 空行美观
                }

                // === 情况 B: 零初始化 (int a;) -> .bss ===
                ValueKind::ZeroInit(_) => {
                    let _ = writeln!(self.output, "  .bss"); // 切换到 BSS 段
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    // .zero N 表示分配 N 字节并填 0
                    // i32 占 4 字节
                    let _ = writeln!(self.output, "  .zero 4");
                    let _ = writeln!(self.output, "");
                }

                // === 情况 C: 聚合类型 (数组) ===
                // 如果你以后支持数组，这里会是 ValueKind::Aggregate
                // 需要递归生成 .word 或 .zero
                _ => {
                    // 暂时处理不了复杂类型，或者留空
                }
            }
        }
    }

    fn generate_function(&mut self, func: &FunctionData) {
        // 1. 【重置上下文】
        self.ctx = Some(FuncContext::new(func.name().to_string()));
        self.ctx.as_mut().unwrap().analyze_frame(func);

        // 2. 打印函数标签
        let name = &func.name()[1..];
        writeln!(self.output, "\n  .text").unwrap();
        writeln!(self.output, "  .globl {}", name).unwrap();
        writeln!(self.output, "{}:", name).unwrap();

        // 保存 output，用于暂存函数体
        let mut final_output = std::mem::take(&mut self.output);

        // 3. 遍历基本块生成指令
        let dfg = func.dfg();
        for (bb_handle, node) in func.layout().bbs() {
            self.generate_bb(bb_handle, node, dfg);
        }

        // === 【关键步骤 B】 计算最终栈大小 ===
        let ctx = self.ctx.as_ref().unwrap();

        // 16 字节对齐
        let align_stack_size = ctx.total_frame_size;

        // === 【关键步骤 C】 组装 Prologue (序言) ===
        // 栈布局 (High -> Low):
        // [ ... Caller Stack ... ]
        // [ RA (Ret Addr)        ] <- align_stack_size - 4
        // [ S0 (Old FP)          ] <- align_stack_size - 8
        // [ ... Locals/Spill ... ]
        // [ ... Outgoing Args... ]
        // [ ... Current SP ...   ] <- 0

        if align_stack_size < 2048 {
            // --- 情况 A: 栈帧较小 (直接用立即数) ---
            writeln!(final_output, "  addi  sp, sp, -{}", align_stack_size).unwrap();
            writeln!(final_output, "  sw    ra, {}(sp)", align_stack_size - 4).unwrap();
            writeln!(final_output, "  sw    s0, {}(sp)", align_stack_size - 8).unwrap();
        } else {
            // --- 情况 B: 栈帧较大 (需要计算地址) ---
            // 1. 调整 SP
            writeln!(final_output, "  li    t0, -{}", align_stack_size).unwrap();
            writeln!(final_output, "  add   sp, sp, t0").unwrap();

            // 2. 保存 ra, s0 (因为偏移量太大，不能直接 sw ra, 20000(sp))
            // 先计算栈顶地址到 t0: t0 = sp + align_stack_size
            writeln!(final_output, "  li    t0, {}", align_stack_size).unwrap();
            writeln!(final_output, "  add   t0, sp, t0").unwrap();

            writeln!(final_output, "  sw    ra, -4(t0)").unwrap();
            writeln!(final_output, "  sw    s0, -8(t0)").unwrap();
        }

        // === 【关键步骤 D】 组装 Epilogue (结语) ===
        // 构造恢复现场的指令序列
        let epilogue = if align_stack_size < 2048 {
            format!(
                "  lw    ra, {0}(sp)\n  lw    s0, {1}(sp)\n  addi  sp, sp, {2}\n  ret",
                align_stack_size - 4,
                align_stack_size - 8,
                align_stack_size
            )
        } else {
            // 大栈帧恢复
            // 1. 恢复 ra, s0 (先算栈顶地址)
            format!(
                "  li    t0, {0}\n  add   t0, sp, t0\n  lw    ra, -4(t0)\n  lw    s0, -8(t0)\n  li    t0, {0}\n  add   sp, sp, t0\n  ret",
                align_stack_size
            )
        };

        // 获取暂存的函数体汇编
        let body_asm = std::mem::take(&mut self.output);

        // 替换占位符
        let final_body = body_asm.replace("#RET_PLACEHOLDER#", &epilogue);

        // === 【关键步骤 E】 合并与还原 ===
        final_output.push_str(&final_body);
        self.output = final_output;
        self.ctx = None;
    }

    fn get_bb_label(&self, bb: &BasicBlock , dfg: &DataFlowGraph) -> String {
        // 1. 获取函数名，并去掉开头的 '@'
        // self.func.name() 返回的是 "@main"，我们需要 "main"
        let func_name = &self.ctx.as_ref().unwrap().name[1..];
        
        // 2. 获取基本块名，并去掉开头的 '%'
        // bb_name 返回的是 "%entry" 或 "%while_entry_1"，我们需要 "entry"
        let raw_bb_name = dfg.bb(*bb).name().as_ref().unwrap();
        let bb_name = &raw_bb_name[1..];

        // 3. 拼接成标准格式: .L<func>_<bb>
        format!(".L{}_{}", func_name, bb_name)
    }

    fn generate_bb(&mut self, bb: &BasicBlock, node: &BasicBlockNode, dfg: &DataFlowGraph) {
        // 生成基本块标签，例如 .L0:
        // 如果是入口块，有些汇编器不需要标签，但加上也无妨
        // 你可以用 HashMap 映射 bb handle 到标签名，或者直接用名字
        let label = self.get_bb_label(bb, dfg);
        let _ = writeln!(self.output, "{}:", label);

        // 遍历指令
        for (val, _inst_node) in node.insts() {
            // 关键：把 inst_handle (ID) 和 dfg (数据表) 一起传下去
            self.generate_inst(*val, dfg);
        }
    }

    fn generate_inst(&mut self, inst: Value, dfg: &DataFlowGraph) {
        // 使用 dfg.value(inst) 来获取指令的具体数据
        let value_data = dfg.value(inst);

        match value_data.kind() {
            ValueKind::Binary(bin) => {
                // 此时我们需要 helper 函数也接受 dfg，因为操作数 bin.lhs() 也是 handle
                self.process_binary(inst, bin, dfg);
            }
            ValueKind::Return(ret) => {
                self.process_return(ret, dfg);
            }
            ValueKind::Alloc(alloc) => {
                self.process_alloc(inst, alloc, dfg);
            }
            ValueKind::Load(load) => {
                self.process_load(inst, load, dfg);
            }
            ValueKind::Store(store) => {
                self.process_store(store, dfg);
            }
            ValueKind::Branch(br) => {
                self.process_branch(br, dfg);
            }
            ValueKind::Jump(j) => {
                self.process_jump(j, dfg);
            }
            ValueKind::Call(c) => {
                self.process_call(inst, c, dfg);
            }
            _ => {}
        }
    }

    fn resolve_operand(&mut self, val: Value, dfg: &DataFlowGraph) -> String {
        if val.is_global() {
            // 1. 从 Program 中获取数据 (不能用 dfg.value!)
            let data = self.program.borrow_value(val);
            
            // 2. 获取全局变量名字 (去掉 @)
            let name = data.name().as_ref().expect("Global must have name").replace("@", "");

            // 3. 生成加载地址指令 (la reg, symbol)
            // 因为 resolve_operand 的契约是返回一个"存有该值的寄存器"
            // 对于全局变量指针，我们需要把它的地址 load 到临时寄存器里
            let tmp_reg = self
                .ctx
                .as_mut()
                .unwrap()
                .get_temp_reg_for_address_calc(&mut self.output);
            
            writeln!(self.output, "  la    {}, {}", tmp_reg, name).unwrap();
            
            return tmp_reg.to_string();
        }
        let value_data = dfg.value(val);

        match value_data.kind() {
            // === 情况 1: 立即数 (Integer) ===
            ValueKind::Integer(int) => {
                let imm = int.value();
                if imm == 0 {
                    // 优化：0 直接用 x0
                    "x0".to_string()
                } else {
                    // 非 0 立即数：需要一个临时寄存器
                    // 注意：立即数不对应任何 Value，所以不需要 update_reg_map

                    // 1. 找一个空闲寄存器 (可能会触发 Spill，写入 sw 指令到 self.output)
                    let reg_idx = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .find_free_reg_or_spill(&mut self.output);

                    let reg_name = self.ctx.as_mut().unwrap().phys_regs[reg_idx];

                    // 2. 生成加载立即数的指令
                    writeln!(self.output, "  li    {}, {}", reg_name, imm).unwrap();

                    // 3. 【关键】这个寄存器存的是临时值，不是某个 Value
                    // 我们不需要将其 bind 到 val 上。
                    // 但是，为了防止处理下一个操作数时把它 spill 掉，最好锁住它
                    // (如果你在外部调用处统一 lock，这里可以不 lock，但建议返回后在外部 lock)
                    self.ctx.as_mut().unwrap().lock_reg(reg_name);

                    reg_name.to_string()
                }
            }

            koopa::ir::ValueKind::FuncArgRef(arg) => {
                let index = arg.index();
                if index < 8 {
                    // 如果是前 8 个参数，直接返回对应的物理寄存器名
                    format!("a{}", index)
                } else {
                    if index < 8 {
                        // 情况 A: 寄存器传参 (a0 - a7)
                        format!("a{}", index)
                    } else {
                        // 情况 B: 栈传参 (index >= 8)
                        // 1. 获取当前函数的栈帧大小
                        // 注意：这要求你在 generate_function 开头已经计算好了 ctx.stack_size (align_stack_size)
                        let frame_size = self.ctx.as_ref().unwrap().total_frame_size;
                        // 如果你的 stack_size 还没加上 ra/s0 的 8 字节，记得加上:
                        // let frame_size = self.ctx.as_ref().unwrap().raw_stack_size_with_ra_s0_aligned;

                        // 2. 计算相对于当前 SP 的偏移量
                        let offset = frame_size + (index as i32 - 8) * 4;

                        // 3. 申请一个临时寄存器来存放加载进来的参数
                        // 因为 resolve_operand 必须返回一个寄存器名供后续指令使用
                        let tmp_reg = self
                            .ctx
                            .as_mut()
                            .unwrap()
                            .get_temp_reg_for_address_calc(&mut self.output);

                        // 4. 生成 Load 指令 (注意处理大立即数)
                        if offset >= -2048 && offset <= 2047 {
                            writeln!(self.output, "  lw    {}, {}(sp)", tmp_reg, offset).unwrap();
                        } else {
                            // 偏移量太大，需要先算地址
                            writeln!(self.output, "  li    {}, {}", tmp_reg, offset).unwrap();
                            writeln!(self.output, "  add   {}, {}, sp", tmp_reg, tmp_reg).unwrap();
                            writeln!(self.output, "  lw    {}, 0({})", tmp_reg, tmp_reg).unwrap();
                        }

                        // 5. 返回临时寄存器名
                        tmp_reg.to_string()
                    }
                }
            }

            // === 情况 2: 变量 (Value) ===
            _ => {
                // 变量可能在寄存器里，也可能在栈里。
                // 如果在栈里，get_reg_for_operand 会自动生成 lw 指令并更新状态。

                let reg_name = self
                    .ctx
                    .as_mut()
                    .unwrap()
                    .get_reg_for_operand(val, &mut self.output);

                // 返回寄存器名 (因为 phys_regs 是 &'static str，所以要转 String)
                reg_name.to_string()
            }
        }
    }

    fn process_binary(&mut self, result_val: Value, bin: &Binary, dfg: &DataFlowGraph) {
        // 1. 准备左操作数 (LHS)
        // 注意：resolve_operand 可能会生成 li 指令，也会消耗寄存器池
        let lhs_reg = self.resolve_operand(bin.lhs(), dfg);

        // 2. 准备右操作数 (RHS)
        let rhs_reg = self.resolve_operand(bin.rhs(), dfg);

        // 3. 为当前运算的结果分配一个目标寄存器 (Dest)
        // alloc_reg 会将 result_val 绑定到该寄存器，供后续指令使用
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(result_val, &mut self.output);

        match bin.op() {
            // === 算术运算 ===
            BinaryOp::Add => {
                let _ = writeln!(
                    self.output,
                    "  add   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }
            BinaryOp::Sub => {
                let _ = writeln!(
                    self.output,
                    "  sub   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }
            BinaryOp::Mul => {
                let _ = writeln!(
                    self.output,
                    "  mul   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }
            BinaryOp::Div => {
                let _ = writeln!(
                    self.output,
                    "  div   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }

            // === 位运算 ===
            BinaryOp::And => {
                let _ = writeln!(
                    self.output,
                    "  and   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }
            BinaryOp::Or => {
                let _ = writeln!(
                    self.output,
                    "  or    {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }
            BinaryOp::Xor => {
                let _ = writeln!(
                    self.output,
                    "  xor   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }

            // === 比较运算 (生成 0 或 1) ===

            // Eq (等于): x == y
            // 逻辑: xor 结果为 0 表示相等，seqz (Set if Equal Zero) 将 0 变 1
            BinaryOp::Eq => {
                let _ = writeln!(
                    self.output,
                    "  xor   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
                let _ = writeln!(self.output, "  seqz  {}, {}", dest_reg, dest_reg);
            }

            // Lt (小于): x < y
            // 直接使用 slt (Set Less Than)
            BinaryOp::Lt => {
                let _ = writeln!(
                    self.output,
                    "  slt   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }

            // Gt (大于): x > y
            // 逻辑: 相当于 y < x，交换操作数位置使用 slt
            BinaryOp::Gt => {
                let _ = writeln!(
                    self.output,
                    "  slt   {}, {}, {}",
                    dest_reg, rhs_reg, lhs_reg
                );
            }

            // Le (小于等于): x <= y
            // 逻辑: x <= y 等价于 !(x > y) 等价于 !(y < x)
            // 实现: slt dest, rhs, lhs (判断 y < x?) -> xori dest, dest, 1 (取反)
            // 或者使用伪指令: sgt dest, lhs, rhs (也就是 slt rhs, lhs) -> seqz/xori
            BinaryOp::Le => {
                let _ = writeln!(
                    self.output,
                    "  slt   {}, {}, {}",
                    dest_reg, rhs_reg, lhs_reg
                ); // dest = (x > y)
                let _ = writeln!(self.output, "  xori  {}, {}, 1", dest_reg, dest_reg);
                // dest = !(x > y)
            }

            // Ge (大于等于): x >= y
            // 逻辑: x >= y 等价于 !(x < y)
            BinaryOp::Ge => {
                let _ = writeln!(
                    self.output,
                    "  slt   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                ); // dest = (x < y)
                let _ = writeln!(self.output, "  xori  {}, {}, 1", dest_reg, dest_reg);
                // dest = !(x < y)
            }

            // NotEq (不等于): x != y
            // 逻辑: xor 结果不为 0 表示不等，使用 sltu dest, x0, dest 设置非零为 1
            BinaryOp::NotEq => {
                let _ = writeln!(
                    self.output,
                    "  xor   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
                let _ = writeln!(self.output, "  sltu  {}, x0, {}", dest_reg, dest_reg);
            }

            BinaryOp::Mod => {
                let _ = writeln!(
                    self.output,
                    "  rem   {}, {}, {}",
                    dest_reg, lhs_reg, rhs_reg
                );
            }

            _ => panic!("暂不支持的二元运算: {:?}", bin.op()),
        }

        // 优化：清理临时寄存器
        // 如果 lhs_reg 或 rhs_reg 是我们在 resolve_operand 里临时分配给立即数的(不在 val_map 中)
        // 理论上这里应该回收它们以供复用。
        // 但为了保持代码简单，对于"简单寄存器分配"，你可以选择不回收(直到耗尽)，或者在这里做一个简单的检查回收。
    }

    fn process_return(&mut self, ret: &Return, dfg: &DataFlowGraph) {
        // 1. 如果有返回值，将其移动到 a0
        if let Some(val) = ret.value() {
            let value_data = dfg.value(val);

            match value_data.kind() {
                // === 情况 A: 返回值是立即数 (例如 return 0;) ===
                // 直接写入 a0，不需要经过寄存器分配器，也不占用临时寄存器
                ValueKind::Integer(int) => {
                    let imm = int.value();
                    // 直接生成 li a0, imm
                    writeln!(self.output, "  li    a0, {}", imm).unwrap();
                }

                // === 情况 B: 返回值是变量 (例如 return %1;) ===
                _ => {
                    // 1. 让 Context 帮我们找到这个变量在哪里
                    //    - 如果它在某个寄存器(如 t1)中，这里返回 "t1"
                    //    - 如果它被 Spill 到了栈上，get_reg_for_operand 会自动生成 lw t? offset(sp)，然后返回那个 t?
                    let reg = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .get_reg_for_operand(val, &mut self.output);

                    // 2. 如果它不在 a0，就搬运到 a0
                    //    (如果 get_reg_for_operand 刚好分配了 a0 给它，这步就省了)
                    if reg != "a0" {
                        writeln!(self.output, "  mv    a0, {}", reg).unwrap();
                    }
                }
            }
        }

        // 2. 【关键】写入占位符
        // 后续会在 generate_function 结束时被替换为真正的 Epilogue (恢复栈指针 + ret)
        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        writeln!(self.output, "#RET_PLACEHOLDER#").unwrap();
    }

    fn process_alloc(&mut self, _result_val: Value, _alloc: &Alloc, _dfg: &DataFlowGraph) {
      {}
    }
    fn process_load(&mut self, result_val: Value, load: &Load, dfg: &DataFlowGraph) {
        let src_ptr = load.src();

        // 1. 分配目标寄存器
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(result_val, &mut self.output);

        // 2. 检查源是否是栈槽
        if let Some(offset) = self.ctx.as_mut().unwrap().stack_slots.get(&src_ptr) {
            // === 情况 A: 从局部变量加载 ===
            // 汇编: lw dest_reg, offset(sp)
            if *offset >= -2048 && *offset <= 2047 {
                writeln!(self.output, "  lw    {}, {}(sp)", dest_reg, offset).unwrap();
            } else {
                // 处理大偏移...
            }
        } else {
            // === 情况 B: 从普通指针加载 ===
            let ptr_reg = self.resolve_operand(src_ptr, dfg);
            writeln!(self.output, "  lw    {}, 0({})", dest_reg, ptr_reg).unwrap();
        }
    }

    fn process_store(&mut self, store: &Store, dfg: &DataFlowGraph) {
        let val = store.value();
        let dest_ptr = store.dest();

        // 1. 准备要存的值
        let val_reg = self.resolve_operand(val, dfg);
        let val_reg_str = val_reg.to_string();
        self.ctx.as_mut().unwrap().lock_reg(&val_reg_str);

        // 2. 检查 dest_ptr 是 Alloc 出来的栈槽，还是普通指针？
        let offset_opt = self
            .ctx
            .as_ref()
            .unwrap()
            .stack_slots
            .get(&dest_ptr)
            .copied();

        if let Some(offset) = offset_opt {
            // === 情况 A: 存入局部变量 (Alloc) ===
            // 直接使用 sp + offset 寻址
            // 汇编: sw val_reg, offset(sp)

            // 处理大立即数偏移 (超过 +/- 2047)
            if offset >= -2048 && offset <= 2047 {
                writeln!(self.output, "  sw    {}, {}(sp)", val_reg, offset).unwrap();
            } else {
                // 只有偏移量超级大时，才需要临时算地址
                // 注意：这里用一个临时寄存器，用完即扔，不用分配
                let tmp_reg = self
                    .ctx
                    .as_mut()
                    .unwrap()
                    .get_temp_reg_for_address_calc(&mut self.output);
                writeln!(self.output, "  li    {}, {}", tmp_reg, offset).unwrap();
                writeln!(self.output, "  add   {}, {}, sp", tmp_reg, tmp_reg).unwrap();
                writeln!(self.output, "  sw    {}, 0({})", val_reg, tmp_reg).unwrap();
            }
        } else {
            // === 情况 B: 存入普通指针 (比如数组指针参数，或者全局变量地址) ===
            // 这个指针本身存在寄存器里
            let ptr_reg = self.resolve_operand(dest_ptr, dfg);
            writeln!(self.output, "  sw    {}, 0({})", val_reg, ptr_reg).unwrap();
        }

        self.ctx.as_mut().unwrap().unlock_all();
    }

    fn process_branch(&mut self, br: &Branch, dfg: &DataFlowGraph) {
        let cond_reg = self.resolve_operand(br.cond(), dfg);
        let true_name = self.get_bb_label(&br.true_bb(), dfg);
        let false_name = self.get_bb_label(&br.false_bb(), dfg);
        
        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        let _ = writeln!(self.output, "  bnez  {}, {}", cond_reg, true_name);
        let _ = writeln!(self.output, "  j     {}", false_name);
    }
    // 在 generate_inst 或 process_jump 中
    fn process_jump(&mut self, jump: &Jump, dfg: &DataFlowGraph) {

        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        // 1. 获取跳转目标的名称 (例如 %end_2)
        let target_bb = jump.target();
        let target_name = self.get_bb_label(&target_bb, dfg);

        // 2. 处理标签格式 (去掉 %, 去掉 @)
        let asm_label = target_name.replace("%", "").replace("@", "");

        // 3. 【必须】生成 RISC-V 的无条件跳转指令
        writeln!(self.output, "  j     {}", asm_label).unwrap();
    }

    fn process_call(&mut self, inst_val: Value, call: &Call, dfg: &DataFlowGraph) {
        // 1. 【关键】保护现场：Spill 所有活跃寄存器
        // 必须在处理参数之前做，否则参数计算用的寄存器也会被清理
        // 但这里有个技巧：参数也是 Value，如果先 Spill，后面 get_reg 会重新 load，虽然慢但安全。
        // 为了性能，通常先计算参数，再 Spill 其他的。
        // 简单起见，我们先 Spill All，绝对安全。
        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        let args = call.args();

        // 2. 传递参数
        for (i, &arg) in args.iter().enumerate() {
            // 获取参数的值 (Load 到寄存器)
            let val_data = dfg.value(arg);
            let val_reg = match val_data.kind() {
                // 情况 A: 参数是立即数 (half(10))
                koopa::ir::ValueKind::Integer(int) => {
                    let imm = int.value();
                    let tmp = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .get_temp_reg_for_address_calc(&mut self.output);
                    writeln!(self.output, "  li    {}, {}", tmp, imm).unwrap();
                    tmp.to_string()
                }
                // 情况 B: 参数是变量
                _ => self
                    .ctx
                    .as_mut()
                    .unwrap()
                    .get_reg_for_operand(arg, &mut self.output)
                    .to_string(),
            };

            if i < 8 {
                // 前 8 个参数 -> 寄存器 a0-a7
                let target_reg = format!("a{}", i);
                if val_reg != target_reg {
                    writeln!(self.output, "  mv    {}, {}", target_reg, val_reg).unwrap();
                    // 注意：mv 到了 aX 后，aX 可能会被后续参数的计算覆盖
                    // 如果你的 get_reg 可能会用到 aX，这里就有冲突风险。
                    // 全栈策略+SpillAll 可以规避这个问题，因为 get_reg 会 reload 到 tX。
                }
            } else {
                // 后续参数 -> 栈 (0(sp), 4(sp)...)
                let offset = (i - 8) * 4;
                writeln!(self.output, "  sw    {}, {}(sp)", val_reg, offset).unwrap();
            }
        }

        // 3. 生成 Call 指令
        let callee_name = &self.program.func(call.callee()).name()[1..];
        writeln!(self.output, "  call  {}", callee_name).unwrap();

        // 4. 处理返回值
        if !dfg.value(inst_val).ty().is_unit() {
            // 此时返回值在 a0
            // 我们需要把它"接管"过来，分配一个寄存器存起来 (标记为 dirty)
            let dest_reg = self
                .ctx
                .as_mut()
                .unwrap()
                .alloc_reg_for_result(inst_val, &mut self.output);

            if dest_reg != "a0" {
                writeln!(self.output, "  mv    {}, a0", dest_reg).unwrap();
            }
        }
    }
}
