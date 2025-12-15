use koopa::ir::{
    dfg::DataFlowGraph,
    layout::BasicBlockNode,
    values::{Alloc, Binary, Branch, Load, Return, Store},
    BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind,
};
use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Write;

// 1. 定义寄存器分配/状态管理的上下文
struct FuncContext {
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

    // 最近使用记录 (用于 LRU 算法，决定谁被 Spill)
    // 存的是 PhysRegIndex
    lru_queue: VecDeque<usize>,

    // 【新增】当前被锁定的寄存器索引集合
    // 正在处理的指令的操作数不能被 Spill
    locked_regs: HashSet<usize>,
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
    pub fn new() -> Self {
        // 定义可用的临时寄存器，根据 RISC-V 约定
        // 优先使用 t0-t6, a0-a7
        let regs = vec![
            "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6",
            "a7",
        ];

        let reg_count = regs.len();

        // 初始化 LRU 队列，顺序无所谓
        let mut lru = VecDeque::new();
        for i in 0..reg_count {
            lru.push_back(i);
        }

        Self {
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
            lru_queue: lru,
            locked_regs: HashSet::new(),
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
}

// 2. 定义代码生成器主结构体
pub struct AsmBuilder {
    // 最终输出的汇编字符串
    pub output: String,
    // Option 是因为在处理全局变量时可能不需要它，或者用来显式控制生命周期
    ctx: Option<FuncContext>,
}

impl AsmBuilder {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            ctx: None,
        }
    }

    pub fn generate_program(&mut self, program: &Program) {
        // 1. 先生成全局变量 (.data) 等
        // ...

        // 2. 遍历函数
        // 你的 IR 结构：(_func_handle, func_data)
        for (_func, func_data) in program.funcs() {
            // 如果只有声明没有定义的函数（比如库函数），通常 func_data.layout().entry_bb() 是 None
            if func_data.layout().entry_bb().is_some() {
                self.generate_function(func_data);
            }
        }
    }

    // 生成单个函数
    fn generate_function(&mut self, func: &FunctionData) {
        // 1. 【重置上下文】
        self.ctx = Some(FuncContext::new());

        // 2. 打印函数标签到【最终输出】
        let name = &func.name()[1..];
        // 注意：这里还是写到 self.output，因为标签是在函数最前面的
        writeln!(self.output, "\n  .text").unwrap();
        writeln!(self.output, "  .globl {}", name).unwrap();
        writeln!(self.output, "{}:", name).unwrap();

        // === 【关键步骤 A】 准备临时缓冲区 ===
        // 我们把 self.output 里的内容（主要是前面的全局代码）先移走，
        // 或者创建一个新的 String 给函数体用。
        // 为了不修改 generate_bb 等函数的签名，我们用 std::mem::take 偷梁换柱。

        // 保存之前的 output (包含 .data 和 函数标签)
        let mut final_output = std::mem::take(&mut self.output);

        // 此时 self.output 变成了空字符串，用来暂存函数体汇编

        // 3. 遍历基本块生成指令 (生成的代码会进入 self.output 暂存)
        let dfg = func.dfg();
        for (bb_handle, node) in func.layout().bbs() {
            self.generate_bb(bb_handle, node, dfg);
        }

        // === 【关键步骤 B】 计算最终栈大小并对齐 ===
        let ctx = self.ctx.as_ref().unwrap();
        // RISC-V 要求栈指针 16 字节对齐
        let raw_stack_size = ctx.stack_size;
        let align_stack_size = (raw_stack_size + 15) & !15;

        // === 【关键步骤 C】 组装最终代码 ===

        // C1. 写入 Prologue (分配栈空间) 到 final_output
        if align_stack_size > 0 {
            // 支持大立即数处理
            if align_stack_size < 2048 {
                writeln!(final_output, "  addi  sp, sp, -{}", align_stack_size).unwrap();
            } else {
                writeln!(final_output, "  li    t0, -{}", align_stack_size).unwrap();
                writeln!(final_output, "  add   sp, sp, t0").unwrap();
            }
        }

        // C2. 处理 Epilogue (替换占位符)
        // 从 self.output (暂存区) 拿出生成的函数体
        let body_asm = std::mem::take(&mut self.output);

        // 构造真正的 return 序列
        let epilogue = if align_stack_size > 0 {
            if align_stack_size < 2048 {
                format!("  addi  sp, sp, {}\n  ret", align_stack_size)
            } else {
                format!(
                    "  li    t0, {}\n  add   sp, sp, t0\n  ret",
                    align_stack_size
                )
            }
        } else {
            "  ret".to_string()
        };

        // 将占位符替换为真正的 epilogue
        // 假设我们在 process_ret 里写入了 "#RET_PLACEHOLDER#"
        let final_body = body_asm.replace("#RET_PLACEHOLDER#", &epilogue);

        // C3. 将处理好的函数体追加到 final_output
        final_output.push_str(&final_body);

        // === 【关键步骤 D】 还原 self.output ===
        self.output = final_output;
        self.ctx = None;
    }

    fn generate_bb(&mut self, bb: &BasicBlock, node: &BasicBlockNode, dfg: &DataFlowGraph) {
        let bb_data = dfg.bbs().get(bb).unwrap();
        // 生成基本块标签，例如 .L0:
        // 如果是入口块，有些汇编器不需要标签，但加上也无妨
        // 你可以用 HashMap 映射 bb handle 到标签名，或者直接用名字
        let bb_name = &bb_data.name().clone().unwrap_or("unknown_bb".into())[1..];
        let _ = writeln!(self.output, "{}:", bb_name);

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
            _ => {}
        }
    }

    fn resolve_operand(&mut self, val: Value, dfg: &DataFlowGraph) -> String {
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
                    // 假设 self.ctx 是 FuncContext，self.output 是 String
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

                    reg_name.to_string()
                }
            }

            // === 情况 2: 变量 (Value) ===
            _ => {
                // 不能只用 get() 查表了！
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
        writeln!(self.output, "#RET_PLACEHOLDER#").unwrap();
    }
    
    fn process_alloc(&mut self, result_val: Value, _alloc: &Alloc, _dfg: &DataFlowGraph) {
        // 1. 分配栈槽
        // 假设 alloc i32 占 4 字节
        let offset = self.ctx.as_mut().unwrap().stack_size;
        self.ctx.as_mut().unwrap().stack_size += 4; // 仅仅增加计数器

        // 2. 记录映射：这个 IR Value 对应栈上的这个偏移
        // 【关键】不要调用 alloc_reg_for_result！不要给它分配寄存器！
        self.ctx
            .as_mut()
            .unwrap()
            .stack_slots
            .insert(result_val, offset);

        // 3. 函数结束，什么汇编都不写
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
        let true_bb_data = dfg.bbs().get(&br.true_bb()).unwrap();
        let true_name = &true_bb_data.name().as_ref().unwrap()[1..];
        let false_bb_data = dfg.bbs().get(&br.false_bb()).unwrap();
        let false_name = &false_bb_data.name().as_ref().unwrap()[1..];
        let _ = writeln!(self.output, "  bnez  {}, {}", cond_reg, true_name);
        let _ = writeln!(self.output, "  j     {}", false_name);
    }
}
