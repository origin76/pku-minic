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
        // 1. 【重置上下文】进入新函数，清空寄存器分配状态
        self.ctx = Some(FuncContext::new());

        // 2. 打印函数标签 (去除 @ 前缀)
        let name = &func.name()[1..];
        use std::fmt::Write; // 允许 write! 宏写入 String
        let _ = writeln!(self.output, "\n  .text");
        let _ = writeln!(self.output, "  .globl {}", name);
        let _ = writeln!(self.output, "{}:", name);

        // 3. 序言 (Prologue): 保存 fp, ra, 移动 sp 等 (如果需要)
        // ...

        // 4. 获取 DFG (Data Flow Graph)，指令的具体信息都在这里
        let dfg = func.dfg();

        // 5. 遍历基本块 (利用 Koopa 的 Layout)
        for (bb_handle, node) in func.layout().bbs() {
            // node 是基本块节点，bb_handle 是 basic_block的 ID
            // 我们把 dfg 传下去，因为处理指令时需要查表
            self.generate_bb(bb_handle, node, dfg);
        }

        // 6. 结尾清理
        if let Some(ctx) = &self.ctx {
            if ctx.stack_size > 0 {
                let _ = writeln!(self.output, "  addi  sp, sp, {}", ctx.stack_size);
            }
        }
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

    fn process_return(&mut self, ret_val: &Return, dfg: &DataFlowGraph) {
        if ret_val.value().is_some() {
            let val = ret_val.value().unwrap();
            let reg = self.resolve_operand(val, dfg);
            let _ = writeln!(self.output, "  mv    a0 , {}", reg);
        }
        let _ = writeln!(self.output, "  ret");
    }

    fn process_alloc(&mut self, result_val: Value, _alloc: &Alloc, _dfg: &DataFlowGraph) {
        // 1. 在 Context 中分配栈空间 (这只是增加计数器，不动 SP)
        // 假设 alloc i32 需要 4 字节
        // 注意：这里分配的是"变量本身的存储空间"
        let offset = self
            .ctx
            .as_mut()
            .unwrap()
            .get_or_alloc_stack_slot(result_val);

        // 2. 分配结果寄存器 (用来存放这个变量的地址指针)
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(result_val, &mut self.output);

        // 3. 计算地址: dest_reg = sp + offset
        // 只有 offset 超出立即数范围时才需要特殊处理，通常直接 addi 即可
        if offset >= -2048 && offset <= 2047 {
            let _ = writeln!(self.output, "  addi  {}, sp, {}", dest_reg, offset);
        } else {
            // 如果 offset 太大，需要先 li 到临时寄存器再 add (这里简化处理)
            let _ = writeln!(self.output, "  li    {}, {}", dest_reg, offset);
            let _ = writeln!(self.output, "  add   {}, {}, sp", dest_reg, dest_reg);
        }
    }

    fn process_load(&mut self, result_val: Value, load: &Load, dfg: &DataFlowGraph) {
        // 1. 获取源地址寄存器
        let ptr_reg = self.resolve_operand(load.src(), dfg);
        let ptr_reg_str = ptr_reg.to_string();

        // 【关键】锁定源地址寄存器
        self.ctx.as_mut().unwrap().lock_reg(&ptr_reg_str);

        // 2. 分配目标寄存器 (此时 ptr_reg 不会被选为 victim)
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(result_val, &mut self.output);

        // 3. 生成 Load 指令
        let _ = writeln!(self.output, "  lw    {}, 0({})", dest_reg, ptr_reg);

        // 4. 解锁
        self.ctx.as_mut().unwrap().unlock_all();
    }

    fn process_store(&mut self, store: &Store, dfg: &DataFlowGraph) {
        // 1. 获取地址寄存器
        let ptr_reg = self.resolve_operand(store.dest(), dfg);
        let ptr_reg_str = ptr_reg.to_string(); // 复制名字以避免借用冲突

        // 【关键】锁定地址寄存器，防止在解析 value 时被 Spill 掉
        self.ctx.as_mut().unwrap().lock_reg(&ptr_reg_str);

        // 2. 获取值寄存器
        let val_reg = self.resolve_operand(store.value(), dfg);
        let val_reg_str = val_reg.to_string();

        // 锁定值寄存器 (虽然这之后马上就用了，但为了习惯一致性可以锁)
        self.ctx.as_mut().unwrap().lock_reg(&val_reg_str);

        // 3. 生成指令
        let _ = writeln!(self.output, "  sw    {}, 0({})", val_reg, ptr_reg);

        // 4. 解锁所有
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
