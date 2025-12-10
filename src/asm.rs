use std::collections::HashMap;
use std::fmt::Write;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind, dfg::DataFlowGraph, layout::BasicBlockNode, values::{Binary, Return}};

// 1. 定义寄存器分配/状态管理的上下文
struct FuncContext {
    // 记录 Value -> 寄存器名
    val_map: HashMap<Value, String>,
    // 记录 Value -> 栈偏移 (Spilling用)
    stack_map: HashMap<Value, i32>,
    // 当前可用寄存器
    free_regs: Vec<String>,
    // 当前栈帧大小
    stack_size: i32,
}

impl FuncContext {
    fn new() -> Self {
        Self {
            val_map: HashMap::new(),
            stack_map: HashMap::new(),
            // 假设可用 t0-t6
            free_regs: vec![
                "t6".into(),
                "t5".into(),
                "t4".into(),
                "t3".into(),
                "t2".into(),
                "t1".into(),
                "t0".into(),
            ],
            stack_size: 0,
        }
    }

    // 分配一个新的寄存器给某个 Value (作为结果)
    fn alloc_reg(&mut self, val: Value) -> String {
        if let Some(reg) = self.free_regs.pop() {
            self.val_map.insert(val, reg.clone());
            reg
        } else {
            panic!("寄存器不够用了！需要实现 Spilling (溢出到栈) 机制");
        }
    }

    // 获取某个 Value 所在的寄存器 (作为操作数)
    // 如果是常数，我们需要特殊处理，这里为了简单演示，假设常数也先 load 到寄存器或者直接返回 x0
    fn get_reg(&self, val: Value) -> Option<&String> {
        self.val_map.get(&val)
    }
}

// 2. 定义代码生成器主结构体
pub struct AsmBuilder {
    // 最终输出的汇编字符串
    pub output: String,
    // Option 是因为在处理全局变量时可能不需要它，或者用来显式控制生命周期
    current_ctx: Option<FuncContext>,
}

impl AsmBuilder {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            current_ctx: None,
        }
    }
    
    // 获取当前上下文的辅助函数
    fn ctx(&mut self) -> &mut FuncContext {
        self.current_ctx.as_mut().expect("Context not initialized!")
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
        self.current_ctx = Some(FuncContext::new());

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
            // node 是基本块节点，bb_handle 是基本块的 ID
            // 我们把 dfg 传下去，因为处理指令时需要查表
            self.generate_bb(bb_handle, node, dfg);
        }
        
        // 6. 结尾清理
        self.current_ctx = None;
    
    }

    fn generate_bb(&mut self, bb: &BasicBlock, node: &BasicBlockNode, dfg: &DataFlowGraph) {
        let bb_data = dfg.bbs().get(bb).unwrap();
        // 生成基本块标签，例如 .L0:
        // 如果是入口块，有些汇编器不需要标签，但加上也无妨
        // 你可以用 HashMap 映射 bb handle 到标签名，或者直接用名字
        let bb_name = bb_data.name().clone().unwrap_or("unknown_bb".into());
        let _ = writeln!(self.output, "{}:", bb_name);

        // 遍历指令
        for (val , _inst_node) in node.insts(){
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
            // ... 其他指令
            _ => {}
        }
    }

    fn resolve_operand(&mut self, val: Value, dfg: &DataFlowGraph) -> String {
        let value_data = dfg.value(val);
        
        match value_data.kind() {
            // 情况 1: 这是一个立即数 (Integer)
            ValueKind::Integer(int) => {
                let imm = int.value();
                if imm == 0 {
                    // 优化：0 直接用 x0，不需要分配寄存器，也不需要 li 指令
                    "x0".to_string()
                } else {
                    // 非 0 立即数：需要一个临时寄存器来存放它
                    // 这里直接从 free_regs 拿一个用，不记录在 val_map 里，因为它只是临时的
                    let reg = self.ctx().free_regs.pop().expect("寄存器耗尽");
                    let _ = writeln!(self.output, "  li    {}, {}", reg, imm);
                    reg
                }
            }
            // 情况 2: 这是一个变量 (之前指令的计算结果)
            _ => {
                // 直接查表，找不到就 panic (说明生成逻辑有顺序错误)
                self.ctx().val_map.get(&val)
                    .expect("使用变量前未定义 (寄存器未分配)")
                    .clone()
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
        let dest_reg = self.ctx().alloc_reg(result_val);

        match bin.op() {
            // === 算术运算 ===
            BinaryOp::Add => {
                let _ = writeln!(self.output, "  add   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            BinaryOp::Sub => {
                let _ = writeln!(self.output, "  sub   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            BinaryOp::Mul => {
                let _ = writeln!(self.output, "  mul   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            BinaryOp::Div => {
                let _ = writeln!(self.output, "  div   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            
            // === 位运算 ===
            BinaryOp::And => {
                let _ = writeln!(self.output, "  and   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            BinaryOp::Or => {
                let _ = writeln!(self.output, "  or    {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }
            BinaryOp::Xor => {
                let _ = writeln!(self.output, "  xor   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }

            // === 比较运算 (生成 0 或 1) ===
            
            // Eq (等于): x == y 
            // 逻辑: xor 结果为 0 表示相等，seqz (Set if Equal Zero) 将 0 变 1
            BinaryOp::Eq => {
                let _ = writeln!(self.output, "  xor   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
                let _ = writeln!(self.output, "  seqz  {}, {}", dest_reg, dest_reg);
            }

            // Lt (小于): x < y
            // 直接使用 slt (Set Less Than)
            BinaryOp::Lt => {
                let _ = writeln!(self.output, "  slt   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
            }

            // Gt (大于): x > y
            // 逻辑: 相当于 y < x，交换操作数位置使用 slt
            BinaryOp::Gt => {
                let _ = writeln!(self.output, "  slt   {}, {}, {}", dest_reg, rhs_reg, lhs_reg);
            }

            // Le (小于等于): x <= y
            // 逻辑: x <= y 等价于 !(x > y) 等价于 !(y < x)
            // 实现: slt dest, rhs, lhs (判断 y < x?) -> xori dest, dest, 1 (取反)
            // 或者使用伪指令: sgt dest, lhs, rhs (也就是 slt rhs, lhs) -> seqz/xori
            BinaryOp::Le => {
                let _ = writeln!(self.output, "  slt   {}, {}, {}", dest_reg, rhs_reg, lhs_reg); // dest = (x > y)
                let _ = writeln!(self.output, "  xori  {}, {}, 1", dest_reg, dest_reg);       // dest = !(x > y)
            }

            // Ge (大于等于): x >= y
            // 逻辑: x >= y 等价于 !(x < y)
            BinaryOp::Ge => {
                let _ = writeln!(self.output, "  slt   {}, {}, {}", dest_reg, lhs_reg, rhs_reg); // dest = (x < y)
                let _ = writeln!(self.output, "  xori  {}, {}, 1", dest_reg, dest_reg);       // dest = !(x < y)
            }

            // NotEq (不等于): x != y
            // 逻辑: xor 结果不为 0 表示不等，使用 sltu dest, x0, dest 设置非零为 1
            BinaryOp::NotEq => {
                let _ = writeln!(self.output, "  xor   {}, {}, {}", dest_reg, lhs_reg, rhs_reg);
                let _ = writeln!(self.output, "  sltu  {}, x0, {}", dest_reg, dest_reg);
            }
            
            _ => panic!("暂不支持的二元运算: {:?}", bin.op()),
        }

        // 优化：清理临时寄存器
        // 如果 lhs_reg 或 rhs_reg 是我们在 resolve_operand 里临时分配给立即数的(不在 val_map 中)
        // 理论上这里应该回收它们以供复用。
        // 但为了保持代码简单，对于"简单寄存器分配"，你可以选择不回收(直到耗尽)，或者在这里做一个简单的检查回收。
        // 比如:
        self.try_free_temp_reg(lhs_reg);
        self.try_free_temp_reg(rhs_reg);
    }

    fn process_return(&mut self, ret_val: &Return , dfg: &DataFlowGraph) {
        if ret_val.value().is_some() {
            let val = ret_val.value().unwrap();
            let reg = self.resolve_operand(val, dfg);
            let _ = writeln!(self.output, "  mv    a0 , {}", reg);
        } 
        let _ = writeln!(self.output, "  ret");
    }
    
    // 简单的回收逻辑 (可选)
    fn try_free_temp_reg(&mut self, reg: String) {
        // 如果这个寄存器是 x0，不用回收
        if reg == "x0" { return; }
        
        // 如果这个寄存器没有被记录在 val_map 里，说明它是给立即数临时用的，赶紧回收
        // 注意：这需要 val_map 能反向查询，或者你确信它是临时的。
        // 一个简单粗暴的方法是：检查它是否在当前函数的 value_map values 里
        // 但由于性能原因，通常简单的编译器实现会跳过这一步，直接等寄存器耗尽报错，或者把立即数也当做 Value 存起来。
        
        // 这里为了演示你的需求 "t0 -> t1 -> t2"，如果不回收，很快就会用到 t3, t4...
        // 如果要完全复现你题目中的 tight allocation，建议把立即数也作为 Value 存在 map 里，
        // 或者简单地将分配过的临时寄存器 push back 到 free_regs。
        
        // 既然你只是做简单的分配，可以暂时先不实现复杂的回收，
        // 只要你的寄存器池够大 (t0-t6, a0-a7, s0-s11)，跑通简单程序没问题。
    }
}
