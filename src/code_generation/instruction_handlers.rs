use koopa::ir::{
    dfg::DataFlowGraph,
    values::{Alloc, Binary, Branch, Call, Jump, Load, Return, Store},
    BinaryOp, Value, ValueKind,
};
use std::fmt::Write;

impl super::asm::AsmBuilder<'_> {
    // 注意：这里的实现还在 asm.rs 中，这里只是占位
    // 后续会移动全部 process_* 方法到这里

    pub fn process_binary(&mut self, result_val: Value, bin: &Binary, dfg: &DataFlowGraph) {
        // 1. 准备左操作数 (LHS)
        // 注意：resolve_operand 可能会生成 li 指令，也会消耗寄存器池
        let lhs_reg = self.resolve_operand(bin.lhs(), dfg);
        self.ctx.as_mut().unwrap().lock_reg(lhs_reg.as_str());

        // 2. 准备右操作数 (RHS)
        let rhs_reg = self.resolve_operand(bin.rhs(), dfg);
        self.ctx.as_mut().unwrap().lock_reg(rhs_reg.as_str());

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
        self.ctx.as_mut().unwrap().unlock_all();
        // 优化：清理临时寄存器
        // 如果 lhs_reg 或 rhs_reg 是我们在 resolve_operand 里临时分配给立即数的(不在 val_map 中)
        // 理论上这里应该回收它们以供复用。
        // 但为了保持代码简单，对于"简单寄存器分配"，你可以选择不回收(直到耗尽)，或者在这里做一个简单的检查回收。
    }

    pub fn process_return(&mut self, ret: &Return, dfg: &DataFlowGraph) {
        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

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

    pub fn process_alloc(&mut self, _result_val: Value, _alloc: &Alloc, _dfg: &DataFlowGraph) {
        {}
    }

    pub fn process_load(&mut self, result_val: Value, load: &Load, dfg: &DataFlowGraph) {
        let src_ptr = load.src();

        // 1. 分配目标寄存器
        let dest_reg = self
            .ctx
            .as_mut()
            .unwrap()
            .alloc_reg_for_result(result_val, &mut self.output);

        // ================================================================
        // 【关键修复】区分 "读取局部变量" 和 "读取指针指向的内存"
        // ================================================================

        // 判断源是否是局部 Alloc 指令
        let is_local_alloc = if src_ptr.is_global() {
            false
        } else {
            matches!(dfg.value(src_ptr).kind(), koopa::ir::ValueKind::Alloc(_))
        };

        // 2. 检查是否可以直接从栈槽加载 (Case A)
        if is_local_alloc {
            let offset_opt = self
                .ctx
                .as_ref()
                .unwrap()
                .stack_slots
                .get(&src_ptr)
                .copied();

            if let Some(offset) = offset_opt {
                // === 情况 A: 从局部变量加载 (Alloc) ===
                // 语义：读取变量的值
                if offset >= -2048 && offset <= 2047 {
                    writeln!(self.output, "  lw    {}, {}(sp)", dest_reg, offset).unwrap();
                } else {
                    let tmp = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .get_temp_reg_for_address_calc(&mut self.output);
                    writeln!(self.output, "  li    {}, {}", tmp, offset).unwrap();
                    writeln!(self.output, "  add   {}, sp, {}", tmp, tmp).unwrap();
                    writeln!(self.output, "  lw    {}, 0({})", dest_reg, tmp).unwrap();
                }

                self.ctx.as_mut().unwrap().unlock_all();
                return;
            }
        }

        // === 情况 B: 从指针加载 (GetElemPtr, Global, Param...) ===
        // 语义：先获取指针的值(地址)，然后解引用该地址

        let ptr_reg = self.resolve_operand(src_ptr, dfg);

        // 生成: lw dest, 0(ptr)
        writeln!(self.output, "  lw    {}, 0({})", dest_reg, ptr_reg).unwrap();

        self.ctx.as_mut().unwrap().unlock_all();
    }

    pub fn process_store(&mut self, store: &Store, dfg: &DataFlowGraph) {
        let val = store.value();
        let dest = store.dest();

        // 1. 准备要存的值
        let val_reg = self.resolve_operand(val, dfg);
        let val_reg_str = val_reg.to_string();
        self.ctx.as_mut().unwrap().lock_reg(&val_reg_str);

        // ================================================================
        // 【关键修复】安全检查 dest 类型
        // ================================================================

        // 判断是否是局部 Alloc 指令
        let is_local_alloc = if dest.is_global() {
            // 全局变量不算局部 Alloc，它应该走下面的 resolve_operand -> la 逻辑
            false
        } else {
            // 只有非全局的，才去 DFG 里查
            matches!(dfg.value(dest).kind(), koopa::ir::ValueKind::Alloc(_))
        };

        // 2. 尝试直接写栈 (Case A)
        if is_local_alloc {
            let offset_opt = self.ctx.as_ref().unwrap().stack_slots.get(&dest).copied();

            if let Some(offset) = offset_opt {
                // === 情况 A: 存入局部变量 (Alloc) ===
                if offset >= -2048 && offset <= 2047 {
                    writeln!(self.output, "  sw    {}, {}(sp)", val_reg_str, offset).unwrap();
                } else {
                    let tmp_reg = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .get_temp_reg_for_address_calc(&mut self.output); // 使用 t6
                    writeln!(self.output, "  li    {}, {}", tmp_reg, offset).unwrap();
                    writeln!(self.output, "  add   {}, {}, sp", tmp_reg, tmp_reg).unwrap();
                    writeln!(self.output, "  sw    {}, 0({})", val_reg_str, tmp_reg).unwrap();
                }

                self.ctx.as_mut().unwrap().unlock_all();
                return;
            }
        }

        // === 情况 B: 目标是指针 (GetElemPtr, Global, Load...) ===
        // 1. 如果是 GetElemPtr: resolve_operand 返回计算好的地址寄存器
        // 2. 如果是 Global: resolve_operand 会生成 la 指令，返回存有全局地址的寄存器
        let ptr_reg = self.resolve_operand(dest, dfg);
        writeln!(self.output, "  sw    {}, 0({})", val_reg_str, ptr_reg).unwrap();

        self.ctx.as_mut().unwrap().unlock_all();
    }

    pub fn process_branch(&mut self, br: &Branch, dfg: &DataFlowGraph) {
        let cond_reg = self.resolve_operand(br.cond(), dfg);
        let true_name = self.get_bb_label(&br.true_bb(), dfg);
        let false_name = self.get_bb_label(&br.false_bb(), dfg);

        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        let _ = writeln!(self.output, "  bnez  {}, {}", cond_reg, true_name);
        let _ = writeln!(self.output, "  j     {}", false_name);
    }

    pub fn process_jump(&mut self, jump: &Jump, dfg: &DataFlowGraph) {
        self.ctx.as_mut().unwrap().spill_all(&mut self.output);

        // 1. 获取跳转目标的名称 (例如 %end_2)
        let target_bb = jump.target();
        let target_name = self.get_bb_label(&target_bb, dfg);

        // 2. 处理标签格式 (去掉 %, 去掉 @)
        let asm_label = target_name.replace("%", "").replace("@", "");

        // 3. 【必须】生成 RISC-V 的无条件跳转指令
        writeln!(self.output, "  j     {}", asm_label).unwrap();
    }

    pub fn process_call(&mut self, inst_val: Value, call: &Call, dfg: &DataFlowGraph) {
        // 1. 保护现场
        self.ctx
            .as_mut()
            .unwrap()
            .spill_caller_saved_for_call(&mut self.output);

        let args = call.args();

        // 2. 传递参数
        for (i, &arg) in args.iter().enumerate() {
            // 【核心修复】使用 resolve_operand 统一处理
            // - 立即数 -> li
            // - 普通变量 -> lw
            // - 局部数组(Alloc) -> addi sp (这正是你缺失的逻辑！)
            // - 全局变量 -> la
            let val_reg = self.resolve_operand(arg, dfg);

            if i < 8 {
                // === 寄存器传参 (a0 - a7) ===
                let target_reg = format!("a{}", i);
                if val_reg != target_reg {
                    writeln!(self.output, "  mv    {}, {}", target_reg, val_reg).unwrap();
                }

                // 【关键】锁定已填充的参数寄存器！
                // 防止处理下一个参数时，resolve_operand 挑选了 target_reg 作为临时寄存器
                self.ctx.as_mut().unwrap().lock_reg(&target_reg);
            } else {
                // === 栈传参 (Arg 9+) ===
                // 存入 Outgoing Args 区域 (位于当前 SP 的最底部)
                let offset = (i as i32 - 8) * 4;

                // 检查立即数范围
                if offset >= -2048 && offset <= 2047 {
                    writeln!(self.output, "  sw    {}, {}(sp)", val_reg, offset).unwrap();
                } else {
                    let tmp = self
                        .ctx
                        .as_mut()
                        .unwrap()
                        .get_temp_reg_for_address_calc(&mut self.output);
                    writeln!(self.output, "  li    {}, {}", tmp, offset).unwrap();
                    writeln!(self.output, "  add   {}, sp, {}", tmp, tmp).unwrap();
                    writeln!(self.output, "  sw    {}, 0({})", val_reg, tmp).unwrap();
                }
            }
        }

        // 3. 生成 Call 指令
        let callee_name = &self.program.func(call.callee()).name()[1..];
        writeln!(self.output, "  call  {}", callee_name).unwrap();

        // 4. 【关键】调用结束后，解锁所有寄存器 (a0-a7 解放)
        self.ctx.as_mut().unwrap().unlock_all();

        // 5. 处理返回值
        if !dfg.value(inst_val).ty().is_unit() {
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
