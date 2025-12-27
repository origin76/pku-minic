use koopa::ir::{
    BasicBlock, BinaryOp, FunctionData, Type, TypeKind, Value, ValueKind, builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}
};

use crate::{
    analysis::scope::{Symbol, SymbolTable},
    parser::ast::{Block, BlockItem, Exp, PrimaryExp, Stmt, UnaryOp},
};

pub struct FunctionGenerator<'a> {
    // 持有 FunctionData 的可变引用
    pub func: &'a mut FunctionData,
    // 【关键】追踪当前正在写入的基本块
    current_bb: Option<BasicBlock>,
    pub symbol_table: SymbolTable,
    name_counter: usize,
    loop_stack: Vec<LoopInfo>,
}

impl<'a> FunctionGenerator<'a> {
    pub fn new(func: &'a mut FunctionData, global_symbols: SymbolTable) -> Self {
        Self {
            func,
            current_bb: None,
            // 使用传入的符号表 (其中已经包含了所有函数的声明)
            symbol_table: global_symbols,
            name_counter: 1,
            loop_stack: Vec::new(),
        }
    }

    // 获取当前基本块，如果为 None 则获取 entry (通常用于刚开始时)
    fn get_cur_bb(&self) -> BasicBlock {
        self.current_bb.unwrap_or_else(|| {
            self.func
                .layout()
                .entry_bb()
                .expect("Entry block should exist")
        })
    }

    fn new_bb(&mut self, prefix: &str) -> BasicBlock {
        // 格式化名字：例如 "%then_1", "%end_2"
        // 注意：Koopa IR 的名字通常以 % 开头
        let name = format!("%{}_{}", prefix, self.name_counter);

        // 计数器自增，保证下一个名字不重复
        self.name_counter += 1;

        // 在 DFG 中创建基本块
        self.func.dfg_mut().new_bb().basic_block(Some(name))
    }

    // 统一插入指令
    pub fn add_inst(&mut self, inst: Value) {
        let bb = self.get_cur_bb();
        let _ = self
            .func
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst);
    }

    // 切换当前基本块 (用于 br/jump 之后)
    pub fn set_cur_bb(&mut self, bb: BasicBlock) {
        self.current_bb = Some(bb);
    }
}

struct LoopInfo {
    entry_bb: BasicBlock, // continue 跳转的目标 (重新判断条件)
    end_bb: BasicBlock,   // break 跳转的目标 (跳出循环)
}

impl<'a> FunctionGenerator<'a> {
    // 以前是 FuncDef::generate_exp，现在移到这里
    // 注意：不再需要传入 func 参数，因为 self.func 就在结构体里
    pub fn generate_exp(&mut self, exp: &Exp) -> Value {
        match exp {
            Exp::BinaryExp(lhs, op, rhs) => self.generate_binary_exp(lhs, op, rhs),
            Exp::UnaryExp(op, child) => self.generate_unary_exp(op, child),
            Exp::PrimaryExp(p) => self.generate_primary_exp(p),
            Exp::FuncCall(name, args) => {
                // 1. 从符号表中查找函数
                // lookup 应该能查到全局作用域里的函数
                let callee = match self.symbol_table.lookup(name) {
                    Some(Symbol::Func(func_handle)) => *func_handle,
                    Some(_) => panic!("Error: '{}' is not a function", name),
                    None => panic!("Error: Function '{}' is undefined", name),
                };

                // 2. 递归计算所有实参的 Value
                let mut arg_values = Vec::new();
                for arg in args {
                    let val = self.generate_exp(arg);
                    arg_values.push(val);
                }

                // 3. 生成 Call 指令
                // call 指令会返回一个 Value (即使是 void 函数，Koopa 也会返回一个 unit 类型的 Value)
                let call_inst = self.func.dfg_mut().new_value().call(callee, arg_values);

                // 4. 将指令加入当前基本块
                self.add_inst(call_inst);

                // 5. 返回 call 指令产生的 Value
                call_inst
            }
        }
    }

    pub fn generate_binary_exp(&mut self, lhs: &Exp, op: &BinaryOp, rhs: &Exp) -> Value {
        match op {
            // === 逻辑运算 (需要短路求值 + 保证结果为 0/1) ===
            BinaryOp::And => self.generate_logical_and(lhs, rhs),
            BinaryOp::Or => self.generate_logical_or(lhs, rhs),
            _ => {
                // 1. 递归计算左右操作数
                let lhs_val = self.generate_exp(lhs);
                let rhs_val = self.generate_exp(rhs);

                // 3. 构建指令并插入当前基本块
                let value = self
                    .func
                    .dfg_mut()
                    .new_value()
                    .binary(*op, lhs_val, rhs_val);
                self.add_inst(value);

                value
            }
        }
    }

    fn generate_unary_exp(&mut self, op: &UnaryOp, child: &Exp) -> Value {
        // 1. 递归计算子表达式
        let val = self.generate_exp(child);

        // 2. 根据操作符生成对应逻辑
        match op {
            UnaryOp::Plus => val, // +x 等于 x，直接返回
            UnaryOp::Minus => {
                // -x 等价于 0 - x
                let zero = self.func.dfg_mut().new_value().integer(0);
                let res =
                    self.func
                        .dfg_mut()
                        .new_value()
                        .binary(koopa::ir::BinaryOp::Sub, zero, val);

                self.add_inst(res);
                res
            }
            UnaryOp::Not => {
                // !x 等价于 x == 0
                let zero = self.func.dfg_mut().new_value().integer(0);
                let res =
                    self.func
                        .dfg_mut()
                        .new_value()
                        .binary(koopa::ir::BinaryOp::Eq, val, zero);

                self.add_inst(res);
                res
            }
        }
    }

    // 处理基础表达式
    fn generate_primary_exp(&mut self, primary: &PrimaryExp) -> Value {
        match primary {
            // 递归终点：生成整数常量
            PrimaryExp::Number(n) => self.func.dfg_mut().new_value().integer(*n),

            // 括号：(Exp) -> 重新调用 generate_exp
            PrimaryExp::Parentheses(exp) => self.generate_exp(exp),

            // 1. 查表
            PrimaryExp::LVal(lval) => {
                // 1. 尝试查找常量 (如果是 const int a = 5)
                if lval.indices.is_empty() {
                    if let Some(Symbol::Const(v)) = self.symbol_table.lookup(&lval.ident) {
                        return self.func.dfg_mut().new_value().integer(*v);
                    }
                }

                // 2. 计算地址并 Load
                // 注意：如果是数组名作为参数传递 (return a)，这里逻辑不同 (需要 getelemptr 0)
                // 但 Lv9.1 是一维数组基础，先假设都是取元素
                let ptr = self.generate_lval_address(lval);

                // 检查 ptr 指向的是不是 i32
                // 如果指向的是数组(比如 a 是 [i32, 10]*，且没有索引)，则需要降级为指针 (getelemptr 0)
                // SysY 中数组名做右值表示首地址
                let ptr_ty = self.func.dfg().value(ptr).ty();
                if let TypeKind::Pointer(base) = ptr_ty.kind() {
                    if matches!(base.kind(),TypeKind::Array(_,_ )) {
                        // 数组退化为指针: getelemptr ptr, 0
                        let zero = self.func.dfg_mut().new_value().integer(0);
                        let decay = self.func.dfg_mut().new_value().get_elem_ptr(ptr, zero);
                        self.add_inst(decay);
                        return decay;
                    }
                }

                // 普通变量读取
                let load = self.func.dfg_mut().new_value().load(ptr);
                self.add_inst(load);
                load
            }
        }
    }

    fn generate_logical_and(&mut self, lhs: &Exp, rhs: &Exp) -> Value {
        // 1. 在 Entry Block 分配一个临时栈变量 result，用于存结果
        //    C 语言逻辑: result 初始化为 0 (False)
        let entry_bb = self.get_cur_bb();
        let result_ptr = self.func.dfg_mut().new_value().alloc(Type::get_i32());
        let _ = self
            .func
            .layout_mut()
            .bb_mut(entry_bb)
            .insts_mut()
            .push_key_front(result_ptr); // alloc 最好放在入口块最前面

        // 初始化 result = 0
        let zero = self.func.dfg_mut().new_value().integer(0);
        let init_store = self.func.dfg_mut().new_value().store(zero, result_ptr);
        self.set_cur_bb(entry_bb);
        self.add_inst(init_store);

        // 2. 计算 LHS
        let lhs_val = self.generate_exp(lhs);
        // 判断 LHS != 0
        let lhs_ne_zero = self
            .func
            .dfg_mut()
            .new_value()
            .binary(BinaryOp::NotEq, lhs_val, zero);
        self.add_inst(lhs_ne_zero);

        // 3. 创建基本块: true_bb (计算 RHS), end_bb (结束)
        let true_bb = self.new_bb("true");
        let end_bb = self.new_bb("end");

        // 4. 生成分支指令: if (lhs) goto true_bb; else goto end_bb;
        let br = self
            .func
            .dfg_mut()
            .new_value()
            .branch(lhs_ne_zero, true_bb, end_bb);
        self.add_inst(br);

        // === 开始生成 true_bb ===
        let _ = self.func.layout_mut().bbs_mut().push_key_back(true_bb);
        self.set_cur_bb(true_bb);

        // 5. 计算 RHS
        let rhs_val = self.generate_exp(rhs);
        // 判断 RHS != 0
        let rhs_ne_zero =
            self.func
                .dfg_mut()
                .new_value()
                .binary(koopa::ir::BinaryOp::NotEq, rhs_val, zero);
        self.add_inst(rhs_ne_zero);

        // 6. 将 RHS 的结果 (0或1) 存入 result
        // 如果能走到这里，说明 LHS 是真，那么整个表达式的值就取决于 RHS 是否为真
        let store_true = self
            .func
            .dfg_mut()
            .new_value()
            .store(rhs_ne_zero, result_ptr);
        self.add_inst(store_true);

        // 跳转到 end_bb
        let jump_end = self.func.dfg_mut().new_value().jump(end_bb);
        self.add_inst(jump_end);

        // === 开始生成 end_bb ===
        let _ = self.func.layout_mut().bbs_mut().push_key_back(end_bb);
        self.set_cur_bb(end_bb);

        // 7. 读取最终结果
        let load_res = self.func.dfg_mut().new_value().load(result_ptr);
        self.add_inst(load_res);

        load_res
    }

    fn generate_logical_or(&mut self, lhs: &Exp, rhs: &Exp) -> Value {
        // 1. 分配结果变量，初始化为 1 (True)
        //    如果 LHS 为真，短路跳到 end，结果保持 1
        let entry_bb = self.get_cur_bb();
        let result_ptr = self.func.dfg_mut().new_value().alloc(Type::get_i32());
        let _ = self
            .func
            .layout_mut()
            .bb_mut(entry_bb)
            .insts_mut()
            .push_key_front(result_ptr);

        let one = self.func.dfg_mut().new_value().integer(1);
        let zero = self.func.dfg_mut().new_value().integer(0);
        let init_store = self.func.dfg_mut().new_value().store(one, result_ptr);
        self.set_cur_bb(entry_bb);
        self.add_inst(init_store);

        // 2. 计算 LHS
        let lhs_val = self.generate_exp(lhs);
        let lhs_ne_zero = self
            .func
            .dfg_mut()
            .new_value()
            .binary(BinaryOp::NotEq, lhs_val, zero);
        self.add_inst(lhs_ne_zero);

        // 3. 基本块: false_bb (LHS为假，需要算RHS), end_bb
        let false_bb = self.new_bb("false");
        let end_bb = self.new_bb("end");

        // 4. 分支: if (lhs) goto end_bb; else goto false_bb;
        // 注意这里: lhs为真直接跳 end (短路)
        let br = self
            .func
            .dfg_mut()
            .new_value()
            .branch(lhs_ne_zero, end_bb, false_bb);
        self.add_inst(br);

        // === false_bb ===
        let _ = self.func.layout_mut().bbs_mut().push_key_back(false_bb);
        self.set_cur_bb(false_bb);

        // 5. 计算 RHS
        let rhs_val = self.generate_exp(rhs);
        let rhs_ne_zero = self
            .func
            .dfg_mut()
            .new_value()
            .binary(BinaryOp::NotEq, rhs_val, zero);
        self.add_inst(rhs_ne_zero);

        // 6. 存入结果
        // 此时 LHS 为假，整个表达式的值取决于 RHS
        let store_false = self
            .func
            .dfg_mut()
            .new_value()
            .store(rhs_ne_zero, result_ptr);
        self.add_inst(store_false);

        let jump_end = self.func.dfg_mut().new_value().jump(end_bb);
        self.add_inst(jump_end);

        // === end_bb ===
        let _ = self.func.layout_mut().bbs_mut().push_key_back(end_bb);
        self.set_cur_bb(end_bb);

        let load_res = self.func.dfg_mut().new_value().load(result_ptr);
        self.add_inst(load_res);

        load_res
    }

    pub fn generate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign(lval, exp) => {
                self.generate_assign(lval, exp);
            }

            Stmt::Return(exp_opt) => {
                let ret_val = if let Some(exp) = exp_opt {
                    // 有返回值: return 0;
                    Some(self.generate_exp(exp))
                } else {
                    // 无返回值: return; (对应 void)
                    None
                };

                // 生成 ret 指令
                let ret_inst = self.func.dfg_mut().new_value().ret(ret_val);
                self.add_inst(ret_inst);
            }

            Stmt::Block(block) => {
                // 直接递归调用 generate_block
                // generate_block 内部会处理 symbol_table.enter_scope() / exit_scope()
                self.generate_block(block);
            }

            Stmt::Exp(exp_opt) => {
                if let Some(exp) = exp_opt {
                    // 计算表达式
                    // 生成的指令会自动插入到 basic block 中
                    // 虽然有了 result Value，但我们不需要用它，直接丢弃即可
                    self.generate_exp(exp);
                }
                // 如果是 None (即空语句 ";")，什么都不做
            }

            Stmt::If(cond, then_stmt, else_stmt) => {
                self.generate_if(cond, then_stmt, else_stmt.as_deref());
            }

            Stmt::While(cond, body) => {
                self.generate_while(cond, body);
            }

            Stmt::Break => {
                // 1. 获取当前最近的循环信息
                if let Some(loop_info) = self.loop_stack.last() {
                    // 2. 生成跳转到 end_bb 的指令
                    let jump = self.func.dfg_mut().new_value().jump(loop_info.end_bb);
                    self.add_inst(jump);

                    // break 之后的代码是不可达的，不需要继续生成
                    // (generate_block 里的 is_cur_bb_terminated 会处理截断)
                } else {
                    // 语义错误：break 不在循环内
                    // 实际编译器中应报错，这里 panic 仅作演示
                    panic!("Semantic Error: 'break' statement not in loop");
                }
            }

            Stmt::Continue => {
                // 1. 获取当前最近的循环信息
                if let Some(loop_info) = self.loop_stack.last() {
                    // 2. 生成跳转到 entry_bb (重新判断条件) 的指令
                    let jump = self.func.dfg_mut().new_value().jump(loop_info.entry_bb);
                    self.add_inst(jump);
                } else {
                    panic!("Semantic Error: 'continue' statement not in loop");
                }
            }
        }
    }

    pub fn generate_block(&mut self, block: &Block) {
        // 1. 【关键】进入新作用域
        // 这保证了 { int a; } 里的 a 不会污染外部，也不会跟外部的 a 冲突
        self.symbol_table.enter_scope();

        for item in &block.items {
            if self.is_cur_bb_terminated() {
                break;
            }
            match item {
                BlockItem::Decl(decl) => self.generate_decl(decl),
                BlockItem::Stmt(stmt) => self.generate_stmt(stmt),
            }
        }

        // 2. 【关键】退出作用域
        self.symbol_table.exit_scope();
    }

    fn generate_if(&mut self, cond: &Exp, then_stmt: &Stmt, else_stmt: Option<&Stmt>) {
        // 1. 创建基本块名字
        let then_bb = self.new_bb("then");
        let end_bb = self.new_bb("end");

        // 只有当存在 else 分支时，才需要 else_bb
        // 如果没有 else，else 跳转的目标就是 end_bb
        let else_bb = if else_stmt.is_some() {
            self.new_bb("else")
        } else {
            end_bb
        };

        // 2. 计算条件 & 生成跳转
        // generate_exp 会生成计算 cond 的指令
        let cond_val = self.generate_exp(cond);

        // 我们需要把 cond_val (可能是 i32) 转换为 bool (i1) 或者直接跟 0 比较
        // 生成: bne cond_val, 0, then_bb, else_bb
        let zero = self.func.dfg_mut().new_value().integer(0);
        let cond_bool =
            self.func
                .dfg_mut()
                .new_value()
                .binary(koopa::ir::BinaryOp::NotEq, cond_val, zero);
        self.add_inst(cond_bool);

        let br = self
            .func
            .dfg_mut()
            .new_value()
            .branch(cond_bool, then_bb, else_bb);
        self.add_inst(br);

        // 3. 生成 Then 块
        let _ = self.func.layout_mut().bbs_mut().push_key_back(then_bb);
        self.set_cur_bb(then_bb);
        self.generate_stmt(then_stmt);
        // Then 块执行完后，跳转到 End
        // 注意：如果 then_stmt 里已经有 return 了，就不需要 jump 了 (需要检查 BasicBlock 是否已经有 terminator)
        if !self.is_cur_bb_terminated() {
            let jump = self.func.dfg_mut().new_value().jump(end_bb);
            self.add_inst(jump);
        }

        // 4. 生成 Else 块 (如果有)
        if let Some(else_body) = else_stmt {
            let _ = self.func.layout_mut().bbs_mut().push_key_back(else_bb);
            self.set_cur_bb(else_bb);
            self.generate_stmt(else_body);
            if !self.is_cur_bb_terminated() {
                let jump = self.func.dfg_mut().new_value().jump(end_bb);
                self.add_inst(jump);
            }
        }

        // 5. 生成 End 块
        let _ = self.func.layout_mut().bbs_mut().push_key_back(end_bb);
        self.set_cur_bb(end_bb);
    }

    // 伪代码参考
    fn generate_while(&mut self, cond: &Exp, body: &Stmt) {
        let entry_bb = self.new_bb("while_entry"); // 计算条件
        let body_bb = self.new_bb("while_body"); // 循环体
        let end_bb = self.new_bb("while_end"); // 结束

        // 1. 从当前块跳入 entry (第一次检查条件)
        // 如果当前块还没终结
        if !self.is_cur_bb_terminated() {
            let jump = self.func.dfg_mut().new_value().jump(entry_bb);
            self.add_inst(jump);
        }

        self.loop_stack.push(LoopInfo { entry_bb, end_bb });

        // 2. 生成 Entry 块 (条件检查)
        let _ = self.func.layout_mut().bbs_mut().push_key_back(entry_bb);
        self.set_cur_bb(entry_bb);

        let cond_val = self.generate_exp(cond);
        let zero = self.func.dfg_mut().new_value().integer(0);
        let cond_bool =
            self.func
                .dfg_mut()
                .new_value()
                .binary(koopa::ir::BinaryOp::NotEq, cond_val, zero);
        self.add_inst(cond_bool);

        // 条件成立去 body，不成立去 end
        let br = self
            .func
            .dfg_mut()
            .new_value()
            .branch(cond_bool, body_bb, end_bb);
        self.add_inst(br);

        // 3. 生成 Body 块
        let _ = self.func.layout_mut().bbs_mut().push_key_back(body_bb);
        self.set_cur_bb(body_bb);

        self.generate_stmt(body);

        // Body 执行完后，必须跳回 Entry 重新检查条件
        if !self.is_cur_bb_terminated() {
            let jump_back = self.func.dfg_mut().new_value().jump(entry_bb);
            self.add_inst(jump_back);
        }

        self.loop_stack.pop();

        // 4. 生成 End 块 (后续代码)
        let _ = self.func.layout_mut().bbs_mut().push_key_back(end_bb);
        self.set_cur_bb(end_bb);
    }

    // 辅助：检查当前 BB 是否已经有终结指令 (ret, jump, br)
    pub fn is_cur_bb_terminated(&mut self) -> bool {
        let bb = self.get_cur_bb();
        let last: Option<Value> = self
            .func
            .layout_mut()
            .bb_mut(bb)
            .insts()
            .back_key()
            .copied();
        if let Some(val) = last {
            matches!(
                self.func.dfg().value(val).kind(),
                ValueKind::Branch(_) | ValueKind::Jump(_) | ValueKind::Return(_)
            )
        } else {
            false
        }
    }
}
