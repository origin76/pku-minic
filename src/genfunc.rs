use koopa::ir::{
    builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder},
    BasicBlock, BinaryOp, FunctionData, Type, Value,
};

use crate::{
    ast::{Exp, LVal, PrimaryExp, UnaryOp},
    scope::{Symbol, SymbolTable},
};

pub struct FunctionGenerator<'a> {
    // 持有 FunctionData 的可变引用
    pub func: &'a mut FunctionData,
    // 【关键】追踪当前正在写入的基本块
    current_bb: Option<BasicBlock>,
    pub symbol_table: SymbolTable,
    name_counter: usize,
}

impl<'a> FunctionGenerator<'a> {
    pub fn new(func: &'a mut FunctionData) -> Self {
        Self {
            func,
            current_bb: None,
            symbol_table: SymbolTable::new(),
            name_counter: 1,
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

    // 【核心工具】统一插入指令，自动找对位置
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

impl<'a> FunctionGenerator<'a> {
    // 以前是 FuncDef::generate_exp，现在移到这里
    // 注意：不再需要传入 func 参数，因为 self.func 就在结构体里
    pub fn generate_exp(&mut self, exp: &Exp) -> Value {
        match exp {
            Exp::BinaryExp(lhs, op, rhs) => self.generate_binary_exp(lhs, op, rhs),
            Exp::UnaryExp(op, child) => self.generate_unary_exp(op, child),
            Exp::PrimaryExp(p) => self.generate_primary_exp(p),
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

            PrimaryExp::LVal(lval) => {
                // 1. 查表
                match self.symbol_table.lookup(&lval.ident) {
                    Some(Symbol::Const(val)) => {
                        // 2. 如果是常量，直接把整数值转换成 Koopa 的 Integer Value
                        self.func.dfg_mut().new_value().integer(*val)
                    }
                    Some(Symbol::Var(ptr)) => {
                        let load = self.func.dfg_mut().new_value().load(*ptr);
                        self.add_inst(load);
                        load
                    }
                    None => {
                        panic!("Undefined variable: {}", lval.ident);
                    }
                }
            }
        }
    }

    fn generate_logical_and(&mut self, lhs: &Exp, rhs: &Exp) -> Value {
        // 1. 在 Entry Block 分配一个临时栈变量 result，用于存结果
        //    C 语言逻辑: result 初始化为 0 (False)
        let entry_bb = self.func.layout().entry_bb().unwrap();
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
        let entry_bb = self.func.layout().entry_bb().unwrap();
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
}
