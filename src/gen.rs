use crate::ast::*;
use koopa::ir::{builder_traits::*, *};
use koopa::ir::{BinaryOp, FunctionData, Program, Type};

pub trait GenerateProgram {
    type Out;
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()>;
}

impl GenerateProgram for CompUnit {
    type Out = ();
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()> {
        self.func_def.generate(program)
    }
}

impl GenerateProgram for FuncDef {
    type Out = ();
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()> {
        let func_data = FunctionData::new(
            "@".to_string() + &self.ident.clone(),
            vec![],
            Type::get_i32(),
        );
        let func = program.new_func(func_data);
        let main_data = program.func_mut(func);
        let bb = main_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let _ = main_data.layout_mut().bbs_mut().push_key_back(bb);

        match &self.block.stmt {
            Stmt::Return(exp) => {
                let result = self.generate_exp(exp, main_data);
                let ret = main_data.dfg_mut().new_value().ret(Some(result));
                main_data.layout_mut().bb_mut(bb).insts_mut().extend([ret]);
            }
        }
        Ok(())
    }
}

impl FuncDef {

    // 主入口
    fn generate_exp(&self, exp: &Exp, func: &mut FunctionData) -> Value {
        match exp {
            // === 情况 1: 二元运算 (递归生成左右子树) ===
            Exp::BinaryExp(lhs_exp, op, rhs_exp) => {
                self.generate_binary_exp(lhs_exp, op, rhs_exp, func)
            }

            // === 情况 2: 一元运算 (递归生成子树) ===
            Exp::UnaryExp(op, child_exp) => {
                self.generate_unary_exp(op, child_exp, func)
            }

            // === 情况 3: 基础表达式 (数值或括号) ===
            // 这是递归的终点，或者是括号改变优先级的入口
            Exp::PrimaryExp(primary) => {
                self.generate_primary_exp(primary, func)
            }
        }
    }

    // 处理二元表达式
    fn generate_binary_exp(&self, lhs: &Exp, op: &BinaryOp, rhs: &Exp, func: &mut FunctionData) -> Value {
        // 1. 递归计算左右操作数
        let lhs_val = self.generate_exp(lhs, func);
        let rhs_val = self.generate_exp(rhs, func);

        // 3. 构建指令并插入当前基本块
        let value = func.dfg_mut().new_value().binary(*op, lhs_val, rhs_val);
        // 注意：后续实现控制流(if/while)时，需动态获取当前 bb，不要硬编码 entry_bb
        let bb = func.layout().entry_bb().unwrap();
        func.layout_mut().bb_mut(bb).insts_mut().push_key_back(value);

        value
    }

    // 处理一元表达式
    fn generate_unary_exp(&self, op: &UnaryOp, child: &Exp, func: &mut FunctionData) -> Value {
        // 1. 递归计算子表达式
        let val = self.generate_exp(child, func);

        // 2. 根据操作符生成对应逻辑
        match op {
            UnaryOp::Plus => val, // +x 等于 x，直接返回
            UnaryOp::Minus => {
                // -x 等价于 0 - x
                let zero = func.dfg_mut().new_value().integer(0);
                let res = func.dfg_mut().new_value().binary(koopa::ir::BinaryOp::Sub, zero, val);
                
                let bb = func.layout().entry_bb().unwrap();
                func.layout_mut().bb_mut(bb).insts_mut().push_key_back(res);
                res
            }
            UnaryOp::Not => {
                // !x 等价于 x == 0
                let zero = func.dfg_mut().new_value().integer(0);
                let res = func.dfg_mut().new_value().binary(koopa::ir::BinaryOp::Eq, val, zero);
                
                let bb = func.layout().entry_bb().unwrap();
                func.layout_mut().bb_mut(bb).insts_mut().push_key_back(res);
                res
            }
        }
    }

    // 处理基础表达式
    fn generate_primary_exp(&self, primary: &PrimaryExp, func: &mut FunctionData) -> Value {
        match primary {
            // 递归终点：生成整数常量
            PrimaryExp::Number(n) => func.dfg_mut().new_value().integer(*n),
            
            // 括号：(Exp) -> 重新调用 generate_exp
            PrimaryExp::Parentheses(exp) => self.generate_exp(exp, func),
        }
    }

    // === 辅助函数：构建二元指令并插入当前基本块 ===
    // 提取这个逻辑是为了避免代码重复，并且当你以后支持控制流(if/while)时，
    // 只需要修改这里获取 `bb` 的逻辑即可 (不要硬编码 entry_bb)。
    fn build_binary(
        &self, 
        op: BinaryOp, 
        lhs: Value, 
        rhs: Value, 
        func: &mut FunctionData
    ) -> Value {
        // 1. 在 DFG 中创建指令数据
        let value = func.dfg_mut().new_value().binary(op, lhs, rhs);
        
        // 2. 获取当前应当插入的基本块
        // 注意：目前你硬编码了 entry_bb，这对于顺序执行没问题。
        // 等实现 if/while 时，你需要一个变量来追踪 "current_bb"
        let bb = func.layout().entry_bb().unwrap();
        
        // 3. 将指令加入基本块
        func.layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(value);
            
        value
    }
    
}
