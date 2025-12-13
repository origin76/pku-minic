use koopa::ir::BinaryOp;

use crate::{ast::*, genfunc::FunctionGenerator, scope::Symbol};

impl<'a> FunctionGenerator<'a> {
    pub fn generate_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::ConstDecl(c) => self.generate_const_decl(c),
        }
    }

    fn generate_const_decl(&mut self, const_decl: &ConstDecl) {
        for def in &const_decl.defs {
            // 1. 计算初始化表达式的值
            // 注意：SysY 规定 const 初始化必须是编译期常量
            // 这里为了演示，我们假设你有一个辅助函数能计算 Exp -> i32
            // 如果你暂时做不到复杂的折叠，至少先支持 Exp::Number
            let val = self.evaluate_const_exp(&def.init);
            
            // 2. 存入符号表
            self.symbol_table.insert_const(def.ident.clone(), val);
        }
    }

    // 辅助：计算编译期常量值 (简单版)
    fn evaluate_const_exp(&self, exp: &Exp) -> i32 {
        match exp {
            Exp::PrimaryExp(p) => match &**p {
                PrimaryExp::Number(n) => *n,
                PrimaryExp::Parentheses(e) => self.evaluate_const_exp(e),
                // 如果 const int a = b; b 也是 const，这里需要递归查表
                PrimaryExp::LVal(lval) => {
                    if let Some(Symbol::Const(v)) = self.symbol_table.lookup(&lval.ident) {
                        *v
                    } else {
                        panic!("Const definition uses unknown or non-const variable: {}", lval.ident);
                    }
                }
                _ => panic!("Not a constant expression"),
            },
            Exp::BinaryExp(lhs, op, rhs) => {
                let l = self.evaluate_const_exp(lhs);
                let r = self.evaluate_const_exp(rhs);
                match op {
                    BinaryOp::Add => l + r,
                    BinaryOp::Sub => l - r,
                    BinaryOp::Mul => l * r,
                    BinaryOp::Div => l / r,
                    // ... 其他运算 ...
                    _ => 0,
                }
            }
            _ => panic!("Unsupported constant expression structure"),
        }
    }
}