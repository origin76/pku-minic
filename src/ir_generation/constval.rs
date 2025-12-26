use koopa::ir::BinaryOp;

use crate::{
    parser::ast::*, ir_generation::genfunc::FunctionGenerator, analysis::scope::{Symbol, SymbolTable}
};

impl<'a> FunctionGenerator<'a> {
    pub fn generate_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::ConstDecl(c) => self.generate_const_decl(c),
            Decl::VarDecl(v) => self.generate_var_decl(v),
        }
    }

    fn generate_const_decl(&mut self,const_decl: &ConstDecl) {
        for def in &const_decl.defs {
            // 1. 计算初始化表达式的值
            // 注意：SysY 规定 const 初始化必须是编译期常量
            // 这里为了演示，我们假设你有一个辅助函数能计算 Exp -> i32
            // 如果你暂时做不到复杂的折叠，至少先支持 Exp::Number
            let val = evaluate_const_exp(&mut self.symbol_table,&def.init);

            // 2. 存入符号表
            self.symbol_table.insert_const(def.ident.clone(), val);
        }
    }
}

// 辅助：计算编译期常量值 (简单版)
pub fn evaluate_const_exp(sym: &mut SymbolTable, exp: &Exp) -> i32 {
    match exp {
        // 1. 基础表达式：数字、括号、查表
        Exp::PrimaryExp(p) => match &**p {
            PrimaryExp::Number(n) => *n,
            PrimaryExp::Parentheses(e) => evaluate_const_exp(sym, e),
            // 查表：如果引用了其他 const 变量
            PrimaryExp::LVal(lval) => {
                if let Some(Symbol::Const(v)) = sym.lookup(&lval.ident) {
                    *v
                } else {
                    panic!(
                        "Compile Error: '{}' is not a constant or not defined.",
                        lval.ident
                    );
                }
            }
        },

        // 2. 一元运算
        Exp::UnaryExp(op, child) => {
            let val = evaluate_const_exp(sym, child);
            match op {
                UnaryOp::Plus => val,   // +x
                UnaryOp::Minus => -val, // -x
                UnaryOp::Not => {
                    // !x
                    // C语义: 0变1, 非0变0
                    if val == 0 {
                        1
                    } else {
                        0
                    }
                }
            }
        }

        // 3. 二元运算 (算术、关系、逻辑)
        Exp::BinaryExp(lhs, op, rhs) => {
            let l = evaluate_const_exp(sym, lhs);
            let r = evaluate_const_exp(sym, rhs);

            match op {
                // === 算术运算 ===
                BinaryOp::Add => l.wrapping_add(r), // 使用 wrapping 防止编译期溢出 panic
                BinaryOp::Sub => l.wrapping_sub(r),
                BinaryOp::Mul => l.wrapping_mul(r),
                BinaryOp::Div => {
                    if r == 0 {
                        panic!("Compile Error: Division by zero in constant expression");
                    }
                    l / r
                }
                BinaryOp::Mod => {
                    if r == 0 {
                        panic!("Compile Error: Modulo by zero in constant expression");
                    }
                    l % r
                }

                // === 关系运算 (结果为 1 或 0) ===
                // Rust 的 bool 可以直接用 `as i32` 转为 1 或 0
                BinaryOp::Lt => (l < r) as i32,
                BinaryOp::Gt => (l > r) as i32,
                BinaryOp::Le => (l <= r) as i32,
                BinaryOp::Ge => (l >= r) as i32,
                BinaryOp::Eq => (l == r) as i32,
                BinaryOp::NotEq => (l != r) as i32,

                // === 逻辑运算 (C 语义) ===
                // 只要不为 0 即为真，结果返回 1 或 0
                BinaryOp::And => {
                    let l_bool = l != 0;
                    let r_bool = r != 0;
                    (l_bool && r_bool) as i32
                }
                BinaryOp::Or => {
                    let l_bool = l != 0;
                    let r_bool = r != 0;
                    (l_bool || r_bool) as i32
                }

                BinaryOp::Xor => l ^ r,
                // 提示：如果你还支持 And(&) 和 Or(|) 的位运算，也可以加上
                // BinaryOp::BitAnd => l & r,
                // BinaryOp::BitOr => l | r,

                // === 移位运算 ===
                BinaryOp::Shl => {
                    // 逻辑左移 (<<)
                    // 使用 wrapping_shl 避免位移量 >= 32 时 panic
                    l.wrapping_shl(r as u32)
                }

                BinaryOp::Sar => {
                    // 算术右移 (>> 对于有符号整数)
                    // 对应 C 语言中的 `>>` (int)
                    // 保留符号位 (负数移位后还是负数)
                    l.wrapping_shr(r as u32)
                }

                BinaryOp::Shr => {
                    // 逻辑右移 (>>> 在 Java 中，或 >> 对于 unsigned)
                    // 高位补 0
                    // 做法：先转成 u32 (无符号) 进行移位，再转回 i32
                    (l as u32).wrapping_shr(r as u32) as i32
                }
            }
        }
        // function isnot const exp
        _ => panic!("not const exp"),
    }
}
