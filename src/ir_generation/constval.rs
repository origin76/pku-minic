use koopa::ir::{BinaryOp, builder::{LocalInstBuilder, ValueBuilder}};

use crate::{
    analysis::scope::{Symbol, SymbolTable}, build_array_type, ir_generation::genfunc::FunctionGenerator, parser::ast::*
};

pub fn flatten_const_init_val(init: &ConstInitVal, symbol_table: &mut SymbolTable) -> Vec<i32> {
    let mut values = Vec::new();
    match init {
        // 遇到基本数值：计算并加入列表
        ConstInitVal::Exp(exp) => {
            let val = evaluate_const_exp(&symbol_table, exp);
            values.push(val);
        }
        // 遇到列表：递归处理每个子项
        ConstInitVal::List(list) => {
            for item in list {
                values.extend(flatten_const_init_val(item, symbol_table));
            }
        }
    }
    values
}

impl<'a> FunctionGenerator<'a> {
    pub fn generate_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::ConstDecl(c) => self.generate_const_decl(c),
            Decl::VarDecl(v) => self.generate_var_decl(v),
        }
    }

    fn generate_const_decl(&mut self, const_decl: &ConstDecl) {
        for def in &const_decl.defs {
            // 1. 计算维度
            let dims: Vec<usize> = def
                .dims
                .iter()
                .map(|d| evaluate_const_exp(&self.symbol_table,d) as usize)
                .collect();

            if dims.is_empty() {
                // === 情况 A: 标量常量 ===
                if let ConstInitVal::Exp(init_exp) = &def.init {
                    let val = evaluate_const_exp(&self.symbol_table,init_exp);
                    // 仅存入符号表，供常量折叠使用
                    self.symbol_table.insert_const(def.ident.clone(), val);
                }
            } else {
                // === 情况 B: 数组常量 ===
                let total_len: usize = dims.iter().product();

                // 1. 展平并求值数据
                let mut values = flatten_const_init_val(&def.init, &mut self.symbol_table);

                // 2. 补零 (SysY 规范)
                if values.len() > total_len {
                    panic!("Too many initializers for const array {}", def.ident);
                }
                values.resize(total_len, 0);

                // 3. 【关键】双重注册
                // (1) 存入 ConstArray，用于编译期常量折叠 (如 b = a[0] + 1 -> b = 2)
                self.symbol_table
                    .insert_const_array(def.ident.clone(), values.clone());

                // (2) 在栈上 Alloc 并 Store，用于运行时访问 (如 b = a[k])
                // 因为我们无法预知用户是否只用常量索引访问数组，所以必须分配内存
                let ty = build_array_type(&dims);
                let alloc = self.alloc_variable(ty.clone());

                // 注册 alloc 指针，以便 generate_lval_address 能找到它
                self.symbol_table.insert_var(def.ident.clone(), alloc , ty);

                // 生成 Store 指令初始化栈上内存
                for (i, &val) in values.iter().enumerate() {
                    let val_v = self.func.dfg_mut().new_value().integer(val);
                    let ptr = self.get_elem_ptr_by_flat_index(alloc, i, &dims);
                    let store = self.func.dfg_mut().new_value().store(val_v, ptr);
                    self.add_inst(store);
                }
            }
        }
    }
}

// 辅助：计算编译期常量值 (简单版)
pub fn evaluate_const_exp(sym: &SymbolTable, exp: &Exp) -> i32 {
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
