use koopa::ir::{Program, builder::{GlobalInstBuilder, ValueBuilder}};

use crate::{ast::Decl, constval::evaluate_const_exp, scope::SymbolTable};

// 辅助函数：处理全局声明
pub fn process_global_decl(program: &mut Program, symbol_table: &mut SymbolTable, decl: &Decl) {
    match decl {
        Decl::ConstDecl(c) => {
            for def in &c.defs {
                // 计算常量值
                let val = evaluate_const_exp( symbol_table,&def.init,);
                // 存入符号表 (编译期常量传播)
                symbol_table.insert_const(def.ident.clone(), val);
            }
        }
        Decl::VarDecl(v) => {
            for def in &v.defs {
                // 1. 计算初始值 (SysY全局变量如果没有初始化，默认为0)
                let init_val = if let Some(init_exp) = &def.init {
                    evaluate_const_exp(symbol_table,init_exp)
                } else {
                    0
                };

                // 2. 在 Koopa Program 中创建 Global Alloc
                // global @name = alloc i32, init_val
                let name = format!("@{}", def.ident);
                let init_value = program.new_value().integer(init_val);
                let global_alloc = program.new_value().global_alloc(init_value);
                
                // 设置名字 (可选，为了 IR 可读性)
                program.set_value_name(global_alloc, Some(name.clone()));
 
                // 3. 将 Global Value 注册到符号表
                // 注意：这里存的是 global_alloc 返回的 Value (指针)
                // 后续函数里 load @name 时，用的就是这个 Value
                symbol_table.insert_var(def.ident.clone(), global_alloc);
            }
        }
    }
}