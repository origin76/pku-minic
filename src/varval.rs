use koopa::ir::{Type, Value, builder::*};

use crate::{ast::{Exp, LVal, VarDecl}, genfunc::FunctionGenerator, scope::Symbol};

impl<'a> FunctionGenerator<'a> {
    // 辅助：在 Entry Block 分配局部变量
    fn alloc_variable(&mut self) -> Value {
        // 1. 获取 Entry Block
        let entry_bb = self.func.layout().entry_bb().unwrap();
        
        // 2. 创建 alloc 指令 (分配 i32)
        let alloc = self.func.dfg_mut().new_value().alloc(Type::get_i32());
        
        // 3. 将 alloc 指令插入到 Entry Block 的【最前面】
        // 这样可以确保它在任何使用之前都有效
        let _ = self.func.layout_mut().bb_mut(entry_bb).insts_mut().push_key_front(alloc);
        
        alloc
    }

    pub fn generate_var_decl(&mut self, decl: &VarDecl) {
        for def in &decl.defs {
            // 1. 在栈上为变量分配空间
            // 生成指令: %ptr = alloc i32
            let ptr = self.alloc_variable();

            // 2. 记录到符号表
            // 以后用到这个变量名时，就知道它对应的地址是 ptr
            self.symbol_table.insert_var(def.ident.clone(), ptr);

            // 3. 处理初始化 (如果有)
            if let Some(init_exp) = &def.init {
                // 计算初始化表达式的值 (int a = b + 1)
                // 注意：这里调用 generate_exp (运行时计算)，而不是 const 的 evaluate
                let init_val = self.generate_exp(init_exp);

                // 生成 store 指令: store %val, %ptr
                let store = self.func.dfg_mut().new_value().store(init_val, ptr);
                self.add_inst(store);
            }
        }
    }

    pub fn generate_assign(&mut self, lval: &LVal, exp: &Exp) {
        // Step 1: 计算右值 (RHS)
        // 例如 a = 1 + 2; 这里算出 3
        let val = self.generate_exp(exp);

        // Step 2: 查找左值 (LHS) 对应的符号
        match self.symbol_table.lookup(&lval.ident) {
            Some(Symbol::Var(ptr)) => {
                // Step 3: 生成 Store 指令
                // *ptr 是之前 alloc 指令返回的内存地址 (pointer to i32)
                // store val, ptr
                let store = self.func.dfg_mut().new_value().store(val, *ptr);
                self.add_inst(store);
            }
            Some(Symbol::Const(_)) => {
                // 语义检查：常量不可赋值
                panic!("Error: Cannot assign to constant '{}'", lval.ident);
            }
            None => {
                // 语义检查：变量未定义
                panic!("Error: Undefined variable '{}'", lval.ident);
            }
        }
    }
}