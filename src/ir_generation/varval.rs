use koopa::ir::{builder::*, Type, TypeKind, Value};

use crate::{
    InitVal, analysis::scope::Symbol, evaluate_const_exp, ir_generation::genfunc::FunctionGenerator, parser::ast::{Exp, LVal, VarDecl}
};

impl<'a> FunctionGenerator<'a> {
    // 辅助：在 Entry Block 分配局部变量
    pub fn alloc_variable(&mut self) -> Value {
        // 1. 获取 Entry Block
        let entry_bb = self.func.layout().entry_bb().unwrap();

        // 2. 创建 alloc 指令 (分配 i32)
        let alloc = self.func.dfg_mut().new_value().alloc(Type::get_i32());

        // 3. 将 alloc 指令插入到 Entry Block 的【最前面】
        // 这样可以确保它在任何使用之前都有效
        let _ = self
            .func
            .layout_mut()
            .bb_mut(entry_bb)
            .insts_mut()
            .push_key_front(alloc);

        alloc
    }

    pub fn generate_var_decl(&mut self, decl: &VarDecl) {  
        for def in &decl.defs {
            // 1. 计算维度 (const exp)
            let mut dims = Vec::new();
            for d in &def.dims {
                dims.push(evaluate_const_exp(&mut self.symbol_table,d) as usize);
            }

            // 2. 构造类型 & Alloc
            let ty = if dims.is_empty() {
                koopa::ir::Type::get_i32()
            } else {
                // 目前只处理 1D: [i32, N]
                koopa::ir::Type::get_array(koopa::ir::Type::get_i32(), dims[0])
            };
            
            let alloc = self.func.dfg_mut().new_value().alloc(ty);

            // Move alloc to entry block
            let entry_bb = self.func.layout().entry_bb().unwrap();
            let _ = self
                .func
                .layout_mut()
                .bb_mut(entry_bb)
                .insts_mut()
                .push_key_front(alloc);

            // 注册到符号表
            self.symbol_table.insert_var(def.ident.clone(), alloc);

            // 3. 处理初始化
            if let Some(init) = &def.init {
                if dims.is_empty() {
                    // 标量初始化
                    if let InitVal::Exp(e) = init {
                        let val = self.generate_exp(e);
                        let store = self.func.dfg_mut().new_value().store(val, alloc);
                        self.add_inst(store);
                    }
                } else {
                    // 数组初始化
                    // 将 InitVal 展平或遍历
                    if let InitVal::List(list) = init {
                        for (i, item) in list.iter().enumerate() {
                            if i >= dims[0] { break; } // 防止越界
                            
                            if let InitVal::Exp(e) = item {
                                // 算出要存的值
                                let val = self.generate_exp(e);
                                
                                // 算出目标地址: getelemptr alloc, i
                                let idx = self.func.dfg_mut().new_value().integer(i as i32);
                                let elem_ptr = self.func.dfg_mut().new_value().get_elem_ptr(alloc, idx);
                                self.add_inst(elem_ptr);
                                
                                // store
                                let store = self.func.dfg_mut().new_value().store(val, elem_ptr);
                                self.add_inst(store);
                            }
                        }
                        // 剩余未初始化的元素默认为 0 (SysY 规范)
                        // Simple implementation: continue loop until dims[0], fill with 0
                        for i in list.len()..dims[0] {
                            let val = self.func.dfg_mut().new_value().integer(0);
                            let idx = self.func.dfg_mut().new_value().integer(i as i32);
                            let elem_ptr = self.func.dfg_mut().new_value().get_elem_ptr(alloc, idx);
                            self.add_inst(elem_ptr);
                            let store = self.func.dfg_mut().new_value().store(val, elem_ptr);
                            self.add_inst(store);
                        }
                    }
                }
            }
        }
    }

    pub fn generate_assign(&mut self, lval: &LVal, exp: &Exp) {
        let val = self.generate_exp(exp);
        // 使用新函数计算地址
        let ptr = self.generate_lval_address(lval);
        let store = self.func.dfg_mut().new_value().store(val, ptr);
        self.add_inst(store);
    }

    pub fn generate_lval_address(&mut self, lval: &LVal) -> Value {
        // 1. 查找基地址
        let mut ptr = match self.symbol_table.lookup(&lval.ident) {
            Some(Symbol::Var(p)) => *p,
            // 兼容之前只存 const 值的情况，如果 const 数组被作为右值使用，逻辑不同
            // 但 LVal 出现在等号左边时，必须是 Var
            _ => panic!("Undefined variable: {}", lval.ident),
        };

        // 2. 逐层应用索引 (getelemptr / getptr)
        for (_, index_exp) in lval.indices.iter().enumerate() {
            let index_val = self.generate_exp(index_exp);

            // 检查当前 ptr 的类型
            let ptr_ty = self.func.dfg().value(ptr).ty();

            match ptr_ty.kind() {
                // 指针指向数组 (alloc [i32, 10] 返回的是 *[i32, 10])
                // 使用 getelemptr
                TypeKind::Pointer(base)
                    if matches!(base.kind(), TypeKind::Array(_, _)) =>
                {
                    let elem_ptr = self.func.dfg_mut().new_value().get_elem_ptr(ptr, index_val);
                    self.add_inst(elem_ptr);
                    ptr = elem_ptr;
                }

                // 指针指向整数 (函数参数 int* a)
                // 使用 getptr
                TypeKind::Pointer(_) => {
                    let new_ptr = self.func.dfg_mut().new_value().get_ptr(ptr, index_val);
                    self.add_inst(new_ptr);
                    ptr = new_ptr;
                }

                _ => panic!("Cannot index into non-pointer type"),
            }
        }

        ptr
    }
}
