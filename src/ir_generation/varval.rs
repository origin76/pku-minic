use koopa::ir::{builder::*, Type, TypeKind, Value};

use crate::{
    InitVal, analysis::scope::Symbol, build_array_type, evaluate_const_exp, flatten::flatten_init_val, ir_generation::genfunc::FunctionGenerator, parser::ast::{Exp, LVal, VarDecl}
};

impl<'a> FunctionGenerator<'a> {
    // 辅助：在 Entry Block 分配局部变量
    pub fn alloc_variable(&mut self, ty: Type) -> Value {
        // 1. 获取 Entry Block
        let entry_bb = self.func.layout().entry_bb().unwrap();

        // 2. 创建 alloc 指令 (分配 i32)
        let alloc = self.func.dfg_mut().new_value().alloc(ty);

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
            // 1. 计算维度
            let dims: Vec<usize> = def
                .dims
                .iter()
                .map(|d| evaluate_const_exp(&self.symbol_table,d) as usize)
                .collect();

            // 2. 分配内存 (alloc)
            let ty = build_array_type(&dims);
            let alloc_ptr = self.alloc_variable(ty);

            // 3. 注册符号表
            self.symbol_table.insert_var(def.ident.clone(), alloc_ptr);

            // 4. 处理初始化
            if let Some(init) = &def.init {
                if dims.is_empty() {
                    // === 情况 A: 标量初始化 ===
                    // int a = 1;
                    if let InitVal::Exp(e) = init {
                        let val = self.generate_exp(e);
                        let store = self.func.dfg_mut().new_value().store(val, alloc_ptr);
                        self.add_inst(store);
                    }
                } else {
                    // === 情况 B: 数组初始化 ===
                    // int a[2][2] = {1, 2, 3};

                    // 1. 计算总容量
                    let total_len: usize = dims.iter().product();

                    // 2. 展平初始化列表 (得到 Exp 列表)
                    let flattened_exps = flatten_init_val(init);

                    if flattened_exps.len() > total_len {
                        panic!("Too many initializers for array {}", def.ident);
                    }

                    // 3. 遍历并生成 Store
                    // i: 线性索引
                    for i in 0..total_len {
                        // 准备要存的值
                        let val = if i < flattened_exps.len() {
                            // 显式初始化的部分：计算表达式
                            self.generate_exp(&flattened_exps[i])
                        } else {
                            // 未初始化的部分：补 0
                            self.func.dfg_mut().new_value().integer(0)
                        };

                        // 计算目标地址 (getelemptr 链)
                        let elem_ptr = self.get_elem_ptr_by_flat_index(alloc_ptr, i, &dims);

                        // 生成 Store
                        let store = self.func.dfg_mut().new_value().store(val, elem_ptr);
                        self.add_inst(store);
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
                TypeKind::Pointer(base) if matches!(base.kind(), TypeKind::Array(_, _)) => {
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
