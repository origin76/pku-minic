use koopa::ir::{builder::*, Type, TypeKind, Value};

use crate::{
    analysis::scope::Symbol,
    ir_generation::{
        constval::evaluate_const_exp,
        decl::build_array_type,
        flatten::flatten_init_val,
        genfunc::FunctionGenerator,
    },
    parser::ast::{Exp, InitVal, LVal, VarDecl},
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
                .map(|d| evaluate_const_exp(&self.symbol_table, d) as usize)
                .collect();

            // 2. 分配内存 (alloc)
            let ty = build_array_type(&dims);
            let alloc_ptr = self.alloc_variable(ty.clone());

            // 3. 注册符号表
            let ptr_type = self.func.dfg().value(alloc_ptr).ty().clone();
            self.symbol_table
                .insert_var(def.ident.clone(), alloc_ptr, ptr_type);

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
        let (ptr, _) = self.generate_lval_address(lval);
        let store = self.func.dfg_mut().new_value().store(val, ptr);
        self.add_inst(store);
    }

    pub fn generate_lval_address(&mut self, lval: &LVal) -> (Value, Type) {
        // 1. 查表
        let (mut ptr, mut ptr_ty) = match self.symbol_table.lookup(&lval.ident) {
            Some(Symbol::Var(v, t)) => (*v, t.clone()),
            _ => panic!("Undefined variable: {}", lval.ident),
        };

        // 标记索引迭代器
        let mut indices_iter = lval.indices.iter();

        // 2. 【关键】判断基地址类型，决定第一步操作
        // 检查是否是 "函数参数" (Alloc出来的二级指针 **T 或 **[T, N])
        if let koopa::ir::TypeKind::Pointer(base) = ptr_ty.kind() {
            if let koopa::ir::TypeKind::Pointer(_) = base.kind() {
                // === 情况 A: 这是一个函数参数 (指针) ===

                // 1. Load 出真正的基址 (从栈上读出指针值)
                if !lval.indices.is_empty() {
                    let load = self.func.dfg_mut().new_value().load(ptr);
                    self.add_inst(load);

                    ptr = load;
                    ptr_ty = base.clone(); // 类型解开一层: **[i32, 10] -> *[i32, 10]

                    // 2. 处理第一维索引 (必须用 getptr !)
                    if let Some(first_idx_exp) = indices_iter.next() {
                        let idx = self.generate_exp(first_idx_exp);

                        // 【核心修复点】
                        // 指针偏移使用 get_ptr，而不是 get_elem_ptr
                        // *[i32, 10] --getptr--> *[i32, 10] (指向下一行)
                        let next_ptr = self.func.dfg_mut().new_value().get_ptr(ptr, idx);
                        self.add_inst(next_ptr);

                        ptr = next_ptr;
                        // 类型保持不变
                    }
                }
            }
        }

        // 3. 处理剩余维度 (局部数组的所有维度，或参数数组的第二维及以后)
        // 此时 ptr 肯定是指向数组的指针 (*[T, N])
        for index_exp in indices_iter {
            let index_val = self.generate_exp(index_exp);

            match ptr_ty.kind() {
                // 指针指向数组 -> 使用 getelemptr 钻入数组内部
                koopa::ir::TypeKind::Pointer(base)
                    if matches!(base.kind(), TypeKind::Array(_, _)) =>
                {
                    let next_ptr = self.func.dfg_mut().new_value().get_elem_ptr(ptr, index_val);
                    self.add_inst(next_ptr);

                    // 类型推导: *[T, N] -> *T
                    if let koopa::ir::TypeKind::Array(elem_ty, _) = base.kind() {
                        ptr = next_ptr;
                        ptr_ty = koopa::ir::Type::get_pointer(elem_ty.clone());
                    } else {
                        unreachable!()
                    }
                }

                // 错误捕获：如果在这里遇到 Pointer(Integer)，说明索引太多越界了，或者是逻辑错了
                _ => panic!("Cannot index into non-array type. Type: {:?}", ptr_ty),
            }
        }

        (ptr, ptr_ty)
    }
}
