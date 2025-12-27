use koopa::ir::{
    builder::{GlobalInstBuilder, ValueBuilder},
    Program,
};

use crate::{
    ConstInitVal, InitVal, analysis::scope::SymbolTable, flatten_const_init_val, ir_generation::constval::evaluate_const_exp, parser::ast::Decl
};

use koopa::ir::{Type, Value};

/// 辅助：递归展平 InitVal (处理 {1, {2, 3}} 这种情况)
fn flatten_global_init_val(init: &InitVal, symbol_table: &SymbolTable) -> Vec<i32> {
    let mut values = Vec::new();
    match init {
        InitVal::Exp(exp) => {
            // 全局变量初始化必须是常量表达式
            values.push(evaluate_const_exp(symbol_table, exp));
        }
        InitVal::List(list) => {
            for item in list {
                values.extend(flatten_global_init_val(item, symbol_table));
            }
        }
    }
    values
}

/// 辅助：递归构建 Koopa 的 Aggregate Value 或 Integer Value
/// data: 扁平化的数据 (例如 [1, 2, 3, 4])
/// dims: 维度信息 (例如 [2, 2] 表示 int a[2][2])
fn build_global_init_value(program: &mut Program, data: &[i32], dims: &[usize]) -> Value {
    // Base Case: 标量 (维度为空)
    if dims.is_empty() {
        return program.new_value().integer(data[0]);
    }

    // Recursive Case: 数组
    let current_dim = dims[0]; // 当前维度的长度
    let remaining_dims = &dims[1..]; // 剩余维度

    // 计算子元素在扁平数据中的跨度 (Stride)
    // 例如 a[2][3]，剩余维度是 [3]，跨度就是 3
    let stride = remaining_dims.iter().product::<usize>().max(1);

    let mut elems = Vec::new();
    for i in 0..current_dim {
        // 切片获取子元素的数据
        let start = i * stride;
        let end = start + stride;
        let sub_data = &data[start..end];

        // 递归构建子元素
        let elem = build_global_init_value(program, sub_data, remaining_dims);
        elems.push(elem);
    }

    // 构造当前层的 Aggregate Value
    // 1. 先把 Aggregate 的 Value 生成出来
    let agg_value = program.new_value().aggregate(elems);

    // 注意：Koopa 的 aggregate 不需要显式传 Type，它会根据 elems 推导
    // 但 Global Alloc 需要知道分配的类型，我们稍后在外部处理
    agg_value
}

/// 辅助：根据维度生成 Koopa 数组类型
fn build_array_type(dims: &[usize]) -> Type {
    let mut ty = Type::get_i32();
    // 从内向外包裹: [2, 3] -> Array(Array(i32, 3), 2)
    for &dim in dims.iter().rev() {
        ty = Type::get_array(ty, dim);
    }
    ty
}

pub fn process_global_decl(program: &mut Program, symbol_table: &mut SymbolTable, decl: &Decl) {
    match decl {
        // ==========================================
        // 处理全局常量 (ConstDecl)
        // ==========================================
        Decl::ConstDecl(c) => {
            for def in &c.defs {
                // 1. 计算维度
                let dims: Vec<usize> = def
                    .dims
                    .iter()
                    .map(|d| evaluate_const_exp(symbol_table, d) as usize)
                    .collect();

                // 2. 根据是否是数组分流
                if dims.is_empty() {
                    // --- 标量 ---
                    // 对于 ConstDecl，init 肯定是 ConstInitVal
                    // 我们需要把它转回 InitVal 的结构或者单独写一个 flatten_const
                    // 这里假设你有办法直接拿到值，或者复用 flatten 逻辑
                    // 简单起见，假设 evaluate_const_exp 能处理 ConstInitVal::Exp
                    if let ConstInitVal::Exp(e) = &def.init {
                        let val = evaluate_const_exp(symbol_table, e);
                        symbol_table.insert_const(def.ident.clone(), val);
                    }
                } else {
                    // --- 数组 ---
                    // 1. 计算总大小
                    let total_len: usize = dims.iter().product();

                    // 2. 展平数据 (你需要把 flatten_global_init_val 改一下适配 ConstInitVal，或者写个类似的)
                    // 这里假设你已经有了 flatten_const_init -> Vec<i32>
                    let mut values = flatten_const_init_val(&def.init, symbol_table);

                    // 3. 补零
                    values.resize(total_len, 0);

                    // 4. 存入符号表 (供编译期折叠使用)
                    symbol_table.insert_const_array(def.ident.clone(), values.clone());

                    // 【可选但推荐】
                    // 即使是 const 数组，如果后面有取地址操作 (load getelemptr)，也需要在 IR 里分配内存
                    // 如果你确信只做常量折叠，可以不生成 global_alloc。
                    // 但通常为了兼容性，建议也生成 global_alloc，逻辑同下方的 VarDecl。

                    // 生成 Global Alloc 逻辑同下...
                    let ty = build_array_type(&dims);
                    let init = build_global_init_value(program, &values, &dims);
                    let global = program.new_value().global_alloc(init);
                    program.set_value_name(global, Some(format!("@{}", def.ident)));
                    // 注册为 Var 以便也能当做地址访问
                    symbol_table.insert_var(def.ident.clone(), global);
                }
            }
        }

        // ==========================================
        // 处理全局变量 (VarDecl)
        // ==========================================
        Decl::VarDecl(v) => {
            for def in &v.defs {
                // 1. 计算维度
                let dims: Vec<usize> = def
                    .dims
                    .iter()
                    .map(|d| evaluate_const_exp( symbol_table,d) as usize)
                    .collect();

                // 2. 构造 Koopa 类型
                let ty = build_array_type(&dims);

                // 3. 准备初始化数据
                let init_value = if let Some(init) = &def.init {
                    // 有初始化列表
                    if dims.is_empty() {
                        // --- 标量初始化 ---
                        if let InitVal::Exp(e) = init {
                            let val = evaluate_const_exp(symbol_table,e);
                            program.new_value().integer(val)
                        } else {
                            panic!("Scalar initialized with list");
                        }
                    } else {
                        // --- 数组初始化 (Aggregate) ---
                        // 1. 计算总长度
                        let total_len: usize = dims.iter().product();

                        // 2. 展平数据
                        let mut values = flatten_global_init_val(init, symbol_table);

                        // 3. 补零
                        if values.len() > total_len {
                            panic!("Too many initializers for {}", def.ident);
                        }
                        values.resize(total_len, 0);

                        // 4. 递归构建 Aggregate Value
                        build_global_init_value(program, &values, &dims)
                    }
                } else {
                    // 无初始化 -> ZeroInit
                    program.new_value().zero_init(ty.clone())
                };

                // 4. 创建 Global Alloc
                // global_alloc 只需要传入 init value，类型由 init value 决定
                // (对于 zero_init，创建时已经绑定了 type)
                let global_alloc = program.new_value().global_alloc(init_value);

                // 5. 设置名字
                let name = format!("@{}", def.ident);
                program.set_value_name(global_alloc, Some(name));

                // 6. 注册到符号表 (存的是指针)
                symbol_table.insert_var(def.ident.clone(), global_alloc);
            }
        }
    }
}
