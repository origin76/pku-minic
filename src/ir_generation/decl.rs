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

/// 辅助：根据维度生成 Koopa 数组类型
pub fn build_array_type(dims: &[usize]) -> Type {
    let mut ty = Type::get_i32();
    // 从内向外包裹: [2, 3] -> Array(Array(i32, 3), 2)
    for &dim in dims.iter().rev() {
        ty = Type::get_array(ty, dim);
    }
    ty
}

fn build_global_aggregate(program: &mut Program, data: &[i32], dims: &[usize]) -> Value {
    // Base Case: 标量，直接返回 Integer Value
    if dims.is_empty() {
        return program.new_value().integer(data[0]);
    }

    // Recursive Case: 数组
    let current_dim_len = dims[0];
    let remaining_dims = &dims[1..];
    
    // 计算子元素（下一维）的大小
    // 例如 a[2][3]，当前是第一维(2)，子元素是 [3]，大小为 3
    let stride = remaining_dims.iter().product::<usize>().max(1);

    let mut agg_elems = Vec::new();
    for i in 0..current_dim_len {
        let start = i * stride;
        // 切片获取属于该子元素的数据
        let sub_data = &data[start..start + stride];
        
        // 递归构建
        let elem = build_global_aggregate(program, sub_data, remaining_dims);
        agg_elems.push(elem);
    }

    program.new_value().aggregate(agg_elems)
}

pub fn process_global_decl(program: &mut Program, symbol_table: &mut SymbolTable, decl: &Decl) {
    match decl {
        // =========================================================
        // 全局常量 (const int a = 1; const int a[2] = {1, 2};)
        // =========================================================
        Decl::ConstDecl(c) => {
            for def in &c.defs {
                // 1. 计算维度
                let dims: Vec<usize> = def.dims.iter()
                    .map(|d| evaluate_const_exp( symbol_table,d) as usize)
                    .collect();

                // 2. 根据维度分流
                if dims.is_empty() {
                    // --- 标量 ---
                    if let ConstInitVal::Exp(e) = &def.init {
                        let val = evaluate_const_exp(symbol_table,e);
                        // 仅存入常量表供折叠，不需要 global_alloc (除非你要支持取 const 变量的地址)
                        symbol_table.insert_const(def.ident.clone(), val);
                    }
                } else {
                    // --- 数组 ---
                    let total_len: usize = dims.iter().product();
                    
                    // 2.1 展平数据
                    let mut values = flatten_const_init_val(&def.init, symbol_table);
                    
                    // 2.2 补零
                    if values.len() > total_len {
                        panic!("Too many initializers for global const array {}", def.ident);
                    }
                    values.resize(total_len, 0);

                    // 2.3 【双重注册】
                    // (A) 存入 ConstArray 用于编译期常量折叠 (a[0])
                    symbol_table.insert_const_array(def.ident.clone(), values.clone());

                    // (B) 创建 Global Alloc 用于运行时地址访问 (f(a) 或 a[var])
                    let init_val = build_global_aggregate(program, &values, &dims);
                    
                    let global_alloc = program.new_value().global_alloc(init_val);
                    let ptr_type = program.borrow_value(global_alloc).ty().clone(); 
                    program.set_value_name(global_alloc, Some(format!("@{}", def.ident)));
                    
                    // 注册为 Var (指针)
                    symbol_table.insert_var(def.ident.clone(), global_alloc,ptr_type);
                }
            }
        }

        // =========================================================
        // 全局变量 (int a = 1; int a[2];)
        // =========================================================
        Decl::VarDecl(v) => {
            for def in &v.defs {
                // 1. 计算维度
                let dims: Vec<usize> = def.dims.iter()
                    .map(|d| evaluate_const_exp( symbol_table,d) as usize)
                    .collect();
                
                let ty = build_array_type(&dims);

                // 2. 准备初始化 Value
                let init_value = if let Some(init) = &def.init {
                    // 有显式初始化
                    if dims.is_empty() {
                        // --- 标量初始化 ---
                        if let InitVal::Exp(e) = init {
                            let val = evaluate_const_exp(symbol_table,e);
                            program.new_value().integer(val)
                        } else {
                            panic!("Scalar initialized with list");
                        }
                    } else {
                        // --- 数组初始化 ---
                        let total_len: usize = dims.iter().product();
                        
                        // A. 展平
                        let mut values = flatten_global_init_val(init, symbol_table);
                        
                        // B. 补零
                        if values.len() > total_len {
                            panic!("Too many initializers for global array {}", def.ident);
                        }
                        values.resize(total_len, 0);
                        
                        // C. 优化：如果全是 0，直接使用 zeroinit
                        if values.iter().all(|&x| x == 0) {
                            program.new_value().zero_init(ty.clone())
                        } else {
                            // D. 构建 Aggregate
                            build_global_aggregate(program, &values, &dims)
                        }
                    }
                } else {
                    // 无显式初始化 -> 默认零初始化
                    program.new_value().zero_init(ty.clone())
                };

                // 3. 创建 Global Alloc 指令
                let global_alloc = program.new_value().global_alloc(init_value);
                let ptr_type = program.borrow_value(global_alloc).ty().clone();
                
                // 4. 设置名字
                program.set_value_name(global_alloc, Some(format!("@{}", def.ident)));

                // 5. 注册到符号表 (存的是指针 Value)
                symbol_table.insert_var(def.ident.clone(), global_alloc,ptr_type);
            }
        }
    }
}