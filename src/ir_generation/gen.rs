use crate::parser::ast::Type;
use crate::parser::ast::*;
use crate::ir_generation::decl::process_global_decl;
use crate::ir_generation::genfunc::FunctionGenerator;
use crate::analysis::scope::{Symbol, SymbolTable};
use koopa::ir::{builder_traits::*};
use koopa::ir::{FunctionData, Program, Type as IrType};

pub trait GenerateProgram {
    type Out;
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()>;
}

// 辅助函数：声明单个函数
fn declare_single_function(
    func_def: &FuncDef,
    program: &mut Program,
    symbol_table: &mut SymbolTable,
) {
    // A. 准备参数类型列表
    // SysY 的参数目前只有 int，所以全部转为 i32
    let param_types: Vec<IrType> = func_def.params.iter().map(|_| IrType::get_i32()).collect();

    // B. 准备返回值类型
    let ret_type = match func_def.func_type {
        Type::Int => IrType::get_i32(),
        Type::Void => IrType::get_unit(),
    };

    // C. 创建 FunctionData
    // 名字加 @ 前缀符合 Koopa 规范
    let func_name = format!("@{}", func_def.ident);
    let mut func_data = FunctionData::new(func_name, param_types, ret_type);

    // (可选) 设置参数名称，方便调试生成的 IR
    for (i, param) in func_def.params.iter().enumerate() {
        let param_val = func_data.params()[i];
        func_data
            .dfg_mut()
            .set_value_name(param_val, Some(format!("%arg_{}", param.ident)));
    }

    // D. 注册到 Program，拿到 Function Handle
    let func_handle = program.new_func(func_data);

    // E. 【关键】注册到全局符号表
    // 以后在 generate_exp 里遇到 FuncCall(name) 时，就来查这个表
    symbol_table.insert_func(func_def.ident.clone(), func_handle);
}

// 辅助函数：生成单个函数体
fn generate_single_function(func_def: &FuncDef, program: &mut Program, symbol_table: &SymbolTable) {
    // A. 获取函数 Handle
    // 此时一定能查到，因为第一遍已经全部注册了
    let func_handle = match symbol_table.lookup(&func_def.ident) {
        Some(Symbol::Func(h)) => *h,
        _ => panic!("Function {} not found in symbol table", func_def.ident),
    };

    // B. 获取 FunctionData 的可变引用
    let func_data = program.func_mut(func_handle);

    // C. 初始化生成器
    // 注意：传入 global_symbols 的克隆，因为每个函数都需要一份全局视图
    let mut gen = FunctionGenerator::new(func_data, symbol_table.clone());

    // D. 创建 Entry Block
    let entry_bb = gen
        .func
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".to_string()));
    let _ = gen.func.layout_mut().bbs_mut().push_key_back(entry_bb);
    gen.set_cur_bb(entry_bb);

    // =========================================================
    // E. 【关键】处理形参 (Alloc + Store)
    // Koopa 的参数是 Value，不可变；SysY 参数是变量，可变。
    // 必须在入口处把 Value 拷贝到栈上的局部变量里。
    // =========================================================

    // 获取 Koopa IR 中的参数 Value 列表
    let args = gen.func.params().to_vec();

    for (i, arg_val) in args.iter().enumerate() {
        let param_ast = &func_def.params[i];

        // 1. 在栈上分配空间 (Alloc)
        // 使用你现有的 alloc_variable 或 process_alloc 逻辑
        // 这里假设你封装了一个 alloc_stack_variable
        // 它会在 entry block 插入 alloc 指令，并返回该指针 Value
        let alloc_ptr = gen.alloc_variable(IrType::get_i32());

        // 2. 将参数值存入该空间 (Store)
        let store = gen.func.dfg_mut().new_value().store(*arg_val, alloc_ptr);
        gen.add_inst(store);

        // 3. 将参数名注册到当前函数的局部符号表
        // 以后在函数体内用到 param_ast.ident 时，查到的就是 alloc_ptr
        gen.symbol_table
            .insert_var(param_ast.ident.clone(), alloc_ptr);
    }

    // F. 生成函数体 Block
    gen.generate_block(&func_def.block);

    // G. 处理 Void 函数可能的缺失 Ret
    // 如果函数是 void 类型，且最后没有 return，手动补一个 ret
    if let Type::Void = func_def.func_type {
        if !gen.is_cur_bb_terminated() {
            let ret_inst = gen.func.dfg_mut().new_value().ret(None);
            gen.add_inst(ret_inst);
        }
    } else {
        if !gen.is_cur_bb_terminated() {
            let zero = gen.func.dfg_mut().new_value().integer(0);
            let ret_inst = gen.func.dfg_mut().new_value().ret(Some(zero));
            gen.add_inst(ret_inst);
        }
    }
}

// 辅助函数：向 Program 和 符号表 中注入 SysY 库函数
fn register_sysy_lib(program: &mut Program, symbol_table: &mut SymbolTable) {
    // 定义一个宏或闭包来简化重复代码
    // name: SysY 中的函数名 (如 "getint")
    // params: 参数类型列表
    // ret: 返回值类型
    let mut add_lib_func = |name: &str, params: Vec<IrType>, ret: IrType| {
        // 1. 创建 FunctionData
        // 注意：库函数在 IR 中只是声明，不需要 BasicBlock
        // 名字加 @ 前缀
        let func_name = format!("@{}", name);
        let func_data = FunctionData::new(func_name, params, ret);

        // 2. 注册到 Program，拿到 Handle
        let func_handle = program.new_func(func_data);

        // 3. 注册到全局符号表 (使用原始名字 name)
        symbol_table.insert_func(name.to_string(), func_handle);
    };

    // === 输入输出函数 ===

    // int getint()
    add_lib_func("getint", vec![], IrType::get_i32());

    // int getch()
    add_lib_func("getch", vec![], IrType::get_i32());

    // int getarray(int[])
    // 注意：数组在函数参数中是指针类型 (*i32)
    add_lib_func(
        "getarray",
        vec![IrType::get_pointer(IrType::get_i32())],
        IrType::get_i32(),
    );

    // void putint(int)
    add_lib_func("putint", vec![IrType::get_i32()], IrType::get_unit());

    // void putch(int)
    add_lib_func("putch", vec![IrType::get_i32()], IrType::get_unit());

    // void putarray(int, int[])
    add_lib_func(
        "putarray",
        vec![IrType::get_i32(), IrType::get_pointer(IrType::get_i32())],
        IrType::get_unit(),
    );

    // === 时间函数 ===

    // void starttime()
    // 实际上 SysY 运行时定义通常是 _sysy_starttime(int lineno)
    // 但如果在 AST 层面你把它当无参处理，这里可以定义为无参，或者根据具体运行时库调整
    // 这里按照标准定义，通常传行号，所以给一个 int 参数
    add_lib_func("starttime", vec![IrType::get_i32()], IrType::get_unit());

    // void stoptime()
    add_lib_func("stoptime", vec![IrType::get_i32()], IrType::get_unit());
}

impl GenerateProgram for CompUnit {
    type Out = ();

    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()> {
        // 1. 创建一个全局符号表
        let mut global_symbols = SymbolTable::new();

        register_sysy_lib(program, &mut global_symbols);

        // =========================================================
        // 【第一遍扫描】声明所有函数
        // 目的：让函数 A 在生成时能查找到函数 B 的 Handle (支持递归和乱序调用)
        // =========================================================
        for g_item in &self.items {
            match g_item {
                GlobalItem::Decl(decl) => {
                    process_global_decl(program, &mut global_symbols, decl);
                }
                GlobalItem::FuncDef(func_def) => {
                    declare_single_function(&func_def, program, &mut global_symbols);
                }
            }
        }

        // =========================================================
        // 【第二遍扫描】生成函数体
        // =========================================================
        for g_item in &self.items {
            match g_item {
                GlobalItem::Decl(_) => {}
                GlobalItem::FuncDef(func_def) => {
                    generate_single_function(func_def, program, &global_symbols);
                }
            }
        }

        Ok(())
    }
}
