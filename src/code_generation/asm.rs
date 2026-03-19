use super::reg_context::FuncContext;
use super::array::get_type_size;
use koopa::ir::{
    Program, Type, Value, ValueKind, dfg::DataFlowGraph
};
use std::fmt::Write;

// 2. 定义代码生成器主结构体
pub struct AsmBuilder<'a> {
    // 最终输出的汇编字符串
    pub output: String,
    // Option 是因为在处理全局变量时可能不需要它，或者用来显式控制生命周期
    pub ctx: Option<FuncContext>,
    pub program: &'a Program,
}

impl<'a> AsmBuilder<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self {
            output: String::new(),
            ctx: None,
            program,
        }
    }

    pub fn generate_program(&mut self, program: &Program) {
        for &value in program.inst_layout() {
            self.generate_global(program, value);
        }

        for (_func, func_data) in program.funcs() {
            // 如果只有声明没有定义的函数（比如库函数），通常 func_data.layout().entry_bb() 是 None
            if func_data.layout().entry_bb().is_some() {
                self.generate_function(func_data);
            }
        }
    }

    pub fn get_value_type(&self, val: Value, dfg: &DataFlowGraph) -> Type {
        if val.is_global() {
            // 全局变量：去 Program 查
            self.program.borrow_value(val).ty().clone()
        } else {
            // 局部变量：去 DFG 查
            dfg.value(val).ty().clone()
        }
    }

    fn generate_global_value(&mut self, program: &Program, value: Value) {
        let data = program.borrow_value(value);
        
        match data.kind() {
            // 标量数值
            ValueKind::Integer(int) => {
                let _ = writeln!(self.output, "  .word {}", int.value());
            }
            // 聚合体 (数组) -> 递归生成每个元素
            ValueKind::Aggregate(agg) => {
                for &elem in agg.elems() {
                    self.generate_global_value(program, elem);
                }
            }
            // 聚合体内部可能包含 ZeroInit (部分 0 初始化)
            ValueKind::ZeroInit(_) => {
                let size = get_type_size(&data.ty());
                let _ = writeln!(self.output, "  .zero {}", size);
            }
            _ => panic!("Invalid global init value type"),
        }
    }

    pub fn generate_global(&mut self, program: &Program, value: Value) {
        let value_data = program.borrow_value(value);

        if let ValueKind::GlobalAlloc(alloc) = value_data.kind() {
            let name = value_data.name().as_ref().unwrap().replace("@", "");
            let init_val = alloc.init();
            let init_data = program.borrow_value(init_val);

            match init_data.kind() {
                // === 情况 A: 零初始化 (int a; 或 int a[10];) -> .bss ===
                ValueKind::ZeroInit(_) => {
                    let _ = writeln!(self.output, "  .bss");
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    
                    // 【修复】不能写死 4，必须根据类型计算大小
                    // 例如 [i32, 10] 需要 .zero 40
                    let size = get_type_size(&init_data.ty());
                    let _ = writeln!(self.output, "  .zero {}", size);
                    let _ = writeln!(self.output, "");
                }

                // === 情况 B: 有初始化 (int a=1; 或 int a[2]={1,2};) -> .data ===
                ValueKind::Integer(_) | ValueKind::Aggregate(_) => {
                    let _ = writeln!(self.output, "  .data");
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    
                    // 调用递归辅助函数生成 .word 序列
                    self.generate_global_value(program, init_val);
                    
                    let _ = writeln!(self.output, "");
                }

                _ => {}
            }
        }
    }
}
