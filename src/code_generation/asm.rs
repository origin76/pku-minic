use super::reg_context::FuncContext;
use koopa::ir::{
    Program, Value, ValueKind,
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

    pub fn generate_global(&mut self, program: &Program, value: Value) {
        // 获取 Value 的详细数据
        let value_data = program.borrow_value(value);

        // 我们只关心 GlobalAlloc 指令
        if let ValueKind::GlobalAlloc(alloc) = value_data.kind() {
            // 1. 获取变量名 (去掉 @ 前缀)
            let name = value_data.name().as_ref().unwrap().replace("@", "");

            // 2. 获取初始值
            let init_val = alloc.init();
            let init_data = program.borrow_value(init_val);

            // 3. 根据初始值类型决定放入 .data 还是 .bss
            match init_data.kind() {
                // === 情况 A: 整数初始化 (int a = 10;) -> .data ===
                ValueKind::Integer(int) => {
                    let _ = writeln!(self.output, "  .data"); // 切换到数据段
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    let _ = writeln!(self.output, "  .word {}", int.value()); // 写入 4 字节整数
                    let _ = writeln!(self.output, ""); // 空行美观
                }

                // === 情况 B: 零初始化 (int a;) -> .bss ===
                ValueKind::ZeroInit(_) => {
                    let _ = writeln!(self.output, "  .bss"); // 切换到 BSS 段
                    let _ = writeln!(self.output, "  .globl {}", name);
                    let _ = writeln!(self.output, "{}:", name);
                    // .zero N 表示分配 N 字节并填 0
                    // i32 占 4 字节
                    let _ = writeln!(self.output, "  .zero 4");
                    let _ = writeln!(self.output, "");
                }

                // === 情况 C: 聚合类型 (数组) ===
                // 如果你以后支持数组，这里会是 ValueKind::Aggregate
                // 需要递归生成 .word 或 .zero
                _ => {
                    // 暂时处理不了复杂类型，或者留空
                }
            }
        }
    }
}
