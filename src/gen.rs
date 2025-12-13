use crate::ast::*;
use crate::genfunc::FunctionGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{FunctionData, Program, Type};

pub trait GenerateProgram {
    type Out;
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()>;
}

impl GenerateProgram for CompUnit {
    type Out = ();
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()> {
        self.func_def.generate(program)
    }
}

impl GenerateProgram for FuncDef {
    type Out = ();
    fn generate(&self, program: &mut Program) -> Result<Self::Out, ()> {
        let func_data = FunctionData::new("@".to_string() + &self.ident, vec![], Type::get_i32());
        let func = program.new_func(func_data);
        let main_data = program.func_mut(func);

        // 2. 创建 Entry Block
        let entry_bb = main_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let _ = main_data.layout_mut().bbs_mut().push_key_back(entry_bb);

        // 3. 【核心修改】实例化生成器，并设置初始状态
        let mut gen = FunctionGenerator::new(main_data);
        // 初始化 current_bb 为 entry
        gen.set_cur_bb(entry_bb);

        // 4. 使用生成器处理函数体
        for i in &self.block.items {
            match i {
                BlockItem::Stmt(exp) => {
                    match exp {
                        Stmt::Return(ret_exp) => {
                            // 调用 gen.generate_exp，而不是 self.generate_exp
                            let result = gen.generate_exp(&ret_exp);

                            // 生成 ret 指令
                            let ret = gen.func.dfg_mut().new_value().ret(Some(result));

                            // 使用 gen 插入指令 (它知道该插在 entry 还是某个 end_bb)
                            gen.add_inst(ret);
                        }
                    }
                }
                BlockItem::Decl(decl) => {
                    ()
                }
            }
        }
        Ok(())
    }
}
