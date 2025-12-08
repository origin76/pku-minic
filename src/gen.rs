use crate::ast::*;
use koopa::ir::{builder_traits::*, *};
use koopa::ir::{BinaryOp, FunctionData, Program, Type};
use std::fs::File;
use std::io::Write;

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
        let func_data = FunctionData::new(
            "@".to_string() + &self.ident.clone(),
            vec![],
            Type::get_i32(),
        );
        let func = program.new_func(func_data);
        let main_data = program.func_mut(func);
        let bb = main_data
            .dfg_mut()
            .new_bb()
            .basic_block(Some("%entry".to_string()));
        let _ = main_data.layout_mut().bbs_mut().push_key_back(bb);

        match &self.block.stmt {
            Stmt::Return(exp) => {
                let result = self.generate_exp(exp, main_data);
                let ret = main_data.dfg_mut().new_value().ret(Some(result));
                main_data.layout_mut().bb_mut(bb).insts_mut().extend([ret]);
            }
        }
        Ok(())
    }
}

impl FuncDef {
    fn generate_exp(&self, exp: &Exp, func: &mut FunctionData) -> Value {
        match exp {
            Exp::UnaryExp(unary_exp) => self.generate_unary_exp(unary_exp, func),
        }
    }

    fn generate_unary_exp(&self, unary_exp: &UnaryExp, func: &mut FunctionData) -> Value {
        match unary_exp {
            UnaryExp::PrimaryExp(primary_exp) => self.generate_primary_exp(primary_exp, func),
            UnaryExp::UnaryOp(op, operand) => {
                let operand_value = self.generate_unary_exp(operand, func);
                match op {
                    UnaryOp::Plus => operand_value,
                    UnaryOp::Minus => {
                        let zero = func.dfg_mut().new_value().integer(0);
                        let value = func.dfg_mut().new_value().binary(BinaryOp::Sub, zero, operand_value);
                        let bb = func.layout().entry_bb().unwrap();
                        let _ = func.layout_mut().bb_mut(bb).insts_mut().push_key_back(value); 
                        value
                    }
                    UnaryOp::Not => {
                        let zero = func.dfg_mut().new_value().integer(0);
                        let value = func.dfg_mut().new_value().binary(BinaryOp::Eq, operand_value, zero);
                        let bb = func.layout().entry_bb().unwrap();
                        let _ = func.layout_mut().bb_mut(bb).insts_mut().push_key_back(value); 
                        value
                    }
                }
            }
        }
    }

    fn generate_primary_exp(&self, primary_exp: &PrimaryExp, func: &mut FunctionData) -> Value {
        match primary_exp {
            PrimaryExp::Number(value) => func.dfg_mut().new_value().integer(*value),
            PrimaryExp::Parentheses(exp) => {
                self.generate_exp(exp, func)
            }
        }

    }
}

pub trait GenerateAsm {
    fn generate(&self, f: &mut File) -> Result<(), ()>;
}

impl GenerateAsm for Program {
    fn generate(&self, f: &mut File) -> Result<(), ()> {
        for &func in self.func_layout() {
            let _ = writeln!(f, ".text");
            let _ = writeln!(f, ".globl {}", &self.func(func).name()[1..]);
            let _ = writeln!(f, "{}:", &self.func(func).name()[1..]);
            self.func(func).generate(f)?;
        }
        Ok(())
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self, f: &mut File) -> Result<(), ()> {
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                match self.dfg().value(inst).kind() {
                    ValueKind::Return(ret) => match self.dfg().value(ret.value().unwrap()).kind() {
                        ValueKind::Integer(num) => {
                            let _ = writeln!(f, "li a0,{}", num.value());
                            let _ = writeln!(f, "ret");
                        }
                        _ => {
                            let _ = writeln!(f, "ret");
                        }
                    },
                    ValueKind::Binary(bin) => {
                        match bin.op() {
                            BinaryOp::Add => {
                                if let ValueKind::Integer(left) = self.dfg().value(bin.lhs()).kind() {
                                    if let ValueKind::Integer(right) = self.dfg().value(bin.rhs()).kind() {
                                        let _ = writeln!(f, "li a0,{}", left.value());
                                        let _ = writeln!(f, "li a1,{}", right.value());
                                        let _ = writeln!(f, "add a0, a0, a1");
                                    }
                                }
                            }
                            BinaryOp::Sub => {
                                if let ValueKind::Integer(right) = self.dfg().value(bin.rhs()).kind() {
                                    let _ = writeln!(f, "li a1, {}", right.value());
                                    let _ = writeln!(f, "li a0, 0");
                                    let _ = writeln!(f, "sub a0, a0, a1");
                                }
                            }
                            BinaryOp::Eq => {
                                if let ValueKind::Integer(right) = self.dfg().value(bin.rhs()).kind() {
                                    if right.value() == 0 {
                                        let _ = writeln!(f, "li a0, 0");
                                    } else {
                                        let _ = writeln!(f, "li a0, 1");
                                    }
                                }
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
        }
        Ok(())
    }
}
