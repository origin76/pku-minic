use gen::{GenerateAsm, GenerateProgram};
use koopa::ir::*;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Result;
use std::io::{self, Write};
use koopa::ir::{*, builder_traits::*};
use koopa::back::KoopaGenerator;

mod ast;
mod gen;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let mut program = Program::new();
    let _ = ast.generate(&mut program);
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(&program).unwrap();
    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
    
    let mut file = File::create(output)?;
    program.generate(&mut file);
    writeln!(file,"{}", text_form_ir)?;
    Ok(())
}
