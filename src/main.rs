use gen::{GenerateProgram};
use koopa::ir::*;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Result;
use std::io::{Write};
use koopa::back::KoopaGenerator;

use crate::asm::AsmBuilder;

mod ast;
mod gen;
mod genfunc;
mod asm;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // Parse command line arguments
    let mut args_iter = args();
    args_iter.next(); // skip program name
    let flag = args_iter.next().expect("Missing flag (-riscv or -koopa)");
    let input = args_iter.next().expect("Missing input file");
    let o_flag = args_iter.next().expect("Missing -o flag");
    if o_flag != "-o" {
        panic!("Expected -o flag");
    }
    let output = args_iter.next().expect("Missing output file");

    // Read input file
    let input_content = read_to_string(input)?;

    // Parse input using lalrpop parser
    let ast = sysy::CompUnitParser::new().parse(&input_content).unwrap();
    let mut program = Program::new();
    let _ = ast.generate(&mut program);

    if flag == "-riscv" {
        // Generate assembly
        let mut builder = AsmBuilder::new();
        builder.generate_program(&program);
        let mut file = File::create(output)?;
        file.write_all(builder.output.as_bytes())?;
    } else if flag == "-koopa" {
        // Generate IR
        let mut gen = KoopaGenerator::new(Vec::new());
        gen.generate_on(&program).unwrap();
        let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
        let mut file = File::create(output)?;
        writeln!(file, "{}", text_form_ir)?;
    } else {
        panic!("Invalid flag: use -riscv or -koopa");
    }

    Ok(())
}
