#[macro_use]
mod utils;

mod lexing;
mod parsing;
mod compiling;
mod assembling;

use std::{ fs::{ self, File }, env, io::prelude::Write };

struct ArgumentData {
    operations_stack_size: usize,
    variables_stack_size: usize,
    call_stack_size: usize,

    path: String,
    out_path: String,
}

fn parse_args() -> Result<ArgumentData, String> {
    let mut operations_stack_size: usize = 1024;
    let mut variables_stack_size: usize = 1024;
    let mut call_stack_size: usize = 1024;

    let mut path = None;
    let mut out_path = None;

    let mut args: Vec<String> = env::args().skip(1).rev().collect();

    while let Some(s) = args.pop() {
        match &s[..] {
            "-o" => operations_stack_size = args.pop().ok_or("expected a stack size".to_string())?.parse().or(Err("invalid stack size".to_string()))?,
            "-v" => variables_stack_size = args.pop().ok_or("expected a stack size".to_string())?.parse().or(Err("invalid stack size".to_string()))?,
            "-c" => call_stack_size = args.pop().ok_or("expected a stack size".to_string())?.parse().or(Err("invalid stack size".to_string()))?,
            "--out" => out_path = Some(args.pop().ok_or("expected an output path".to_string())?),
            _ => path = Some(s)
        }
    }

    Ok(ArgumentData {
        operations_stack_size,
        variables_stack_size,
        call_stack_size,

        path: path.ok_or("no input file provided")?,
        out_path: out_path.unwrap_or("a.pab".to_string()),
    })
}

fn main() -> Result<(), String> {
    let argument_data = parse_args()?;

    let source = fs::read_to_string(argument_data.path).unwrap();

    let bytes = assembling::Assembler::new(compiling::Compiler::new(parsing::Parser::new(lexing::Lexer::new(source)))).assemble(argument_data.operations_stack_size, argument_data.variables_stack_size, argument_data.call_stack_size)?;
    File::create(argument_data.out_path).unwrap().write_all(&bytes).unwrap();
    Ok(())
}
