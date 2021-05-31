#[macro_use]
mod utils;

mod lexing;
mod parsing;
mod compiling;
mod assembling;

use std::{ fs, env, io::{ self, prelude::Write } };

fn main() -> io::Result<()> {
    let mut operations_stack_size: usize = 1024;
    let mut variables_stack_size: usize = 1024;
    let mut call_stack_size: usize = 1024;
    let mut path = None;

    let mut args: Vec<String> = env::args().skip(1).rev().collect();

    while let Some(s) = args.pop() {
        match &s[..] {
            "-o" => operations_stack_size = args.pop().expect("expected stack size").parse().expect("invalid stack size"),
            "-v" => variables_stack_size = args.pop().expect("expected stack size").parse().expect("invalid stack size"),
            "-c" => call_stack_size = args.pop().expect("expected stack size").parse().expect("invalid stack size"),
            _ => path = Some(s)
        }
    }

    let source = fs::read_to_string(path.expect("expected a path"))?;

    match assembling::Assembler::new(compiling::Compiler::new(parsing::Parser::new(lexing::Lexer::new(source)))).assemble(operations_stack_size, variables_stack_size, call_stack_size) {
        Ok(b) => {
            fs::File::create("bin/main.pab").unwrap().write_all(&b)?;
        },
        Err(s) => eprintln!("{}", s),
    }

    Ok(())
}
