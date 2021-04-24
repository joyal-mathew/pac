#![allow(dead_code)]

#[macro_use]
mod utils;

mod lexing;
mod parsing;
mod compiling;
mod assembling;

use std::{ fs, io::prelude::Write };

/* TODO:
 * arrays (figure out how to implement the type, c-like maybe?)
 * make emit a macro and auto format
 * string/array operations
 * structs
 * a->f() notation
 * headers for runtime requirements
 * library functions
 * make functions first-class
 * */

fn main() {
    let source = fs::read_to_string("main.pas").unwrap();

    match assembling::Assembler::new(compiling::Compiler::new(parsing::Parser::new(lexing::Lexer::new(source)))).assemble() {
        Ok(b) => {
            fs::File::create("bin/main.pab").unwrap().write_all(&b).unwrap();
        },
        Err(s) => eprintln!("{}", s),
    }

/*     loop {
        match assembling::Assembler::new(compiling::Compiler::new(parsing::Parser::new(lexing::Lexer::new(utils::input("> ").unwrap())))).assemble() {
            Ok(b) => executing::Executer::new(b).run(),
            Err(s) => eprintln!("{}", s),
        }
    } */
}
