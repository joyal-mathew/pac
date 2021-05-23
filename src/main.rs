#[macro_use]
mod utils;

mod lexing;
mod parsing;
mod compiling;
mod assembling;

use std::{ fs, io::prelude::Write };

/* TODO:
 * automatic memory management
 * automatic stack allocation size
 * structures
 * helpful error messages
 * */

fn main() {
    let source = fs::read_to_string("main.pas").unwrap();

    match assembling::Assembler::new(compiling::Compiler::new(parsing::Parser::new(lexing::Lexer::new(source)))).assemble() {
        Ok(b) => {
            fs::File::create("bin/main.pab").unwrap().write_all(&b).unwrap();
        },
        Err(s) => eprintln!("{}", s),
    }
}
