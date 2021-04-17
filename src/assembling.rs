use crate::{ compiling::Compiler, utils::{ Result, Serial } };
use std::collections::HashMap;

const INSTRUCTIONS: [&str; 53] = [
    "nop",
    "push", "push_l", "pop", "pull", "clean", "clear", "copy", "swap",
    "unoffset", "offset", "call", "sys", "ret",
    "jmp", "br",
    "addr", "deref", "store",
    "fti", "itf",
    "i_add", "i_sub", "i_mul", "i_div", "i_rem", "i_eq", "i_neq", "i_lt", "i_gt", "i_lte", "i_gte", "i_not", "i_neg",
    "f_add", "f_sub", "f_mul", "f_div", "f_rem", "f_eq", "f_neq", "f_lt", "f_gt", "f_lte", "f_gte", "f_not", "f_neg",
    "shr", "shl",
    "b_not",
    "b_and", "b_or", "b_xor",
];

const SYSCALLS: [&str; 7] = [
    "brk",
    "print", "flush",
    "alloc", "dealloc",
    "memtrans", "memwrite",
];

pub struct Assembler {
    compiler: Compiler,
    output: Vec<u8>,
    next_byte_index: usize,

    opcodes: HashMap<&'static str, u8>,
    syscalls: HashMap<&'static str, u8>,
    labels: HashMap<String, usize>,
}

impl Assembler {
    pub fn new(compiler: Compiler) -> Self {
        let mut opcodes = HashMap::new();
        let mut syscalls = HashMap::new();

        for (i, &e) in INSTRUCTIONS.iter().enumerate() {
            opcodes.insert(e, i as u8);
        }

        for (i, &e) in SYSCALLS.iter().enumerate() {
            syscalls.insert(e, i as u8);
        }

        Self {
            compiler,
            opcodes,
            syscalls,
            next_byte_index: 0,
            output: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn assemble(mut self) -> Result<Vec<u8>> {
        let commands: Vec<String> = self.compiler.compile()?.split_ascii_whitespace().map(|s| s.to_string()).collect();

        for command in &commands {
            if command.starts_with('#') {
                self.labels.insert(command[1..].to_string(), self.next_byte_index);
            }
            else if command.starts_with('.') {
                self.next_byte_index += 8;
            }
            else if command.starts_with('@') {
                self.next_byte_index += 1;
            }
            else if command.starts_with('\'') {
                self.next_byte_index += 8;
            }
            else if let Ok(_) = command.parse::<u64>() {
                self.next_byte_index += 8;
            }
            else if let Some(_) = self.opcodes.get(&command[..]) {
                self.next_byte_index += 1;
            }
            else {
                return err!("invalid assembly instruction: {}", command);
            }
        }

        for command in commands {
            if command.starts_with('#') {

            }
            else if command.starts_with('.') {
                if let Some(&l) = self.labels.get(&command[1..]) {
                    self.output.extend(&(l as u64).serialize());
                }
                else {
                    return err!("undefined label {}", command);
                }
            }
            else if command.starts_with('@') {
                if let Some(&b) = self.syscalls.get(&command[1..]) {
                    self.output.push(b);
                }
                else {
                    return err!("invalid syscall");
                }
            }
            else if command.starts_with('\'') {
                if let Ok(n) = &command[1..].parse::<u64>() {
                    self.output.extend(&n.to_be_bytes());
                }
                else {
                    return err!("invalid assembly str literal");
                }
            }
            else if let Ok(n) = command.parse::<u64>() {
                self.output.extend(&n.serialize());
            }
            else if let Some(&b) = self.opcodes.get(&command[..]) {
                self.output.push(b);
            }
        }

        Ok(self.output)
    }
}
