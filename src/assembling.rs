use crate::{ compiling::Compiler, utils::{ Result, Serial } };
use std::collections::HashMap;

const OPCODES: [&str; 55] = [
    "nop",
    "push", "push_l", "pop", "pull", "clean", "clear", "copy", "clone", "swap",
    "unoffset", "offset", "call", "call_s", "sys", "ret",
    "jmp", "br",
    "addr", "deref", "store",
    "fti", "itf",
    "i_add", "i_sub", "i_mul", "i_div", "i_rem", "i_eq", "i_neq", "i_lt", "i_gt", "i_lte", "i_gte", "i_not", "i_neg",
    "f_add", "f_sub", "f_mul", "f_div", "f_rem", "f_eq", "f_neq", "f_lt", "f_gt", "f_lte", "f_gte", "f_not", "f_neg",
    "shr", "shl",
    "b_not",
    "b_and", "b_or", "b_xor",
];

const SYSCALLS: [&str; 10] = [
    "brk", "err",
    "print", "flush",
    "alloc", "realloc", "dealloc",
    "memtrans", "memwrite",
    "makestring",
];

const ERROR_CODES: [&str; 4] = [
    "noerr",
    "out_of_index", "underflow",
    "divide_by_zero"
];

fn create_index_map(arr: &[&'static str]) -> HashMap<&'static str, u8> {
    let mut map = HashMap::new();

    for (i, &e) in arr.iter().enumerate() {
        map.insert(e, i as u8);
    }

    map
}

pub struct Assembler {
    compiler: Compiler,
    output: Vec<u8>,
    next_byte_index: usize,

    opcodes: HashMap<&'static str, u8>,
    syscalls: HashMap<&'static str, u8>,
    error_codes: HashMap<&'static str, u8>,
    labels: HashMap<String, usize>,
}

impl Assembler {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            compiler,
            opcodes: create_index_map(&OPCODES),
            syscalls: create_index_map(&SYSCALLS),
            error_codes: create_index_map(&ERROR_CODES),
            next_byte_index: 24,
            output: Vec::new(),
            labels: HashMap::new(),
        }
    }

    pub fn assemble(mut self, operations_stack_size: usize, variables_stack_size: usize, call_stack_size: usize) -> Result<Vec<u8>> {
        self.output.extend(&(operations_stack_size as u64).serialize());
        self.output.extend(&(variables_stack_size as u64).serialize());
        self.output.extend(&(call_stack_size as u64).serialize());

        let commands: Vec<String> = self.compiler.compile()?.split_ascii_whitespace().map(|s| s.to_string()).collect();

        for command in &commands {
            match command {
                cmd if cmd.starts_with('#') => { self.labels.insert(command[1..].to_string(), self.next_byte_index); },
                cmd if cmd.starts_with('.') => self.next_byte_index += 8,
                cmd if cmd.starts_with('@') => self.next_byte_index += 1,
                cmd if cmd.starts_with('!') => self.next_byte_index += 8,
                cmd if cmd.starts_with('\'') => self.next_byte_index += 8,
                cmd => {
                    if let Ok(_) = cmd.parse::<u64>() {
                        self.next_byte_index += 8;
                    }
                    else if let Some(_) = self.opcodes.get(&cmd[..]) {
                        self.next_byte_index += 1;
                    }
                    else {
                        return err!("invalid opcode {}", cmd);
                    }
                },
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
            else if command.starts_with('!') {
                if let Some(&b) = self.error_codes.get(&command[1..]) {
                    self.output.extend(&(b as u64).serialize());
                }
                else {
                    return err!("invalid error_code");
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
