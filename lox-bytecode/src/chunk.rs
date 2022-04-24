use std::fmt::Debug;

use crate::value::{ValueArray, Value};

#[derive(Debug)]
pub enum OpCode {
    Constant,
    ConstantIndex(u8),
    Return,
}

pub struct Chunk {
    code: Vec<OpCode>,
    constants: ValueArray,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: ValueArray::new(),
        }
    }

    pub fn write(&mut self, op: OpCode) -> u8 {
        self.code.push(op);
        (self.code.len() - 1).try_into().unwrap()
    }

    pub fn free(&mut self) {
        self.code = Vec::new()
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        self.constants.write(value)
    }

    // === Debug ===

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            let op = self.code.get(offset).unwrap();
            let inst = self.print_instruction(op, offset);
            println!("{offset:04} {inst}");

            offset = match op {
                OpCode::Constant => offset + 2,
                _ => offset + 1,
            };
        }
    }
    
    fn print_instruction(&self, op: &OpCode, offset: usize) -> String {
        match op {
            OpCode::Constant => {
                let value = match self.code.get(offset + 1).unwrap() {
                    OpCode::ConstantIndex(i) => self.constants.read(*i),
                    got => return format!("Expect constant index after {op:?}, got {got:?}"),
                };
                format!("{op:?} '{value}'")
            },
            _ => format!("{op:?}"),
        }
    }
}
