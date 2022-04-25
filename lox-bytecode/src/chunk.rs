use std::fmt::Debug;

use crate::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant,
    ConstantIndex(usize),
    Return,
}

pub struct Chunk {
    code: Vec<OpCode>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write_code(&mut self, op: OpCode, line: usize) -> usize {
        self.code.push(op);
        self.lines.push(line);
        self.code.len() - 1
    }

    pub fn read_code(&self, index: usize) -> OpCode {
        unsafe { *self.code.get_unchecked(index) }
    }

    pub fn read_line(&self, index: usize) -> usize {
        unsafe { *self.lines.get_unchecked(index) }
    }

    pub fn write_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn read_constant(&self, index: usize) -> Value {
        unsafe { *self.constants.get_unchecked(index) }
    }

    // === Debug ===

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");

        let mut offset = 0;
        while offset < self.code.len() {
            let op = self.read_code(offset);
            let inst = self.print_instruction(op, offset);

            let line = self.read_line(offset);
            let line = if offset > 0 && line == self.read_line(offset - 1) {
                format!("   |")
            } else {
                format!("{line:4}")
            };

            println!("{offset:04} {line} {inst}");

            offset = match op {
                OpCode::Constant => offset + 2,
                _ => offset + 1,
            };
        }
    }

    fn print_instruction(&self, op: OpCode, offset: usize) -> String {
        let op_str = format!("{op:?}");

        match op {
            OpCode::Constant => {
                let i = match self.code.get(offset + 1) {
                    Some(OpCode::ConstantIndex(i)) => *i,
                    Some(got) => return format!("Expect constant index after {op:?}, got {got:?}"),
                    None => return format!("Unexpected EOF"),
                };
                let value = self.read_constant(i);
                format!("{op_str:16} {i} '{value}'")
            }
            _ => format!("{op_str:16}"),
        }
    }
}
