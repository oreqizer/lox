use lox_bytecode::{Chunk, OpCode};

fn main() {
   let mut chunk = Chunk::new();
   let i = chunk.write_constant(13.37);
   chunk.write_code(OpCode::Constant, 123);
   chunk.write_code(OpCode::ConstantIndex(i), 123);
   chunk.write_code(OpCode::Return.into(), 123);
   chunk.disassemble("test chunk");
}
