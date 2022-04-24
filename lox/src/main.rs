use lox_bytecode::{Chunk, OpCode};

fn main() {
   let mut chunk = Chunk::new();
   let i = chunk.add_constant(13.37);
   chunk.write(OpCode::Constant);
   chunk.write(OpCode::ConstantIndex(i));
   chunk.write(OpCode::Return.into());
   chunk.disassemble("test chunk");
}
