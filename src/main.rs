mod chunk;
mod debug;
mod value;
mod vm;

use std::rc::Rc;

use chunk::Chunk;
use chunk::OpCode;
use vm::VM;

fn main() {
    let mut vm = VM::init();
    let mut chunk: Chunk = Chunk::init();

    let constant = chunk.add_const(1.2);
    chunk.write(OpCode::CONST as usize, 1);
    chunk.write(constant, 1);

    let constant = chunk.add_const(3.4);
    chunk.write(OpCode::CONST as usize, 1);
    chunk.write(constant, 1);

    chunk.write(OpCode::ADD as usize, 1);

    let constant = chunk.add_const(5.6);
    chunk.write(OpCode::CONST as usize, 1);
    chunk.write(constant, 1);

    chunk.write(OpCode::DIVIDE as usize, 1);

    chunk.write(OpCode::NEGATE as usize, 1);
    chunk.write(OpCode::RETURN as usize, 1);

    // debug::disassemble_chunk(&chunk, "test chunk");
    vm.interpret(Rc::new(chunk));
}
