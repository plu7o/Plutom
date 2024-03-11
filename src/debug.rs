use std::isize;

use crate::chunk::{Chunk, OpCode};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    // (1..chunk.code.len()).fold(0, |offset, _| disassemble_instruction(chunk, offset));
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("[{:04}] -> ", offset);
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("{:3}| ", "");
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::CONST => constant_op("CONST", &chunk, offset),
        OpCode::DefineGlobal => constant_op("DEFINE_GLOBAL", &chunk, offset),
        OpCode::GetGlobal => constant_op("GET_GLOBAL", &chunk, offset),
        OpCode::SetGlobal => constant_op("SET_GLOBAL", &chunk, offset),
        OpCode::GetLocal => byte_op("GET_LOCAL", &chunk, offset),
        OpCode::SetLocal => byte_op("SET_LOCAL", &chunk, offset),
        OpCode::RETURN => simple_op("RETURN", offset),
        OpCode::NEGATE => simple_op("NEGATE", offset),
        OpCode::ADD => simple_op("ADD", offset),
        OpCode::SUBSTRACT => simple_op("SUBSTRACT", offset),
        OpCode::MULTIPLY => simple_op("MULTIPLY", offset),
        OpCode::DIVIDE => simple_op("DIVIDE", offset),
        OpCode::NONE => simple_op("NONE", offset),
        OpCode::TRUE => simple_op("TRUE", offset),
        OpCode::FALSE => simple_op("FALSE", offset),
        OpCode::NOT => simple_op("NOT", offset),
        OpCode::EQUAL => simple_op("EQUAL", offset),
        OpCode::GREATER => simple_op("GREATER", offset),
        OpCode::LESS => simple_op("LESS", offset),
        OpCode::ECHO => simple_op("ECHO", offset),
        OpCode::POP => simple_op("POP", offset),
        OpCode::Compare => simple_op("COMPARE", offset),
        OpCode::JumpIfFalse => jump_op("JUMP_IF_FALSE", 1, &chunk, offset),
        OpCode::Jump => jump_op("JUMP", 1, &chunk, offset),
        OpCode::Loop => jump_op("LOOP", -1, &chunk, offset),
    }
}

fn simple_op(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn byte_op(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:16} [{}] = ", name, slot);
    chunk.constants[slot].print();
    println!();
    return offset + 2;
}

fn jump_op(name: &str, sign: isize, chunk: &Chunk, offset: usize) -> usize {
    let jump = (chunk.code[offset + 1] << 8) as u16;
    let jump = jump | chunk.code[offset + 2] as u16;
    println!(
        "{} {} -> {}",
        name,
        offset,
        offset as isize + 3 + sign * jump as isize
    );
    offset + 3
}

fn constant_op(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:16} [{}] = ", name, slot);
    chunk.constants[slot].print();
    println!();
    offset + 2
}
