use std::isize;

use crate::compiler::chunk::{Chunk, OpCode};

#[allow(dead_code)]
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

    if offset > 0 && chunk.get_location(offset).row == chunk.get_location(offset - 1).row {
        print!("{:3}| ", "");
    } else {
        print!("{:4} ", chunk.get_location(offset));
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::CONST => constant_op("CONST", &chunk, offset),
        OpCode::DefineGlobal => constant_op("DEFINE_GLOBAL", &chunk, offset),
        OpCode::LoadGlobal => constant_op("GET_GLOBAL", &chunk, offset),
        OpCode::SetGlobal => constant_op("SET_GLOBAL", &chunk, offset),
        OpCode::List => constant_op("LIST", &chunk, offset),
        OpCode::Map => constant_op("MAP", &chunk, offset),
        OpCode::LoadLocal => byte_op("GET_LOCAL", &chunk, offset),
        OpCode::SetLocal => byte_op("SET_LOCAL", &chunk, offset),
        OpCode::Call => byte_op("CALL", &chunk, offset),
        OpCode::RETURN => simple_op("RETURN", offset),
        OpCode::GetIndex => simple_op("GET_INDEX", offset),
        OpCode::SetIndex => simple_op("SET_INDEX", offset),
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
        OpCode::Inc => simple_op("INC", offset),
        OpCode::Dec => simple_op("DEC", offset),
        OpCode::PreInc => simple_op("PRE_INC", offset),
        OpCode::PreDec => simple_op("PRE_DEC", offset),
        OpCode::PostInc => simple_op("POST_INC", offset),
        OpCode::PostDec => simple_op("POST_DEC", offset),
        OpCode::Compare => simple_op("COMPARE", offset),
        OpCode::JumpIfFalse => jump_op("JUMP_IF_FALSE", 1, &chunk, offset),
        OpCode::Jump => jump_op("JUMP", 1, &chunk, offset),
        OpCode::Loop => jump_op("LOOP", -1, &chunk, offset),
        OpCode::Closure => {
            let mut offset = offset;
            offset += 1;
            let constant = chunk.code[offset];
            offset += 1;
            print!("{:16} {:4} ", "CLOSURE", constant);
            print!("{}", chunk.constants[constant]);
            println!();
            offset
        }
    }
}

fn simple_op(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn byte_op(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!("{:16} [{}] = ", name, slot);
    print!("{}", chunk.constants[slot]);
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
    print!("{}", chunk.constants[slot]);
    println!();
    offset + 2
}
