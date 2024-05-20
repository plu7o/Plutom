use std::isize;

use crate::compiler::chunk::{Chunk, OpCode};

#[allow(dead_code)]
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    let spacer = "-".repeat(10);
    println!("{spacer} [ {} ] {spacer}", name);
    // (1..chunk.code.len()).fold(0, |offset, _| disassemble_instruction(chunk, offset));
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
    println!("{spacer} END {spacer}");
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("[{:04}] -> ", offset);

    if offset > 0 && chunk.get_location(offset).row == chunk.get_location(offset - 1).row {
        print!("{:1}| ", "");
    } else {
        print!("{:2} ", chunk.get_location(offset).row);
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::CONST => constant_op(instruction, &chunk, offset),
        OpCode::DefineGlobal => constant_op(instruction, &chunk, offset),
        OpCode::LoadGlobal => constant_op(instruction, &chunk, offset),
        OpCode::SetGlobal => constant_op(instruction, &chunk, offset),
        OpCode::LoadLocal => constant_op(instruction, &chunk, offset),
        OpCode::SetLocal => constant_op(instruction, &chunk, offset),
        OpCode::List => constant_op(instruction, &chunk, offset),
        OpCode::Map => constant_op(instruction, &chunk, offset),
        OpCode::Call => constant_op(instruction, &chunk, offset),
        OpCode::Enum => constant_op(instruction, &chunk, offset),
        OpCode::RETURN => simple_op(instruction, offset),
        OpCode::GetIndex => simple_op(instruction, offset),
        OpCode::SetIndex => simple_op(instruction, offset),
        OpCode::NEGATE => simple_op(instruction, offset),
        OpCode::ADD => simple_op(instruction, offset),
        OpCode::SUBSTRACT => simple_op(instruction, offset),
        OpCode::MULTIPLY => simple_op(instruction, offset),
        OpCode::DIVIDE => simple_op(instruction, offset),
        OpCode::NONE => simple_op(instruction, offset),
        OpCode::TRUE => simple_op(instruction, offset),
        OpCode::FALSE => simple_op(instruction, offset),
        OpCode::NOT => simple_op(instruction, offset),
        OpCode::EQUAL => simple_op(instruction, offset),
        OpCode::GREATER => simple_op(instruction, offset),
        OpCode::LESS => simple_op(instruction, offset),
        OpCode::ECHO => simple_op(instruction, offset),
        OpCode::POP => simple_op(instruction, offset),
        OpCode::Inc => simple_op(instruction, offset),
        OpCode::Dec => simple_op(instruction, offset),
        OpCode::PreInc => simple_op(instruction, offset),
        OpCode::PreDec => simple_op(instruction, offset),
        OpCode::PostInc => simple_op(instruction, offset),
        OpCode::PostDec => simple_op(instruction, offset),
        OpCode::Compare => simple_op(instruction, offset),
        OpCode::Dup => simple_op(instruction, offset),
        OpCode::JumpIfFalse => jump_op(instruction, 1, &chunk, offset),
        OpCode::Jump => jump_op(instruction, 1, &chunk, offset),
        OpCode::Loop => jump_op(instruction, -1, &chunk, offset),
        OpCode::Closure => {
            let mut offset = offset;
            offset += 1;
            let constant = chunk.code[offset];
            offset += 1;
            print!(
                "{:15} {:4} ",
                format!("{}", OpCode::from(instruction)),
                constant
            );
            print!("{}", chunk.constants[constant]);
            println!();
            offset
        }
    }
}

fn simple_op(instruction: usize, offset: usize) -> usize {
    println!("{}", format!("{}", OpCode::from(instruction)));
    offset + 1
}

fn jump_op(instruction: usize, sign: isize, chunk: &Chunk, offset: usize) -> usize {
    let jump = (chunk.code[offset + 1] << 8) as u16;
    let jump = jump | chunk.code[offset + 2] as u16;
    println!(
        "{} {} -> {}",
        format!("{}", OpCode::from(instruction)),
        offset,
        offset as isize + 3 + sign * jump as isize
    );
    offset + 3
}

fn constant_op(instruction: usize, chunk: &Chunk, offset: usize) -> usize {
    let slot = chunk.code[offset + 1];
    print!(
        "{:16} [{}] = ",
        format!("{}", OpCode::from(instruction)),
        slot
    );
    print!("{}", chunk.constants[slot]);
    println!();
    offset + 2
}
