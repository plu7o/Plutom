use crate::{
    chunk::{Chunk, OpCode},
    value::print_value,
};

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);
    // (1..chunk.code.len()).fold(0, |offset, _| disassemble_instruction(chunk, offset));
    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

#[rustfmt::skip]
pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("[{:04}] -> ", offset);
    if offset > 0 && chunk.get_line(offset) == chunk.get_line(offset - 1) {
        print!("{:3}| ", "");
    } else {
        print!("{:4} ", chunk.get_line(offset));
    }

    let instruction = chunk.code[offset];
    match OpCode::from(instruction) {
        OpCode::CONST       => constant_op("CONST", &chunk, offset),
        OpCode::RETURN      => simple_op("RETURN", offset),
        OpCode::NEGATE      => simple_op("NEGATE", offset),
        OpCode::ADD         => simple_op("ADD", offset),
        OpCode::SUBSTRACT   => simple_op("SUBSTRACT", offset),
        OpCode::MULTIPLY    => simple_op("MULTIPLY", offset),
        OpCode::DIVIDE      => simple_op("DIVIDE", offset),
        OpCode::NONE        => simple_op("NONE", offset),
        OpCode::TRUE        => simple_op("TRUE", offset),
        OpCode::FALSE       => simple_op("FALSE", offset),
        OpCode::NOT         => simple_op("NOT", offset),
        OpCode::EQUAL       => simple_op("EQUAL", offset),
        OpCode::GREATER     => simple_op("GREATER", offset),
        OpCode::LESS        => simple_op("LESS", offset),
    }
}

fn simple_op(name: &str, offset: usize) -> usize {
    println!("{}", name);
    offset + 1
}

fn constant_op(name: &str, chunk: &Chunk, offset: usize) -> usize {
    let idx = chunk.code[offset + 1];
    print!("{:8} [{}] ", name, idx);
    print_value(&chunk.constants[idx]);
    println!();
    offset + 2
}
