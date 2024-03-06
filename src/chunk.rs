use core::panic;
use std::usize;

use crate::value::Value;

#[derive(Debug)]
#[repr(usize)]
pub enum OpCode {
    RETURN,
    CONST,
    NEGATE,
    ADD,
    SUBSTRACT,
    MULTIPLY,
    DIVIDE,
}

impl From<usize> for OpCode {
    fn from(value: usize) -> OpCode {
        match value {
            0 => OpCode::RETURN,
            1 => OpCode::CONST,
            2 => OpCode::NEGATE,
            3 => OpCode::ADD,
            4 => OpCode::SUBSTRACT,
            5 => OpCode::MULTIPLY,
            6 => OpCode::DIVIDE,
            _ => panic!("Unkown OpCode"),
        }
    }
}

pub struct Chunk {
    pub code: Vec<usize>,
    pub constants: Vec<Value>,
    lines_rle: String,
}

impl Chunk {
    pub fn init() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            lines_rle: String::new(),
        }
    }

    pub fn write(&mut self, byte: usize, line: usize) {
        self.code.push(byte);
        self.lines_rle
            .push_str(&format!("{}:{},", self.code.len() - 1, line));
    }

    pub fn add_const(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn get_line(&self, index: usize) -> usize {
        let chunk: Vec<&str> = self.lines_rle.split(",").collect();
        let line = chunk[index];
        let line_num: usize = line[line.len() - 1..]
            .parse()
            .expect("Wrong REF formatting");
        line_num
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_opcode_from_bytes() {
        let tests = vec![0, 1, 2];
        for test in tests.iter() {
            match OpCode::from(*test) {
                OpCode::RETURN => {
                    assert_eq!(*test, 0)
                }
                OpCode::CONST => {
                    assert_eq!(*test, 1)
                }
                OpCode::NEGATE => {
                    assert_eq!(*test, 2)
                }
                OpCode::ADD => {
                    assert_eq!(*test, 3)
                }
                OpCode::SUBSTRACT => {
                    assert_eq!(*test, 4)
                }
                OpCode::MULTIPLY => {
                    assert_eq!(*test, 5)
                }
                OpCode::DIVIDE => {
                    assert_eq!(*test, 6)
                }
            }
        }
    }
}
