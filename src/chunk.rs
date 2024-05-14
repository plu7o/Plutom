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
    NONE,
    TRUE,
    FALSE,
    NOT,
    EQUAL,
    GREATER,
    LESS,
    ECHO,
    POP,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Compare,
    Call,
}

impl Into<usize> for OpCode {
    fn into(self) -> usize {
        self as usize
    }
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
            7 => OpCode::NONE,
            8 => OpCode::TRUE,
            9 => OpCode::FALSE,
            10 => OpCode::NOT,
            11 => OpCode::EQUAL,
            12 => OpCode::GREATER,
            13 => OpCode::LESS,
            14 => OpCode::ECHO,
            15 => OpCode::POP,
            16 => OpCode::DefineGlobal,
            17 => OpCode::GetGlobal,
            18 => OpCode::SetGlobal,
            19 => OpCode::GetLocal,
            20 => OpCode::SetLocal,
            21 => OpCode::JumpIfFalse,
            22 => OpCode::Jump,
            23 => OpCode::Loop,
            24 => OpCode::Compare,
            25 => OpCode::Call,
            _ => panic!("Unkown OpCode"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
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
        let chunks: Vec<&str> = self.lines_rle.split(",").collect();
        let line = chunks[index];
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
        for test in 0..20 {
            match OpCode::from(test) {
                OpCode::RETURN => {
                    assert_eq!(test, 0)
                }
                OpCode::CONST => {
                    assert_eq!(test, 1)
                }
                OpCode::NEGATE => {
                    assert_eq!(test, 2)
                }
                OpCode::ADD => {
                    assert_eq!(test, 3)
                }
                OpCode::SUBSTRACT => {
                    assert_eq!(test, 4)
                }
                OpCode::MULTIPLY => {
                    assert_eq!(test, 5)
                }
                OpCode::DIVIDE => {
                    assert_eq!(test, 6)
                }
                OpCode::NONE => {
                    assert_eq!(test, 7)
                }
                OpCode::TRUE => {
                    assert_eq!(test, 8)
                }
                OpCode::FALSE => {
                    assert_eq!(test, 9)
                }
                OpCode::NOT => {
                    assert_eq!(test, 10)
                }
                OpCode::EQUAL => {
                    assert_eq!(test, 11)
                }
                OpCode::GREATER => {
                    assert_eq!(test, 12)
                }
                OpCode::LESS => {
                    assert_eq!(test, 13)
                }
                OpCode::ECHO => {
                    assert_eq!(test, 14)
                }
                OpCode::POP => {
                    assert_eq!(test, 15)
                }
                OpCode::DefineGlobal => {
                    assert_eq!(test, 16)
                }
                OpCode::GetGlobal => {
                    assert_eq!(test, 17)
                }
                OpCode::SetGlobal => {
                    assert_eq!(test, 18)
                }
                OpCode::GetLocal => {
                    assert_eq!(test, 19)
                }
                OpCode::SetLocal => {
                    assert_eq!(test, 20)
                }
                OpCode::JumpIfFalse => {
                    assert_eq!(test, 21)
                }
                OpCode::Jump => {
                    assert_eq!(test, 22)
                }
                OpCode::Loop => {
                    assert_eq!(test, 23)
                }
                OpCode::Compare => {
                    assert_eq!(test, 24)
                }
                OpCode::Call => {
                    assert_eq!(test, 25)
                }
            }
        }
    }
}
