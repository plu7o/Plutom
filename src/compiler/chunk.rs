use core::fmt;

use crate::{lexer::token::Loc, value::Value};

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
    LoadGlobal,
    SetGlobal,
    LoadLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Compare,
    Call,
    Closure,
    List,
    Map,
    GetIndex,
    SetIndex,
    Inc,
    Dec,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    Enum,
    Dup,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpCode::RETURN => write!(f, "RETURN"),
            OpCode::CONST => write!(f, "CONST"),
            OpCode::NEGATE => write!(f, "NEGATE"),
            OpCode::ADD => write!(f, "ADD"),
            OpCode::SUBSTRACT => write!(f, "SUB"),
            OpCode::MULTIPLY => write!(f, "MUL"),
            OpCode::DIVIDE => write!(f, "DIV"),
            OpCode::NONE => write!(f, "NONE"),
            OpCode::TRUE => write!(f, "TRUE"),
            OpCode::FALSE => write!(f, "FALSE"),
            OpCode::NOT => write!(f, "NOT"),
            OpCode::EQUAL => write!(f, "EQ"),
            OpCode::GREATER => write!(f, "GT"),
            OpCode::LESS => write!(f, "LT"),
            OpCode::ECHO => write!(f, "ECHO"),
            OpCode::POP => write!(f, "POP"),
            OpCode::DefineGlobal => write!(f, "DEF_GLOBAL"),
            OpCode::LoadGlobal => write!(f, "LOAD_GLOBAL"),
            OpCode::SetGlobal => write!(f, "SET_GLOBAL"),
            OpCode::LoadLocal => write!(f, "LOAD_LOCAL"),
            OpCode::SetLocal => write!(f, "SET_LOCAL"),
            OpCode::JumpIfFalse => write!(f, "JUMP_IF_FALSE"),
            OpCode::Jump => write!(f, "JUMP"),
            OpCode::Loop => write!(f, "LOOP"),
            OpCode::Compare => write!(f, "CMP"),
            OpCode::Call => write!(f, "CALL"),
            OpCode::Closure => write!(f, "CLOSURE"),
            OpCode::List => write!(f, "LIST"),
            OpCode::Map => write!(f, "MAP"),
            OpCode::GetIndex => write!(f, "LOAD_IDX"),
            OpCode::SetIndex => write!(f, "SET_IDX"),
            OpCode::Inc => write!(f, "INC"),
            OpCode::Dec => write!(f, "DEC"),
            OpCode::PreInc => write!(f, "PRE_INC"),
            OpCode::PreDec => write!(f, "PRE_DEC"),
            OpCode::PostInc => write!(f, "POST_INC"),
            OpCode::PostDec => write!(f, "POST_DEC"),
            OpCode::Enum => write!(f, "ENUM"),
            OpCode::Dup => write!(f, "DUP"),
        }
    }
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
            17 => OpCode::LoadGlobal,
            18 => OpCode::SetGlobal,
            19 => OpCode::LoadLocal,
            20 => OpCode::SetLocal,
            21 => OpCode::JumpIfFalse,
            22 => OpCode::Jump,
            23 => OpCode::Loop,
            24 => OpCode::Compare,
            25 => OpCode::Call,
            26 => OpCode::Closure,
            27 => OpCode::List,
            28 => OpCode::Map,
            29 => OpCode::GetIndex,
            30 => OpCode::SetIndex,
            31 => OpCode::Inc,
            32 => OpCode::Dec,
            33 => OpCode::PreInc,
            34 => OpCode::PreDec,
            35 => OpCode::PostInc,
            36 => OpCode::PostDec,
            37 => OpCode::Enum,
            38 => OpCode::Dup,
            _ => panic!("Unkown OpCode"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Chunk {
    pub code: Vec<usize>,
    pub constants: Vec<Value>,
    location_rle: String,
}

impl Chunk {
    pub fn init() -> Self {
        Self {
            code: vec![],
            constants: vec![],
            location_rle: String::new(),
        }
    }

    pub fn write(&mut self, byte: usize, location: Loc) {
        self.code.push(byte);
        self.rle_encode(location);
    }

    pub fn add_const(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn rle_encode(&mut self, location: Loc) {
        let mut encoded = String::new();
        let format = format!("{}:{}:{}", location.row, location.col, location.len);
        let seperator = ",";
        let entry: Vec<&str> = self.location_rle.split_terminator(",").collect();
        let count: usize = 1;
        let mut found = false;
        for e in entry.iter() {
            let block: Vec<&str> = e.split_terminator(":").collect();
            let mut count: usize = block[0].parse().expect("Expected count as a number");
            let row: usize = block[1].parse().expect("Expected row as a number");
            let col: usize = block[2].parse().expect("Expected col as a number");
            if row == location.row && col == location.col {
                count += 1;
                found = true;
                encoded.push_str(&format!("{}:{}{}", count, format, seperator));
            } else {
                encoded.push_str(&format!("{}{}", e, seperator));
            }
        }
        if !found {
            encoded.push_str(&format!("{}:{}{}", count, format, seperator));
        }
        self.location_rle = encoded;
    }

    pub fn rle_decode(&self) -> Vec<(usize, usize, usize)> {
        let mut decoded: Vec<(usize, usize, usize)> = Vec::new();
        let entry: Vec<&str> = self.location_rle.split_terminator(",").collect();
        for e in entry.iter() {
            let block: Vec<&str> = e.split_terminator(":").collect();
            let count: usize = block[0].parse().expect("Expected count as a number");
            let row: usize = block[1].parse().expect("Expected row as a number");
            let col: usize = block[2].parse().expect("Expected col as a number");
            let len: usize = block[3].parse().expect("Expected len as a number");
            for _ in 0..count {
                decoded.push((row, col, len));
            }
        }
        decoded
    }

    pub fn get_location(&self, index: usize) -> Loc {
        Loc::from(self.rle_decode()[index])
    }
}
