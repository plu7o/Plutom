use core::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Colon,
    SemiColon,
    Slash,
    Star,
    Dollar,
    UnderScore,
    QuestionMark,
    Modulo,

    // One or two character tokens.
    Bang,
    BangEQ,
    Eq,
    EqEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Arm,
    PlusPlus,
    MinusMinus,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,

    // Literals.
    Ident,
    String,
    Integer,
    Float,

    // Keywords.
    Let,
    And,
    Or,
    False,
    True,
    If,
    Else,
    While,
    For,
    Fn,
    None,
    Echo,
    Return,
    Class,
    Super,
    This,
    Match,

    Err,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Loc {
    pub row: usize,
    pub col: usize,
    pub len: usize,
}

impl From<(usize, usize, usize)> for Loc {
    fn from(value: (usize, usize, usize)) -> Self {
        Self {
            row: value.0,
            col: value.1,
            len: value.2,
        }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub _type: TokenType,
    pub literal: &'a str,
    pub location: Loc,
}

impl<'a> Token<'a> {
    pub fn new(_type: TokenType, literal: &'a str, location: Loc) -> Self {
        Self {
            _type,
            literal,
            location,
        }
    }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            _type: TokenType::None,
            literal: "",
            location: Loc {
                row: 0,
                col: 0,
                len: 0,
            },
        }
    }
}
