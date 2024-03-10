use std::{char, usize};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Colon,
    SemiColon,
    Slash,
    Star,
    Dollar,

    // One or two character tokens.
    Bang,
    BangEQ,
    Eq,
    EqEq,
    Gt,
    GtEq,
    Lt,
    LtEq,

    // Literals.
    Ident,
    String,
    Number,

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

    Err,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub _type: TokenType,
    pub literal: &'a str,
    pub line: usize,
}

impl<'a> Token<'a> {
    pub fn new(_type: TokenType, literal: &'a str, line: usize) -> Self {
        Self {
            _type,
            literal,
            line,
        }
    }
}

impl Default for Token<'_> {
    fn default() -> Self {
        Self {
            _type: TokenType::None,
            literal: "",
            line: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn init(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if c.is_alphabetic() {
            return self.identifier();
        }

        if c.is_digit(10) {
            return self.number();
        }

        match c {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ':' => self.make_token(TokenType::Colon),
            ';' => self.make_token(TokenType::SemiColon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '$' => self.make_token(TokenType::Dollar),
            '"' => self.string(),
            '!' => {
                let token = if self.match_token('=') {
                    TokenType::BangEQ
                } else {
                    TokenType::Bang
                };
                self.make_token(token)
            }
            '=' => {
                let token = if self.match_token('=') {
                    TokenType::EqEq
                } else {
                    TokenType::Eq
                };
                self.make_token(token)
            }
            '<' => {
                let token = if self.match_token('=') {
                    TokenType::LtEq
                } else {
                    TokenType::Lt
                };
                self.make_token(token)
            }
            '>' => {
                let token = if self.match_token('=') {
                    TokenType::GtEq
                } else {
                    TokenType::Gt
                };
                self.make_token(token)
            }
            _ => self.error_token("Unexpected character."),
        }
    }

    fn get(&self, index: usize) -> char {
        self.source.chars().nth(index).unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        let next = self.get(self.current);
        self.current += 1;
        next
    }

    fn peek(&mut self) -> char {
        self.get(self.current)
    }

    fn peek_next(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.get(self.current + 1)
    }

    fn match_token(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.get(self.current) != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn is_at_end(&self) -> bool {
        self.get(self.current) == '\0'
    }

    fn make_token(&self, token_type: TokenType) -> Token<'a> {
        Token::new(
            token_type,
            &self.source[self.start..self.current],
            self.line.clone(),
        )
    }

    fn error_token(&self, msg: &'a str) -> Token<'a> {
        Token::new(TokenType::Err, msg, self.line)
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();
            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_at_end() {
                            self.advance();
                        }
                    }
                }
                _ => return,
            };
        }
    }

    fn string(&mut self) -> Token<'a> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance();
        self.make_token(TokenType::String)
    }

    fn number(&mut self) -> Token<'a> {
        while self.peek().is_digit(10) {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'a> {
        while self.peek().is_alphabetic() || self.peek().is_digit(10) {
            self.advance();
        }
        self.make_token(self.ident_type())
    }

    fn ident_type(&self) -> TokenType {
        match self.get(self.start) {
            'a' => return self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => return self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => {
                if self.source[self.start..self.current].len() > 1 {
                    match self.get(self.start + 1) {
                        'l' => return self.check_keyword(2, 2, "se", TokenType::Else),
                        'c' => return self.check_keyword(2, 2, "ho", TokenType::Echo),
                        _ => (),
                    }
                }
            }
            'f' => {
                if self.source[self.start..self.current].len() > 1 {
                    match self.get(self.start + 1) {
                        'a' => return self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => return self.check_keyword(2, 1, "r", TokenType::For),
                        'n' => return self.check_keyword(1, 1, "n", TokenType::Fn),
                        _ => (),
                    }
                }
            }
            'i' => return self.check_keyword(1, 1, "f", TokenType::If),
            'l' => return self.check_keyword(1, 2, "et", TokenType::Let),
            'n' => return self.check_keyword(1, 3, "one", TokenType::None),
            'o' => return self.check_keyword(1, 1, "r", TokenType::Or),
            'r' => return self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => return self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.source[self.start..self.current].len() > 1 {
                    match self.get(self.start + 1) {
                        'h' => return self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => return self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => (),
                    }
                }
            }
            'w' => return self.check_keyword(1, 4, "hile", TokenType::While),
            _ => return TokenType::Ident,
        };
        TokenType::Ident
    }

    fn check_keyword(&self, start: usize, len: usize, rest: &str, _type: TokenType) -> TokenType {
        let ident = &self.source[self.start..self.current];
        if ident.len() == start + len && &ident[start..] == rest {
            return _type;
        }
        TokenType::Ident
    }
}
