use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    lexer::{Lexer, Token, TokenType},
    DEBUG_PRINT_CODE,
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    NONE,
    ASSIGNMENT, // =
    OR,         // or
    AND,        // and
    EQUALITY,   // == !=
    COMPARISON, // < > <= >=
    TERM,       // + -
    FACTOR,     // * /
    UNARY,      // ! -
    CALL,       // . ()
    PRIMARY,
}

enum ParseFn {
    Binary,
    Grouping,
    Unary,
    Number,
    String,
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

pub struct Parser<'a> {
    current: Token<'a>,
    prev: Token<'a>,
    lexer: Lexer<'a>,
    compile_chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn init(lexer: Lexer<'a>, chunk: &'a mut Chunk) -> Self {
        Self {
            current: Token::default(),
            prev: Token::default(),
            lexer,
            compile_chunk: chunk,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&mut self) -> bool {
        self.had_error = false;
        self.panic_mode = false;

        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");

        self.end_compiler();
        !self.had_error
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::ASSIGNMENT);
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        if let Some(prefix_rule) = self.get_rule(self.prev._type).prefix {
            self.parse(prefix_rule);
        } else {
            self.error("Expect expression.".to_string());
        }

        while precedence <= self.get_rule(self.current._type).precedence {
            self.advance();
            if let Some(infix_rule) = self.get_rule(self.prev._type).infix {
                self.parse(infix_rule);
            }
        }
    }
    fn parse(&mut self, func: ParseFn) {
        match func {
            ParseFn::Binary => self.binary(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Number => self.number(),
            ParseFn::String => self.string(),
        }
    }

    fn get_rule(&mut self, _type: TokenType) -> ParseRule {
        match _type {
            TokenType::LeftParen => ParseRule::new(Some(ParseFn::Grouping), None, Precedence::NONE),
            TokenType::RightParen => ParseRule::new(None, None, Precedence::NONE),
            TokenType::LeftBrace => ParseRule::new(None, None, Precedence::NONE),
            TokenType::RightBrace => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Comma => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Dot => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Minus => ParseRule::new(
                Some(ParseFn::Unary),
                Some(ParseFn::Binary),
                Precedence::TERM,
            ),
            TokenType::Plus => ParseRule::new(None, Some(ParseFn::Binary), Precedence::TERM),
            TokenType::Colon => ParseRule::new(None, None, Precedence::NONE),
            TokenType::SemiColon => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Slash => ParseRule::new(None, Some(ParseFn::Binary), Precedence::FACTOR),
            TokenType::Star => ParseRule::new(None, Some(ParseFn::Binary), Precedence::FACTOR),
            TokenType::Dollar => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Bang => ParseRule::new(None, None, Precedence::NONE),
            TokenType::BangEQ => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Eq => ParseRule::new(None, None, Precedence::NONE),
            TokenType::EqEq => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Gt => ParseRule::new(None, None, Precedence::NONE),
            TokenType::GtEq => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Lt => ParseRule::new(None, None, Precedence::NONE),
            TokenType::LtEq => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Ident => ParseRule::new(None, None, Precedence::NONE),
            TokenType::String => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Number => ParseRule::new(Some(ParseFn::Number), None, Precedence::NONE),
            TokenType::And => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Class => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Else => ParseRule::new(None, None, Precedence::NONE),
            TokenType::False => ParseRule::new(None, None, Precedence::NONE),
            TokenType::For => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Fn => ParseRule::new(None, None, Precedence::NONE),
            TokenType::If => ParseRule::new(None, None, Precedence::NONE),
            TokenType::None => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Or => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Echo => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Return => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Super => ParseRule::new(None, None, Precedence::NONE),
            TokenType::This => ParseRule::new(None, None, Precedence::NONE),
            TokenType::True => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Let => ParseRule::new(None, None, Precedence::NONE),
            TokenType::While => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Err => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Eof => ParseRule::new(None, None, Precedence::NONE),
        }
    }

    fn binary(&mut self) {
        let op_type = self.prev._type;
        let rule = self.get_rule(op_type);
        self.parse_precedence(rule.precedence);

        match op_type {
            TokenType::Plus => self.emit_byte(OpCode::ADD as usize),
            TokenType::Minus => self.emit_byte(OpCode::SUBSTRACT as usize),
            TokenType::Star => self.emit_byte(OpCode::MULTIPLY as usize),
            TokenType::Slash => self.emit_byte(OpCode::DIVIDE as usize),
            _ => (),
        }
    }

    fn unary(&mut self) {
        let op_type = self.prev._type;

        // Compile the operand
        self.parse_precedence(Precedence::UNARY);

        // Emiit the operator instruction
        match op_type {
            TokenType::Minus => self.emit_byte(OpCode::NEGATE as usize),
            _ => (),
        }
    }

    fn number(&mut self) {
        let value: f64 = self.prev.literal.parse().unwrap_or(0.0);
        self.emit_const(value);
    }

    fn string(&mut self) {
        todo!()
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn advance(&mut self) {
        self.prev = self.current;

        loop {
            self.current = self.lexer.scan_token();
            if self.current._type != TokenType::Err {
                return;
            }
            self.error_at_current(self.current.literal.to_string());
        }
    }

    fn consume(&mut self, _type: TokenType, msg: &str) {
        if self.current._type == _type {
            self.advance();
        } else {
            self.error_at_current(msg.to_string());
        }
    }

    fn emit_byte(&mut self, byte: usize) {
        self.compile_chunk.write(byte, self.prev.line);
    }

    fn emit_bytes(&mut self, byte_1: usize, byte_2: usize) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_const(&mut self, value: f64) {
        let value = self.make_const(value);
        self.emit_bytes(OpCode::CONST as usize, value);
    }

    fn make_const(&mut self, value: f64) -> usize {
        let constant = self.compile_chunk.add_const(value).clone();
        if constant > usize::MAX {
            self.error("Too many constans in one chunk.".to_string());
            return 0;
        }
        constant as usize
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::RETURN as usize);
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if DEBUG_PRINT_CODE {
            if !self.had_error {
                disassemble_chunk(self.compile_chunk, "code");
            }
        }
    }

    fn error_at_current(&mut self, msg: String) {
        let current = self.current;
        self.error_at(&current, &msg);
    }

    fn error(&mut self, msg: String) {
        let prev = self.prev;
        self.error_at(&prev, &msg);
    }

    fn error_at(&mut self, token: &Token<'a>, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        print!("[line {}] Error", token.line);
        match token._type {
            TokenType::Eof => print!(" at end"),
            TokenType::Err => (),
            _ => print!(" at {}", token.literal),
        }
        println!(": {}", msg);
        self.had_error = true;
    }
}
