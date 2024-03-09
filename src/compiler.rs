use crate::{
    chunk::{Chunk, OpCode},
    debug::disassemble_chunk,
    lexer::{Lexer, Token, TokenType},
    object::{ObjString, ObjType},
    value::{Value, ValueType},
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
    Variable,
    And,
    Or,
    Binary,
    Grouping,
    Unary,
    Number,
    String,
    Literal,
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

#[derive(Debug, Clone)]
pub struct Local<'a> {
    pub name: Token<'a>,
    pub depth: isize,
}

#[derive(Debug, Clone)]
pub struct Compiler<'a> {
    locals: Vec<Local<'a>>,
    local_count: usize,
    scope_depth: isize,
}

impl Compiler<'_> {
    pub fn init() -> Self {
        Self {
            local_count: 0,
            scope_depth: 0,
            locals: Vec::new(),
        }
    }
}

pub struct Parser<'a> {
    current: Token<'a>,
    prev: Token<'a>,
    lexer: Lexer<'a>,
    compiler: Compiler<'a>,
    compile_chunk: &'a mut Chunk,
    had_error: bool,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn init(lexer: Lexer<'a>, compiler: Compiler<'a>, chunk: &'a mut Chunk) -> Self {
        Self {
            current: Token::default(),
            prev: Token::default(),
            lexer,
            compiler,
            compile_chunk: chunk,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&mut self) -> bool {
        self.had_error = false;
        self.panic_mode = false;

        self.advance();

        while !self.match_token(TokenType::Eof) {
            self.declaration();
        }

        self.end_compiler();
        !self.had_error
    }

    // -----------------------------------------------------------
    //                          Parsing
    // -----------------------------------------------------------

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let can_assign = precedence <= Precedence::ASSIGNMENT;

        if let Some(prefix_rule) = self.get_rule(self.prev._type).prefix {
            self.parse(prefix_rule, can_assign);
        } else {
            self.error("Expect expression.".to_string());
        }

        while precedence <= self.get_rule(self.current._type).precedence {
            self.advance();
            if let Some(infix_rule) = self.get_rule(self.prev._type).infix {
                self.parse(infix_rule, can_assign);
            }
        }

        if can_assign && self.match_token(TokenType::Eq) {
            self.error("Invalid assignment target".to_string());
        }
    }

    fn parse(&mut self, rule: ParseFn, can_assign: bool) {
        match rule {
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::And => self.variable(can_assign),
            ParseFn::Binary => self.binary(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Number => self.number(),
            ParseFn::String => self.string(),
            ParseFn::Literal => self.literal(),
        }
    }

    fn parse_variable(&mut self, msg: &str) -> usize {
        self.consume(TokenType::Ident, msg);

        match self.declare_var() {
            Err(msg) => self.error(msg),
            Ok(()) => (),
        };

        if self.compiler.scope_depth > 0 {
            return 0;
        }
        self.make_ident_const(self.prev)
    }

    fn define_var(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as usize, global);
    }

    fn declare_var(&mut self) -> Result<(), String> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }
        let name = &self.prev;
        let scopes = &self.compiler.locals[..self.compiler.local_count];
        for local in scopes.iter().rev() {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }
            if *name == local.name {
                return Err("Already a variable with this name in this scope".to_string());
            }
        }
        self.add_local(*name);
        Ok(())
    }

    fn mark_initialized(&mut self) {
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.compiler.local_count == usize::MAX {
            self.error("Too many local variables in function.".to_string());
            return;
        }
        self.compiler.locals.push(Local {
            name,
            depth: self.compiler.local_count as isize,
        });
        self.compiler.local_count += 1;
    }

    //#[rustfmt::skip]
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
            TokenType::Bang => ParseRule::new(Some(ParseFn::Unary), None, Precedence::NONE),
            TokenType::BangEQ => ParseRule::new(None, Some(ParseFn::Binary), Precedence::EQUALITY),
            TokenType::Eq => ParseRule::new(None, None, Precedence::ASSIGNMENT),
            TokenType::EqEq => ParseRule::new(None, Some(ParseFn::Binary), Precedence::EQUALITY),
            TokenType::Gt => ParseRule::new(None, Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::GtEq => ParseRule::new(None, Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::Lt => ParseRule::new(None, Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::LtEq => ParseRule::new(None, Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::Ident => ParseRule::new(Some(ParseFn::Variable), None, Precedence::NONE),
            TokenType::String => ParseRule::new(Some(ParseFn::String), None, Precedence::NONE),
            TokenType::Number => ParseRule::new(Some(ParseFn::Number), None, Precedence::NONE),
            TokenType::And => ParseRule::new(None, Some(ParseFn::And), Precedence::AND),
            TokenType::Class => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Else => ParseRule::new(None, None, Precedence::NONE),
            TokenType::False => ParseRule::new(Some(ParseFn::Literal), None, Precedence::NONE),
            TokenType::For => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Fn => ParseRule::new(None, None, Precedence::CALL),
            TokenType::If => ParseRule::new(None, None, Precedence::NONE),
            TokenType::None => ParseRule::new(Some(ParseFn::Literal), None, Precedence::NONE),
            TokenType::Or => ParseRule::new(None, Some(ParseFn::Or), Precedence::OR),
            TokenType::Echo => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Return => ParseRule::new(None, None, Precedence::PRIMARY),
            TokenType::Super => ParseRule::new(None, None, Precedence::NONE),
            TokenType::This => ParseRule::new(None, None, Precedence::NONE),
            TokenType::True => ParseRule::new(Some(ParseFn::Literal), None, Precedence::NONE),
            TokenType::Let => ParseRule::new(None, None, Precedence::NONE),
            TokenType::While => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Err => ParseRule::new(None, None, Precedence::NONE),
            TokenType::Eof => ParseRule::new(None, None, Precedence::NONE),
        }
    }

    fn declaration(&mut self) {
        if self.match_token(TokenType::Let) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.sync();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block")
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Echo) {
            self.echo_stmt();
        } else if self.match_token(TokenType::If) {
            self.if_stmt();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_stmt()
        }
    }

    fn if_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::Colon, "Expected ':' after condition");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse as usize);
        self.emit_byte(OpCode::POP as usize);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump as usize);
        self.patch_jump(then_jump);
        self.emit_byte(OpCode::POP as usize);

        if self.match_token(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn echo_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expected ';' after value");
        self.emit_byte(OpCode::ECHO as usize);
    }

    fn expression_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expected ';' after expression");
        self.emit_byte(OpCode::POP as usize);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name");
        if self.match_token(TokenType::Eq) {
            self.expression();
        } else {
            self.emit_byte(OpCode::NONE as usize);
        }
        self.consume(
            TokenType::SemiColon,
            "Expected ';' after variable definition",
        );
        self.define_var(global);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::ASSIGNMENT);
    }

    fn and(&mut self) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse as usize);
        self.emit_byte(OpCode::POP as usize);
        self.parse_precedence(Precedence::AND);
        self.patch_jump(end_jump);
    }
    fn or(&mut self) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse as usize);
        let end_jump = self.emit_jump(OpCode::Jump as usize);

        self.patch_jump(else_jump);
        self.emit_byte(OpCode::POP as usize);

        self.parse_precedence(Precedence::OR);
        self.patch_jump(end_jump)
    }

    fn binary(&mut self) {
        let op_type = self.prev._type;
        let rule = self.get_rule(op_type);
        self.parse_precedence(rule.precedence);

        match op_type {
            TokenType::BangEQ => self.emit_bytes(OpCode::EQUAL as usize, OpCode::NOT as usize),
            TokenType::EqEq => self.emit_byte(OpCode::EQUAL as usize),
            TokenType::Gt => self.emit_byte(OpCode::GREATER as usize),
            TokenType::GtEq => self.emit_bytes(OpCode::LESS as usize, OpCode::NOT as usize),
            TokenType::Lt => self.emit_byte(OpCode::LESS as usize),
            TokenType::LtEq => self.emit_bytes(OpCode::GREATER as usize, OpCode::NOT as usize),
            TokenType::Plus => self.emit_byte(OpCode::ADD as usize),
            TokenType::Minus => self.emit_byte(OpCode::SUBSTRACT as usize),
            TokenType::Star => self.emit_byte(OpCode::MULTIPLY as usize),
            TokenType::Slash => self.emit_byte(OpCode::DIVIDE as usize),
            _ => (),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let op_type = self.prev._type;
        self.parse_precedence(Precedence::UNARY);
        match op_type {
            TokenType::Minus => self.emit_byte(OpCode::NEGATE as usize),
            TokenType::Bang => self.emit_byte(OpCode::NOT as usize),
            _ => (),
        }
    }

    fn number(&mut self) {
        let value: f64 = self.prev.literal.parse().unwrap_or(0.0);
        self.emit_const(Value::new(ValueType::Number(value)));
    }

    fn string(&mut self) {
        self.emit_const(Value::obj_val(ObjType::String(ObjString {
            chars: self
                .prev
                .literal
                .trim_start_matches("\"")
                .trim_end_matches("\"")
                .to_string()
                .clone(),
            len: self.prev.literal.len().clone(),
        })));
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.prev, can_assign);
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) {
        let get_op: OpCode;
        let set_op: OpCode;

        let arg = match self.resolve_local(&name) {
            Some(arg) => {
                get_op = OpCode::GetLocal;
                set_op = OpCode::SetLocal;
                arg
            }
            None => {
                get_op = OpCode::GetGlobal;
                set_op = OpCode::SetGlobal;
                self.make_ident_const(name)
            }
        };

        if can_assign && self.match_token(TokenType::Eq) {
            self.expression();
            self.emit_bytes(set_op as usize, arg);
        } else {
            self.emit_bytes(get_op as usize, arg);
        }
    }

    fn resolve_local(&mut self, name: &Token) -> Option<usize> {
        let scopes = &self.compiler.locals[..self.compiler.local_count];
        for (i, local) in scopes.iter().enumerate().rev() {
            if name == &local.name {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer".to_string());
                }
                return Some(i);
            }
        }
        None
    }

    fn literal(&mut self) {
        match self.prev._type {
            TokenType::False => self.emit_byte(OpCode::FALSE as usize),
            TokenType::True => self.emit_byte(OpCode::TRUE as usize),
            TokenType::None => self.emit_byte(OpCode::NONE as usize),
            _ => (),
        };
    }

    // -----------------------------------------------------------
    //                      Compiler Internals
    // -----------------------------------------------------------

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

    fn match_token(&mut self, _type: TokenType) -> bool {
        if !self.check(_type) {
            return false;
        }
        self.advance();
        true
    }

    fn check(&self, _type: TokenType) -> bool {
        self.current._type == _type
    }

    fn sync(&mut self) {
        self.panic_mode = false;

        while self.current._type != TokenType::Eof {
            if self.prev._type == TokenType::SemiColon {
                return;
            }

            match self.current._type {
                TokenType::Class
                | TokenType::Fn
                | TokenType::Let
                | TokenType::For
                | TokenType::While
                | TokenType::Echo
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;
        while self.compiler.local_count > 0
            && self.compiler.locals[self.compiler.local_count - 1].depth > self.compiler.scope_depth
        {
            self.emit_byte(OpCode::POP as usize);
            self.compiler.local_count -= 1;
        }
    }

    fn end_compiler(&mut self) {
        self.emit_return();
        if DEBUG_PRINT_CODE {
            if !self.had_error {
                disassemble_chunk(self.compile_chunk, "code");
            }
        }
    }

    // -----------------------------------------------------------
    //                      Compiling Chunks
    // -----------------------------------------------------------

    fn make_ident_const(&mut self, name: Token) -> usize {
        self.make_const(Value::obj_val(ObjType::String(ObjString {
            len: name.literal.len().clone(),
            chars: name
                .literal
                .trim_start_matches("\"")
                .trim_end_matches("\"")
                .to_string()
                .clone(),
        })))
    }

    fn make_const(&mut self, value: Value) -> usize {
        let constant = self.compile_chunk.add_const(value).clone();
        if constant > usize::MAX {
            self.error("Too many constans in one chunk.".to_string());
            return 0;
        }
        constant as usize
    }

    fn emit_byte(&mut self, byte: usize) {
        self.compile_chunk.write(byte, self.prev.line);
    }

    fn emit_bytes(&mut self, byte_1: usize, byte_2: usize) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_jump(&mut self, instruction: usize) -> isize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        (self.compile_chunk.code.len() - 2) as isize
    }

    fn patch_jump(&mut self, offset: isize) {
        let jump = self.compile_chunk.code.len() - offset as usize - 2;
        self.compile_chunk.code[offset as usize] = (jump >> 8) & 0xff;
        self.compile_chunk.code[offset as usize + 1] = jump & 0xff;
    }

    fn emit_const(&mut self, value: Value) {
        let value = self.make_const(value);
        self.emit_bytes(OpCode::CONST as usize, value);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::RETURN as usize);
    }

    // -----------------------------------------------------------
    //                          Errors
    // -----------------------------------------------------------

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
