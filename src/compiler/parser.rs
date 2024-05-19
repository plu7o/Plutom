use crate::{
    error,
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
    objects::object::{ObjFunction, ObjString},
    value::Value,
};

use super::{
    chunk::OpCode,
    compiler::{Compiler, FunctionType, Local},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    NONE,
    ASSIGNMENT, // = += -= *= /=
    TERNARY,    // ?
    OR,         // or
    AND,        // and
    EQUALITY,   // == !=
    COMPARISON, // < > <= >=
    TERM,       // + -
    FACTOR,     // * / %
    UNARY,      // ! - -- ++
    CALL,       // . () []
    PRIMARY,
}

#[derive(Debug)]
enum ParseFn {
    Variable,
    Ternary,
    And,
    Or,
    Binary,
    Grouping,
    Prefix,
    Postfix,
    Number,
    String,
    Literal,
    Index,
    Call,
}

#[derive(Debug)]
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
    pub had_error: bool,
    pub panic_mode: bool,
    compiler: Box<Compiler<'a>>,
    source: &'a str,
}

impl<'a> Parser<'a> {
    pub fn init(source: &'a str, lexer: Lexer<'a>, compiler: Box<Compiler<'a>>) -> Self {
        Self {
            current: Token::default(),
            prev: Token::default(),
            had_error: false,
            panic_mode: false,
            lexer,
            compiler,
            source,
        }
    }

    // -----------------------------------------------------------
    //                          Parsing
    // -----------------------------------------------------------

    fn parse(&mut self, rule: ParseFn, can_assign: bool) {
        match rule {
            ParseFn::Variable => self.variable(can_assign),
            ParseFn::Ternary => self.ternary(),
            ParseFn::Or => self.or(),
            ParseFn::And => self.and(),
            ParseFn::Binary => self.binary(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Index => self.index(can_assign),
            ParseFn::Prefix => self.prefix(),
            ParseFn::Postfix => self.postfix(),
            ParseFn::Call => self.call(can_assign),
            ParseFn::Number => self.number(),
            ParseFn::String => self.string(),
            ParseFn::Literal => self.literal(),
        }
    }

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

    #[rustfmt::skip]
    fn get_rule(&mut self, _type: TokenType) -> ParseRule {
        match _type {
            TokenType::Eq           => ParseRule::new(None,                    None,                  Precedence::ASSIGNMENT),
            TokenType::PlusEq       => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::ASSIGNMENT),
            TokenType::MinusEq      => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::ASSIGNMENT),
            TokenType::SlashEq      => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::ASSIGNMENT),
            TokenType::StarEq       => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::ASSIGNMENT),
            TokenType::QuestionMark => ParseRule::new(None,                    Some(ParseFn::Ternary),Precedence::TERNARY),
            TokenType::Or           => ParseRule::new(None,                    Some(ParseFn::Or),     Precedence::OR),
            TokenType::And          => ParseRule::new(None,                    Some(ParseFn::And),    Precedence::AND),
            TokenType::BangEQ       => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::EQUALITY),
            TokenType::EqEq         => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::EQUALITY),
            TokenType::Gt           => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::GtEq         => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::Lt           => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::LtEq         => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::COMPARISON),
            TokenType::Plus         => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::TERM),
            TokenType::PlusPlus     => ParseRule::new(Some(ParseFn::Prefix),   Some(ParseFn::Postfix),Precedence::UNARY),
            TokenType::Minus        => ParseRule::new(Some(ParseFn::Prefix),   Some(ParseFn::Binary), Precedence::TERM),
            TokenType::MinusMinus   => ParseRule::new(Some(ParseFn::Prefix),   Some(ParseFn::Postfix),Precedence::UNARY),
            TokenType::Star         => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::FACTOR),
            TokenType::Slash        => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::FACTOR),
            TokenType::Modulo       => ParseRule::new(None,                    Some(ParseFn::Binary), Precedence::FACTOR),
            TokenType::Fn           => ParseRule::new(None,                    None,                  Precedence::CALL),
            TokenType::LeftParen    => ParseRule::new(Some(ParseFn::Grouping), Some(ParseFn::Call),   Precedence::CALL),
            TokenType::LeftBrace    => ParseRule::new(Some(ParseFn::Literal),  Some(ParseFn::Index),  Precedence::CALL),
            TokenType::LeftBracket  => ParseRule::new(Some(ParseFn::Literal),  Some(ParseFn::Index),  Precedence::CALL),
            TokenType::Return       => ParseRule::new(None,                    None,                  Precedence::PRIMARY),
            TokenType::Bang         => ParseRule::new(Some(ParseFn::Prefix),   None,                  Precedence::NONE),
            TokenType::Integer      => ParseRule::new(Some(ParseFn::Number),   None,                  Precedence::NONE),
            TokenType::Float        => ParseRule::new(Some(ParseFn::Number),   None,                  Precedence::NONE),
            TokenType::String       => ParseRule::new(Some(ParseFn::String),   None,                  Precedence::NONE),
            TokenType::Ident        => ParseRule::new(Some(ParseFn::Variable), None,                  Precedence::NONE),
            TokenType::False        => ParseRule::new(Some(ParseFn::Literal),  None,                  Precedence::NONE),
            TokenType::True         => ParseRule::new(Some(ParseFn::Literal),  None,                  Precedence::NONE),
            TokenType::None         => ParseRule::new(Some(ParseFn::Literal),  None,                  Precedence::NONE),
            TokenType::Echo         => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Let          => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Class        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Dot          => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::This         => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Super        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::For          => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::While        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Else         => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::If           => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Match        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::UnderScore   => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Arm          => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::RightParen   => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::RightBrace   => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::RightBracket => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Colon        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Comma        => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Dollar       => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::SemiColon    => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Err          => ParseRule::new(None,                    None,                  Precedence::NONE),
            TokenType::Eof          => ParseRule::new(None,                    None,                  Precedence::NONE),
        }
    }

    fn statement(&mut self) {
        if self.match_token(TokenType::Echo) {
            self.echo_stmt();
        } else if self.match_token(TokenType::If) {
            self.if_stmt();
        } else if self.match_token(TokenType::Return) {
            self.return_stmt();
        } else if self.match_token(TokenType::Match) {
            self.match_stmt();
        } else if self.match_token(TokenType::While) {
            self.while_stmt();
        } else if self.match_token(TokenType::For) {
            self.for_stmt();
        } else if self.match_token(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_stmt()
        }
    }

    pub fn declaration(&mut self) {
        if self.match_token(TokenType::Fn) {
            self.func_declaration();
        } else if self.match_token(TokenType::Let) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.sync();
        }
    }

    fn expression_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expected ';' after expression");
        self.emit_byte(OpCode::POP as usize);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::ASSIGNMENT);
    }

    // -----------------------------------------------------------
    //                          Statements
    // -----------------------------------------------------------

    fn echo_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::SemiColon, "Expected ';' after value");
        self.emit_byte(OpCode::ECHO as usize);
    }

    // -----------------------------------------------------------
    //                          Variables
    // -----------------------------------------------------------

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

    fn parse_variable(&mut self, msg: &str) -> usize {
        self.consume(TokenType::Ident, msg);
        match self.declare_var() {
            Err(msg) => self.error(msg),
            Ok(()) => (),
        };
        if self.compiler.scope_depth > 0 {
            return 0;
        }
        let previous = self.prev;
        self.make_ident_const(&previous)
    }

    fn variable(&mut self, can_assign: bool) {
        let mut _previous = self.prev;
        self.named_variable(&_previous, can_assign);
    }

    fn named_variable(&mut self, name: &Token, can_assign: bool) {
        let get_op: OpCode;
        let set_op: OpCode;

        let arg = match self.resolve_local(&name) {
            Some(arg) => {
                get_op = OpCode::LoadLocal;
                set_op = OpCode::SetLocal;
                arg
            }
            None => {
                get_op = OpCode::LoadGlobal;
                set_op = OpCode::SetGlobal;
                self.make_ident_const(&name)
            }
        };

        if can_assign && self.match_token(TokenType::Eq) {
            self.expression();
            self.emit_bytes(set_op as usize, arg);
        } else {
            self.emit_bytes(get_op as usize, arg);
        }
    }

    fn declare_var(&mut self) -> Result<(), String> {
        if self.compiler.scope_depth == 0 {
            return Ok(());
        }
        let name = &self.prev;
        for local in self.compiler.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.compiler.scope_depth {
                break;
            }
            if self.identifiers_equal(name, &local.name) {
                return Err("Already a variable with this name in this scope".to_string());
            }
        }
        self.add_local(*name);
        Ok(())
    }

    fn define_var(&mut self, global: usize) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
        self.emit_bytes(OpCode::DefineGlobal as usize, global);
    }

    fn add_local(&mut self, name: Token<'a>) {
        if self.compiler.local_count == usize::MAX {
            self.error("Too many local variables in function.".to_string());
            return;
        }
        self.compiler.locals.push(Local { name, depth: -1 });
        self.compiler.local_count += 1;
    }

    fn resolve_local(&mut self, name: &Token) -> Option<usize> {
        for (i, local) in self.compiler.locals.iter().enumerate().rev() {
            if self.identifiers_equal(name, &local.name) {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer".to_string());
                }
                return Some(i);
            }
        }
        None
    }

    fn mark_initialized(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }
        self.compiler.locals[self.compiler.local_count - 1].depth = self.compiler.scope_depth;
    }

    fn identifiers_equal(&self, a: &Token, b: &Token) -> bool {
        if a.literal.len() != b.literal.len() {
            return false;
        }

        a.literal == b.literal && a.literal.len() == b.literal.len()
    }

    // -----------------------------------------------------------
    //                          Functions
    // -----------------------------------------------------------

    fn func_declaration(&mut self) {
        let global = self.parse_variable("Expect function name");
        self.mark_initialized();
        self.function(FunctionType::FunctionBody);
        self.define_var(global);
    }

    fn function(&mut self, func_type: FunctionType) {
        let enclosing = &mut self.compiler;
        self.compiler = Box::new(Compiler::init(Some(enclosing.clone()), func_type.clone()));

        match func_type {
            FunctionType::Script => (),
            _ => self.compiler.function.name = Some(ObjString::new(self.prev.literal.to_string())),
        }

        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name");
        if !self.check(TokenType::RightParen) {
            loop {
                self.compiler.function.arity += 1;
                if self.compiler.function.arity > 255 {
                    self.error_at_current("Can't have more than 255 parameters.".to_string());
                }
                let constant = self.parse_variable("Expect parameter name.");
                self.define_var(constant);

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(
            TokenType::RightParen,
            "Expect ')' after function paramaters",
        );
        self.consume(TokenType::LeftBrace, "Expect '{' before funtion body");
        self.block();

        let function = self.end_compiler();
        let value = self.make_const(Value::function(function));

        self.emit_bytes(OpCode::Closure as usize, value);
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.arguments_list();
        self.emit_bytes(OpCode::Call as usize, arg_count);
    }

    fn arguments_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count >= 255 {
                    self.error("Can't have more than 255 arguments.".to_string());
                }
                arg_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn return_stmt(&mut self) {
        match self.compiler._type {
            FunctionType::Script => self.error("Can't return from top-level code.".to_string()),
            _ => (),
        }

        if self.match_token(TokenType::SemiColon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::SemiColon, "Expected ';' after return value.");
            self.emit_byte(OpCode::RETURN as usize);
        }
    }

    // -----------------------------------------------------------
    //                      Control Flow
    // -----------------------------------------------------------

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

    fn match_stmt(&mut self) {
        self.expression();
        self.consume(TokenType::LeftBrace, "Expect '{' after match expression");

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            if !self.match_token(TokenType::UnderScore) {
                // case
                self.expression();
                self.emit_byte(OpCode::Compare as usize);
                let jump_index = self.emit_jump(OpCode::JumpIfFalse as usize);
                self.consume(TokenType::Arm, "Expect '=>' after case expression");
                self.statement();
                self.patch_jump(jump_index);
                self.emit_byte(OpCode::POP as usize);
            } else {
                // default
                self.consume(TokenType::Arm, "Expect '=>' after case expression");
                self.statement();
            }
        }
        self.consume(TokenType::RightBrace, "Expect '}' after match body");
    }

    // -----------------------------------------------------------
    //                          Loops
    // -----------------------------------------------------------

    fn while_stmt(&mut self) {
        let loop_start = self.compiler.current_chunk().code.len();
        self.expression();
        self.consume(TokenType::Colon, "Expect ':' after condition");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as usize);
        self.emit_byte(OpCode::POP as usize);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_byte(OpCode::POP as usize);
    }

    fn for_stmt(&mut self) {
        self.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for' ");
        if self.match_token(TokenType::SemiColon) {
            // No initializer
        } else if self.match_token(TokenType::Let) {
            self.var_declaration();
        } else {
            self.expression_stmt();
        }
        let mut loop_start = self.compiler.current_chunk().code.len();
        let mut exit_jump = Option::None;
        if !self.match_token(TokenType::SemiColon) {
            self.expression();
            self.consume(TokenType::SemiColon, "Expect ';' after loop condition");

            // Jump out if the condition is false.
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as usize));
            self.emit_byte(OpCode::POP as usize);
        }

        if !self.match_token(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump as usize);
            let increment_start = self.compiler.current_chunk().code.len();
            self.expression();
            self.emit_byte(OpCode::POP as usize);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);

        if exit_jump.is_some() {
            self.patch_jump(exit_jump.unwrap());
            self.emit_byte(OpCode::POP as usize);
        }

        self.end_scope();
    }

    // -----------------------------------------------------------
    //                      Expressions
    // -----------------------------------------------------------

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }
        self.consume(TokenType::RightBrace, "Expect '}' after block")
    }

    fn ternary(&mut self) {
        let then_jump = self.emit_jump(OpCode::JumpIfFalse as usize);
        self.emit_byte(OpCode::POP as usize);
        self.expression();

        self.consume(TokenType::Colon, "Expected ':' after condition");

        let else_jump = self.emit_jump(OpCode::Jump as usize);
        self.patch_jump(then_jump);
        self.emit_byte(OpCode::POP as usize);

        self.expression();
        self.patch_jump(else_jump);
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
            TokenType::Plus => {
                if self.prev.literal == "1" || self.prev.literal == "1.0" {
                    return self.emit_byte(OpCode::Inc as usize);
                }
                self.emit_byte(OpCode::ADD as usize)
            }
            TokenType::Minus => {
                if self.prev.literal == "1" || self.prev.literal == "1.0" {
                    return self.emit_byte(OpCode::Dec as usize);
                }
                self.emit_byte(OpCode::SUBSTRACT as usize)
            }
            TokenType::Star => self.emit_byte(OpCode::MULTIPLY as usize),
            TokenType::Slash => self.emit_byte(OpCode::DIVIDE as usize),
            TokenType::PlusPlus => return self.emit_byte(OpCode::PostInc as usize),
            TokenType::MinusMinus => return self.emit_byte(OpCode::PostDec as usize),
            _ => self.error_at_current(format!("{:#?} is not a binary operator", op_type)),
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn postfix(&mut self) {
        let op_type = self.prev._type;
        match op_type {
            TokenType::PlusPlus => return self.emit_byte(OpCode::PostInc as usize),
            TokenType::MinusMinus => return self.emit_byte(OpCode::PostDec as usize),
            _ => self.error_at_current(format!("{:#?} is not a postfix operator", op_type)),
        };
    }

    fn prefix(&mut self) {
        let op_type = self.prev._type;
        self.parse_precedence(Precedence::UNARY);
        match op_type {
            TokenType::Minus => return self.emit_byte(OpCode::NEGATE as usize),
            TokenType::Bang => return self.emit_byte(OpCode::NOT as usize),
            TokenType::PlusPlus => return self.emit_byte(OpCode::PreInc as usize),
            TokenType::MinusMinus => return self.emit_byte(OpCode::PreDec as usize),
            _ => self.error_at_current(format!("{:#?} is not a unary operator", op_type)),
        }
    }

    fn index(&mut self, can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightBracket, "Expected ']' after index value.");

        if can_assign && self.match_token(TokenType::Eq) {
            self.expression();
            return self.emit_byte(OpCode::SetIndex as usize);
        }

        self.emit_byte(OpCode::GetIndex as usize);
    }

    // -----------------------------------------------------------
    //                          Literals
    // -----------------------------------------------------------

    fn number(&mut self) {
        let op_type = self.prev._type;
        match op_type {
            TokenType::Integer => {
                let value: i64 = self.prev.literal.parse().unwrap_or(0);
                self.emit_const(Value::int(value));
            }
            TokenType::Float => {
                let value: f64 = self.prev.literal.parse().unwrap_or(0.0);
                self.emit_const(Value::float(value));
            }
            _ => self.error_at_current(format!("{:#?} is not an Integer or Float", op_type)),
        }
    }

    fn string(&mut self) {
        let chars = self
            .prev
            .literal
            .trim_start_matches("\"")
            .trim_end_matches("\"");
        self.emit_const(Value::string(chars.to_string()));
    }

    fn list(&mut self) {
        let mut item_count = 0;
        if !self.check(TokenType::RightBracket) {
            loop {
                self.expression();
                item_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightBracket, "Expect ']' after list values");
        let value = self.make_const(Value::int(item_count));
        self.emit_bytes(OpCode::List as usize, value);
    }

    fn map(&mut self) {
        let mut item_count = 0;
        if !self.check(TokenType::RightBrace) {
            loop {
                self.expression();
                self.consume(TokenType::Colon, "Expect ':' after key");
                self.expression();
                item_count += 1;
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightBrace, "Expect '}' after key:value pairs");
        let value = self.make_const(Value::int(item_count));
        self.emit_bytes(OpCode::Map as usize, value);
    }

    fn literal(&mut self) {
        match self.prev._type {
            TokenType::False => self.emit_byte(OpCode::FALSE as usize),
            TokenType::True => self.emit_byte(OpCode::TRUE as usize),
            TokenType::None => self.emit_byte(OpCode::NONE as usize),
            TokenType::LeftBracket => self.list(),
            TokenType::LeftBrace => self.map(),
            _ => self.error_at_current(format!("{:#?} is not a literal", self.prev._type)),
        };
    }

    // -----------------------------------------------------------
    //                          Utils
    // -----------------------------------------------------------

    pub fn advance(&mut self) {
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

    pub fn match_token(&mut self, _type: TokenType) -> bool {
        if !self.check(_type) {
            return false;
        }
        self.advance();
        true
    }

    fn check(&self, _type: TokenType) -> bool {
        self.current._type == _type
    }

    // -----------------------------------------------------------
    //                      Scoping
    // -----------------------------------------------------------

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
            self.compiler.locals.pop();
        }
    }

    // -----------------------------------------------------------
    //                      Instructions
    // -----------------------------------------------------------

    fn make_ident_const(&mut self, name: &Token) -> usize {
        self.make_const(Value::string(
            name.literal
                .trim_start_matches("\"")
                .trim_end_matches("\"")
                .to_string(),
        ))
    }

    fn make_const(&mut self, value: Value) -> usize {
        let constant = self.compiler.current_chunk().add_const(value).clone();
        if constant > usize::MAX {
            self.error("Too self.many constans in one chunk.".to_string());
            return 0;
        }
        constant as usize
    }

    fn emit_byte(&mut self, byte: usize) {
        self.compiler
            .current_chunk()
            .write(byte, self.prev.location);
    }

    fn emit_bytes(&mut self, byte_1: usize, byte_2: usize) {
        self.emit_byte(byte_1);
        self.emit_byte(byte_2);
    }

    fn emit_jump(&mut self, instruction: usize) -> usize {
        self.emit_byte(instruction);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.compiler.current_chunk().code.len() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit_byte(OpCode::Loop as usize);

        let offset = self.compiler.current_chunk().code.len() - loop_start + 2;
        if offset as u16 > u16::MAX {
            self.error("Loop body too large.".to_string());
        }

        self.emit_byte(offset >> 8 & 0xff);
        self.emit_byte(offset & 0xff)
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.compiler.current_chunk().code.len() - offset - 2;
        self.compiler.current_chunk().code[offset] = (jump >> 8) & 0xff;
        self.compiler.current_chunk().code[offset + 1] = jump & 0xff;
    }

    fn emit_const(&mut self, value: Value) {
        let value = self.make_const(value);
        self.emit_bytes(OpCode::CONST as usize, value);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::NONE as usize);
        self.emit_byte(OpCode::RETURN as usize);
    }

    pub fn end_compiler(&mut self) -> ObjFunction {
        self.emit_return();
        let function = self.compiler.function.clone();
        if self.compiler.enclosing.is_some() {
            self.compiler = self.compiler.enclosing.clone().unwrap();
        }
        function.clone()
    }

    // -----------------------------------------------------------
    //                          Errors
    // -----------------------------------------------------------

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
        match token._type {
            TokenType::Eof => error::report_error(
                &self.source,
                &format!("{} at end.", msg),
                self.prev.location,
            ),
            TokenType::Err => error::report_error(&self.source, &msg, token.location),
            _ => error::report_error(
                &self.source,
                &format!("{} at '{}'.", msg, token.literal),
                token.location,
            ),
        }
        self.had_error = true;
    }
}
