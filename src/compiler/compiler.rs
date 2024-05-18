use crate::{
    compiler::chunk::Chunk,
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
    objects::object::ObjFunction,
};

use super::parser::Parser;

#[derive(Debug, Clone)]
pub struct Local<'a> {
    pub name: Token<'a>,
    pub depth: isize,
}

#[derive(Debug, Clone)]
pub enum FunctionType {
    FunctionBody,
    Script,
}

#[derive(Debug, Clone)]
pub struct Compiler<'a> {
    pub enclosing: Option<Box<Compiler<'a>>>,
    pub function: ObjFunction,
    pub _type: FunctionType,
    pub locals: Vec<Local<'a>>,
    pub local_count: usize,
    pub scope_depth: isize,
}

impl<'a> Compiler<'a> {
    pub fn init(compiler: Option<Box<Compiler<'a>>>, _type: FunctionType) -> Self {
        Self {
            function: ObjFunction::new(),
            _type,
            local_count: 1,
            scope_depth: 0,
            locals: vec![Local {
                depth: 0,
                name: Token::default(),
            }],
            enclosing: compiler,
        }
    }

    pub fn compile(&self, source: &'a str) -> Option<ObjFunction> {
        let lexer = Lexer::init(source);
        let mut parser = Parser::init(source, lexer, Box::new(self.clone()));

        parser.had_error = false;
        parser.panic_mode = false;

        parser.advance();

        while !parser.match_token(TokenType::Eof) {
            parser.declaration();
        }

        let function = parser.end_compiler().clone();
        if parser.had_error {
            None
        } else {
            Some(function)
        }
    }

    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.function.chunk
    }
}
