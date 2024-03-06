use crate::{
    chunk::{Chunk, OpCode},
    compiler::Parser,
    debug::disassemble_instruction,
    lexer::Lexer,
    value::{print_value, Value},
    DEBUG_TRACE_EXECUTION,
};

pub enum InterpretResult<T> {
    Ok(T),
    CompileErr(T),
    RuntimeErr(T),
}

pub struct VM {
    chunk: Box<Chunk>,
    ip: usize,
    stack: Vec<Value>,
    stack_top: isize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            chunk: Box::new(Chunk::init()),
            ip: 0,
            stack: Vec::new(),
            stack_top: -1,
        }
    }

    pub fn init() -> Self {
        let mut vm = VM::new();
        vm.reset_stack();
        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult<()> {
        let mut chunk = Chunk::init();
        let lexer = Lexer::init(source);
        let mut compiler = Parser::init(lexer, &mut chunk);

        if !compiler.compile() {
            return InterpretResult::CompileErr(());
        }

        self.chunk = Box::new(chunk);
        self.ip = 0;

        self.run()
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack.pop().unwrap_or(0.0)
    }

    fn reset_stack(&mut self) {
        self.stack_top = -1;
    }

    fn read_chunk(&mut self) -> usize {
        let byte = self.chunk.code[self.ip];
        self.ip += 1;
        byte
    }

    fn read_constant(&mut self) -> Value {
        let chunk = self.read_chunk();
        self.chunk.constants[chunk]
    }

    fn run(&mut self) -> InterpretResult<()> {
        macro_rules! binary_op {
            ($op:tt) => {{
                let b = self.pop();
                let a = self.pop();
                self.push(a $op b);
            }};
        }

        loop {
            if DEBUG_TRACE_EXECUTION {
                for slot in self.stack.iter() {
                    print!("[ ");
                    print_value(&slot);
                    print!(" ]");
                }
                println!();
                // disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_chunk();
            match OpCode::from(instruction) {
                OpCode::ADD => binary_op!(+),
                OpCode::SUBSTRACT => binary_op!(-),
                OpCode::MULTIPLY => binary_op!(*),
                OpCode::DIVIDE => binary_op!(/),

                OpCode::CONST => {
                    let constant: Value = self.read_constant();
                    self.push(constant);
                }
                OpCode::NEGATE => {
                    let constant = self.stack[self.stack_top as usize];
                    self.stack[self.stack_top as usize] = -constant;
                }
                OpCode::RETURN => {
                    print_value(&self.pop());
                    println!();
                    return InterpretResult::Ok(());
                }
            }
        }
    }
}
