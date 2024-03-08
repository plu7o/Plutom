use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    debug::disassemble_instruction,
    lexer::Lexer,
    value::{ObjString, ObjType, Value},
    DEBUG_TRACE_EXECUTION,
};

pub enum InterpretResult<T, E> {
    Ok(T),
    CompileErr(E),
    RuntimeErr(E),
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

    pub fn interpret(&mut self, source: &str) -> InterpretResult<(), &str> {
        let lexer = Lexer::init(source);
        let mut chunk = Chunk::init();
        let mut compiler = Compiler::init(lexer, &mut chunk);

        if !compiler.compile() {
            return InterpretResult::CompileErr("CompileError: Fatal");
        }

        self.chunk = Box::new(chunk);
        self.ip = 0;
        self.run()
    }

    #[rustfmt::skip]
    fn run(&mut self) -> InterpretResult<(), &str> {
        macro_rules! binary_op {
            ($val_type:expr, $op:tt) => {{
                if !self.peek(0).is_number() || !self.peek(1).is_number() {
                    self.runtime_error("Operands must be numbers.");
                    return InterpretResult::RuntimeErr("Operands must be numbers.");
                }
                let b = self.pop().as_number();
                let a = self.pop().as_number();
                self.push($val_type(a $op b));
            }};
        }

        loop {
            if DEBUG_TRACE_EXECUTION {
                for slot in self.stack.iter() {
                    print!("[ ");
                    slot.print();
                    print!(" ]");
                }
                println!();
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = self.read_chunk();
            match OpCode::from(instruction) {
                OpCode::ADD => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate()
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let a = self.pop().as_number();
                        let b = self.pop().as_number();
                        self.push(Value::number_val(a + b))
                        
                    } else {
                        self.runtime_error(&format!("Operands must be two numbers or two strings"));
                        return InterpretResult::RuntimeErr("Operands must be two numbers or two strings");
                    }
                },
                OpCode::SUBSTRACT   => binary_op!(Value::number_val, -),
                OpCode::MULTIPLY    => binary_op!(Value::number_val, *),
                OpCode::DIVIDE      => binary_op!(Value::number_val, /),
                OpCode::GREATER     => binary_op!(Value::bool_val, >),
                OpCode::LESS        => binary_op!(Value::bool_val, <),
                OpCode::NONE        => self.push(Value::none_val()),
                OpCode::TRUE        => self.push(Value::bool_val(true)),
                OpCode::FALSE       => self.push(Value::bool_val(false)),
                OpCode::NOT => {
                    let value = self.pop();
                    self.push(Value::bool_val(self.is_falsy(value)));
                }
                OpCode::EQUAL => {
                    let a: Value = self.pop();
                    let b: Value = self.pop();
                    self.push(Value::bool_val(a == b))
                }

                OpCode::CONST => {
                    let constant: Value = self.read_constant();
                    self.push(constant);
                }
                OpCode::NEGATE => {
                    if !self.peek(0).is_number() {
                        self.runtime_error("Operand must be a number.");
                        return InterpretResult::RuntimeErr("Operand must be a number.");
                    }
                    let value = -self.pop().as_number();
                    self.push(Value::number_val(value));
                }
                OpCode::RETURN => {
                    self.pop().print();
                    println!();
                    return InterpretResult::Ok(());
                }
            }
        }
    }

    fn concatenate(&mut self) {
        let a = self.pop();
        let b = self.pop();
        let a_string = &a.as_string();
        let b_string = &b.as_string();
        let mut chars = String::new();
        chars.push_str(&a_string.chars);
        chars.push_str(&b_string.chars);
        let length = a_string.len + b_string.len;
        
        self.push(
            Value::obj_val(
                ObjType::String(
                    ObjString {
                        len: length.to_owned(),
                        chars,
                     }
                )
            )
        );
    }

    fn is_falsy(&self, value: Value) -> bool {
        value.is_none() || (value.is_bool() && !value.as_bool())
    }

    fn push(&mut self, value: Value) {
        self.stack.push(value);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Value {
        self.stack_top -= 1;
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> &Value {
        &self.stack[self.stack_top as usize - distance]
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
        self.chunk.constants[chunk].clone()
    }

    fn runtime_error(&mut self, format: &str) {
        println!("{}", format);
        let instruction = self.ip - self.chunk.code.len() - 1;
        let line = self.chunk.get_line(instruction);
        println!("[Line {}] in script\n", line);
        self.reset_stack();
    }
}
