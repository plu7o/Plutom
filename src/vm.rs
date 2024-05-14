use std::{collections::HashMap, fmt::Debug, usize};

use crate::{
    chunk::OpCode,
    compiler::{Compiler, FunctionType},
    debug::disassemble_instruction,
    natives,
    object::{ObjClosure, ObjList, ObjNative, ObjString, ObjType},
    value::{Value, ValueType},
    DEBUG_TRACE_EXECUTION,
};

pub enum InterpretResult<T, E> {
    Ok(T),
    CompileErr(E),
    RuntimeErr(E),
}

#[derive(Debug, Clone)]
struct CallFrame {
    closure: ObjClosure,
    ip: usize,
    slots: Vec<Value>,
}

pub struct VM {
    frames: Vec<CallFrame>,
    frame_count: usize,
    stack: Vec<Value>,
    stack_top: usize,
    globals: HashMap<ObjString, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            frame_count: 0,
            stack: Vec::new(),
            stack_top: 0,
            globals: HashMap::new(),
        }
    }

    pub fn init() -> Self {
        let mut vm = VM::new();
        vm.define_native("time", natives::clock_native);
        vm.define_native("sleep", natives::sleep_native);
        vm.define_native("print", natives::print_native);
        vm.define_native("input", natives::input_native);
        vm.define_native("exit", natives::exit_native);
        vm.define_native("sqrt", natives::sqrt_native);
        vm.define_native("pow", natives::pow_native);
        vm.reset_stack();
        vm
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult<(), &str> {
        let mut compiler = Compiler::init(None, FunctionType::Script);
        let function = match Compiler::compile(&mut compiler, source) {
            Some(function) => function,
            None => return InterpretResult::CompileErr("Panic"),
        };

        self.push(Value::obj_val(ObjType::Function(function.clone())));
        let closure = ObjClosure::new(function);
        self.pop();
        self.push(Value::obj_val(ObjType::Closure(closure.clone())));
        self.call(&closure, 0);

        self.run()
    }

    fn run(&mut self) -> InterpretResult<(), &str> {
        let mut frame = self.frames[self.frame_count - 1].to_owned();

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

        macro_rules! read_byte {
            () => {{
                let byte = frame.closure.function.chunk.code[frame.ip];
                frame.ip += 1;
                byte
            }};
        }

        macro_rules! read_short {
            () => {{
                frame.ip += 2;
                let offset = (frame.closure.function.chunk.code[frame.ip - 2] << 8) as u16;
                offset | frame.closure.function.chunk.code[frame.ip - 1] as u16
            }};
        }

        macro_rules! read_const {
            () => {{
                frame.closure.function.chunk.constants[read_byte!()].clone()
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
                disassemble_instruction(&frame.closure.function.chunk, frame.ip);
            }

            let instruction = read_byte!();
            match OpCode::from(instruction) {
                OpCode::SUBSTRACT => binary_op!(Value::number_val, -),
                OpCode::MULTIPLY => binary_op!(Value::number_val, *),
                OpCode::DIVIDE => binary_op!(Value::number_val, /),
                OpCode::GREATER => binary_op!(Value::bool_val, >),
                OpCode::LESS => binary_op!(Value::bool_val, <),
                OpCode::NONE => self.push(Value::none_val()),
                OpCode::TRUE => self.push(Value::bool_val(true)),
                OpCode::FALSE => self.push(Value::bool_val(false)),
                OpCode::DefineGlobal => {
                    let name = read_const!();
                    let name = name.as_string();
                    self.globals.insert(name.clone(), self.peek(0).clone());
                    self.pop();
                }
                OpCode::GetGlobal => {
                    let name = read_const!().as_string().clone();
                    if let Some(value) = self.globals.get(&name) {
                        self.push(value.clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'", name.chars));
                        return InterpretResult::RuntimeErr("VariableError");
                    }
                }
                OpCode::SetGlobal => {
                    let name = read_const!().as_string().clone();
                    if self.globals.get(&name).is_some() {
                        self.globals.insert(name, self.peek(0).clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable  {}", name.chars));
                        return InterpretResult::RuntimeErr("VariableError");
                    }
                }
                OpCode::GetLocal => {
                    let slot = read_byte!();
                    self.push(frame.slots[slot].clone());
                }
                OpCode::SetLocal => {
                    let slot = read_byte!();
                    frame.slots[slot] = self.peek(0).clone();
                }
                OpCode::Loop => {
                    let offset = read_short!();
                    frame.ip -= offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = read_short!();
                    if self.is_falsy(self.peek(0)) {
                        frame.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = read_short!();
                    frame.ip += offset as usize;
                }
                OpCode::POP => {
                    self.pop();
                }
                OpCode::ADD => {
                    if self.peek(0).is_string() && self.peek(1).is_string() {
                        self.concatenate()
                    } else if self.peek(0).is_number() && self.peek(1).is_number() {
                        let a = self.pop().as_number();
                        let b = self.pop().as_number();
                        self.push(Value::number_val(a + b))
                    } else {
                        self.runtime_error(&format!(
                            "Operands must be two numbers or two strings, Can't add ({:?}, {:?})",
                            self.peek(1),
                            self.peek(0)
                        ));
                        return InterpretResult::RuntimeErr("BinaryError");
                    }
                }
                OpCode::NOT => {
                    let value = self.pop();
                    self.push(Value::bool_val(self.is_falsy(&value)));
                }
                OpCode::EQUAL => {
                    let a: Value = self.pop();
                    let b: Value = self.pop();
                    self.push(Value::bool_val(a == b))
                }
                OpCode::Compare => {
                    let a: Value = self.peek(1).clone();
                    let b: Value = self.peek(0).clone();
                    self.pop();
                    self.push(Value::bool_val(a == b));
                }
                OpCode::CONST => {
                    let constant: Value = read_const!();
                    self.push(constant);
                }
                OpCode::NEGATE => {
                    if !self.peek(0).is_number() {
                        self.runtime_error(&format!(
                            "Operand must be a number. Can't negate {:?}",
                            self.peek(0)
                        ));
                        return InterpretResult::RuntimeErr("NegationError");
                    }
                    let value = -self.pop().as_number();
                    self.push(Value::number_val(value));
                }
                OpCode::ECHO => {
                    let val = self.pop();
                    val.print();
                    println!();
                }
                OpCode::Call => {
                    let arg_count = read_byte!();
                    self.frames[self.frame_count - 1] = frame.to_owned();
                    if !self.call_value(self.peek(arg_count).clone(), arg_count) {
                        return InterpretResult::RuntimeErr("CallError");
                    }
                    frame = self.frames[self.frame_count - 1].to_owned();
                }
                OpCode::RETURN => {
                    self.frames[self.frame_count - 1] = frame.to_owned();
                    let result = self.pop();
                    self.frame_count -= 1;
                    self.frames.pop();
                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok(());
                    }

                    self.stack_top -= frame.slots.len();
                    self.stack
                        .resize(self.stack_top, Value::new(ValueType::None));
                    self.push(result);
                    frame = self.frames[self.frame_count - 1].to_owned();
                }
                OpCode::Closure => {
                    let function = read_const!();
                    let closure = ObjClosure::new(function.as_function().clone());
                    self.push(Value::obj_val(ObjType::Closure(closure)));
                }
                OpCode::List => {
                    let item_count = read_const!();
                    let mut items: Vec<Value> = vec![];

                    for _ in 0..item_count.as_number() {
                        items.push(self.pop().clone());
                    }

                    items.reverse();
                    let list = ObjList::new(items);
                    self.push(Value::obj_val(ObjType::List(list)));
                }
                OpCode::Index => {
                    let index = self.pop();
                    let list = self.pop();

                    if !list.is_list() {
                        self.runtime_error(&format!("Value: {:#?} is not indexable ", list));
                        return InterpretResult::RuntimeErr("IndexError");
                    }

                    if !index.is_number() {
                        self.runtime_error(&format!(
                            "Index Value must be a number but got: {:#?}",
                            index
                        ));
                        return InterpretResult::RuntimeErr("IndexError");
                    }
                    let num_index = index.as_number();
                    let raw_items = list.as_list();

                    if num_index >= raw_items.items.len() as i64 {
                        self.runtime_error(&format!(
                            "Index overflow index is {:?} but len is {:?}",
                            num_index,
                            raw_items.items.len()
                        ));
                        return InterpretResult::RuntimeErr("IndexError");
                    }

                    if raw_items.items.len() as i64 + num_index < 0 {
                        self.runtime_error(&format!(
                            "Index underflow index is {:?} but len is {:?}",
                            num_index,
                            raw_items.items.len()
                        ));
                        return InterpretResult::RuntimeErr("IndexError");
                    }

                    if num_index.is_negative() {
                        let offset = raw_items.items.len() as i64 + num_index;
                        self.push(raw_items.items[offset as usize].clone());
                    } else {
                        self.push(raw_items.items[num_index as usize].clone());
                    }
                }
            }
        }
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        if callee.is_obj() {
            match callee.as_obj() {
                ObjType::Closure(closure) => return self.call(closure, arg_count),
                ObjType::Native(native) => {
                    let result =
                        (native.function)(arg_count, &self.stack[self.stack_top - arg_count..]);

                    match result {
                        Ok(value) => {
                            self.stack_top = self.stack_top - (arg_count + 1);
                            self.stack
                                .resize(self.stack_top, Value::new(ValueType::None));
                            self.push(value);
                            return true;
                        }
                        Err(msg) => {
                            self.runtime_error(&msg);
                            return false;
                        }
                    }
                }
                _ => (), // Non-callable object type
            }
        }

        self.runtime_error("Can only call functions and classes.");
        false
    }

    fn call(&mut self, closure: &ObjClosure, arg_count: usize) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(&format!(
                "Expected {} arguments but got {}",
                closure.function.arity, arg_count
            ));
            return false;
        }

        if self.frame_count == usize::MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let frame = CallFrame {
            closure: closure.clone(),
            ip: 0,
            slots: self.stack[self.stack_top - arg_count - 1..].to_vec(),
        };
        self.frames.push(frame);
        self.frame_count += 1;
        true
    }

    fn define_native(
        &mut self,
        name: &str,
        function: fn(usize, &[Value]) -> Result<Value, String>,
    ) {
        self.push(Value::obj_val(ObjType::String(ObjString::new(
            name.to_string(),
        ))));
        self.push(Value::obj_val(ObjType::Native(ObjNative::new(function))));
        self.globals
            .insert(self.stack[0].as_string().clone(), self.stack[1].clone());
        self.pop();
        self.pop();
    }

    fn concatenate(&mut self) {
        let b = self.pop();
        let a = self.pop();
        let a_string = &a.as_string();
        let b_string = &b.as_string();
        let mut chars = String::new();
        chars.push_str(&a_string.chars);
        chars.push_str(&b_string.chars);
        let length = a_string.len + b_string.len;

        self.push(Value::obj_val(ObjType::String(ObjString {
            len: length.to_owned(),
            chars,
        })));
    }

    fn is_falsy(&self, value: &Value) -> bool {
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
        &self.stack[self.stack_top as usize - 1 - distance]
    }

    fn reset_stack(&mut self) {
        self.stack_top = self.stack.len();
        self.stack.clear();
        self.frame_count = 0;
    }

    fn runtime_error(&mut self, format: &str) {
        let frame = &self.frames[self.frame_count - 1];
        let instruction = frame.ip;
        let line = frame.closure.function.chunk.get_line(instruction);
        println!("[Line {}] {}", line, format);

        for i in (0..self.frame_count - 1).rev() {
            let frame = &self.frames[i];
            let function = &frame.closure.function;
            let instruction = frame.ip - function.chunk.code.len() - 1;
            eprint!("[line {}] in ", function.chunk.get_line(instruction));
            if let Some(name) = &function.name {
                eprintln!("{}()", name.chars);
            } else {
                eprintln!("script");
            }
        }

        self.reset_stack();
    }
}
