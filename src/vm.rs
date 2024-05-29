use crate::{
    compiler::{
        chunk::OpCode,
        compiler::{Compiler, FunctionType},
    },
    debug::disassemble_instruction,
    error,
    objects::{closures::ObjClosure, object::ObjType, string::ObjString, upvalue::ObjUpValue},
    stdlib::natives,
    value::Value,
    DEBUG_PRINT_STACK, DEBUG_TRACE_EXECUTION,
};
use core::fmt;
use std::{cell::RefCell, char, collections::HashMap, fmt::Debug, rc::Rc, usize};

enum BinaryOps {
    ADD,
    SUB,
    DIV,
    MUL,
    LT,
    GT,
    EQ,
}

impl fmt::Display for BinaryOps {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOps::ADD => write!(f, "+"),
            BinaryOps::SUB => write!(f, "-"),
            BinaryOps::DIV => write!(f, "/"),
            BinaryOps::MUL => write!(f, "*"),
            BinaryOps::LT => write!(f, "<"),
            BinaryOps::GT => write!(f, ">"),
            BinaryOps::EQ => write!(f, "=="),
        }
    }
}

pub enum InterpretResult<T, E> {
    Ok(T),
    CompileErr(E),
    RuntimeErr(E),
}

#[derive(Debug, Clone)]
struct CallFrame {
    closure: Rc<RefCell<ObjClosure>>,
    ip: usize,
    slots_offset: usize,
    slots_len: usize,
}

pub struct VM {
    frames: Vec<CallFrame>,
    frame_count: usize,
    stack: Vec<Rc<RefCell<Value>>>,
    stack_top: usize,
    globals: HashMap<ObjString, Rc<RefCell<Value>>>,
    open_upvalues: Vec<ObjUpValue>,
    source: String,
}

impl VM {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            stack: Vec::new(),
            open_upvalues: Vec::new(),
            globals: HashMap::new(),
            source: String::new(),
            frame_count: 0,
            stack_top: 0,
        }
    }

    pub fn init() -> Self {
        let mut vm = VM::new();
        vm.define_native("time", natives::time_native);
        vm.define_native("sleep", natives::sleep_native);
        vm.define_native("print", natives::print_native);
        vm.define_native("input", natives::input_native);
        vm.define_native("exit", natives::exit_native);
        vm.define_native("sqrt", natives::sqrt_native);
        vm.define_native("pow", natives::pow_native);
        vm.define_native("type", natives::type_native);
        vm.reset_stack();
        vm
    }

    pub fn interpret(&mut self, source: String) -> InterpretResult<(), &str> {
        let mut compiler = Compiler::init(None, FunctionType::Script);
        let function = match Compiler::compile(&mut compiler, &source) {
            Some(function) => function,
            None => return InterpretResult::CompileErr("Panic!"),
        };

        self.source = source;
        self.push(Value::function(function.to_owned()));
        let closure = ObjClosure::new(function);
        self.pop();
        self.push(Value::closure(closure.to_owned()));
        self.call(closure, 0);
        self.run()
    }

    fn run(&mut self) -> InterpretResult<(), &str> {
        let mut frame = self.frames[self.frames.len() - 1].to_owned();

        macro_rules! read_byte {
            () => {{
                let byte = frame.closure.as_ref().borrow().function.chunk.code[frame.ip];
                frame.ip += 1;
                byte
            }};
        }

        macro_rules! read_short {
            () => {{
                frame.ip += 2;
                let offset =
                    (frame.closure.as_ref().borrow().function.chunk.code[frame.ip - 2] << 8) as u16;
                offset | frame.closure.as_ref().borrow().function.chunk.code[frame.ip - 1] as u16
            }};
        }

        macro_rules! read_const {
            () => {{
                &frame.closure.as_ref().borrow().function.chunk.constants[read_byte!()]
            }};
        }

        macro_rules! access_frame_slot {
            ($slot_index:expr) => {{
                self.stack[frame.slots_offset + $slot_index].clone()
            }};
        }

        macro_rules! num_ops {
            ($val_type:expr, $a:expr, $b:expr, $op:expr) => {{
                self.pop();
                self.pop();
                match $op {
                    BinaryOps::ADD => self.push($val_type($a + $b)),
                    BinaryOps::SUB => self.push($val_type($a - $b)),
                    BinaryOps::DIV => self.push($val_type($a / $b)),
                    BinaryOps::MUL => self.push($val_type($a * $b)),
                    BinaryOps::GT => self.push(Value::bool($a > $b)),
                    BinaryOps::LT => self.push(Value::bool($a < $b)),
                    BinaryOps::EQ => self.push(Value::bool($a == $b)),
                }
            }};
        }

        macro_rules! binary_op {
            ($op:expr) => {{
                let left = self.peek(1);
                let left = left.as_ref().borrow();

                let right = self.peek(0);
                let right = right.as_ref().borrow();

                match (left.as_object(), right.as_object()) {
                    (ObjType::Int(a), ObjType::Int(b)) => {
                        num_ops!(Value::int, a.clone(), b.clone(), $op);
                    }
                    (ObjType::Int(a), ObjType::Float(b)) => {
                        num_ops!(Value::float, a.clone(), b.clone(), $op);
                    }
                    (ObjType::Float(a), ObjType::Float(b)) => {
                        num_ops!(Value::float, a.clone(), b.clone(), $op);
                    }
                    (ObjType::Float(a), ObjType::Int(b)) => {
                        num_ops!(Value::float, a.clone(), b.clone(), $op);
                    }
                    (ObjType::Bool(a), ObjType::Bool(b)) => match $op {
                        BinaryOps::EQ => {
                            self.pop();
                            self.pop();
                            self.push(Value::bool(a.clone() == b.clone()));
                        }
                        _ => {
                            self.runtime_error(&format!(
                                "Operation not supported on Bool -> {} {} {}.",
                                left, $op, right
                            ));
                            return InterpretResult::RuntimeErr("BinaryOpError");
                        }
                    },
                    (ObjType::Str(a), ObjType::Str(b)) => match $op {
                        BinaryOps::ADD => {
                            self.pop();
                            self.pop();
                            let mut value = String::new();
                            value.push_str(&a.value);
                            value.push_str(&b.value);
                            self.push(Value::string(value));
                        }
                        BinaryOps::EQ => {
                            self.pop();
                            self.pop();
                            self.push(Value::bool(a.clone() == b.clone()));
                        }
                        BinaryOps::LT => {
                            self.pop();
                            self.pop();
                            self.push(Value::bool(a.clone() < b.clone()));
                        }
                        BinaryOps::GT => {
                            self.pop();
                            self.pop();
                            self.push(Value::bool(a.clone() > b.clone()));
                        }
                        _ => {
                            self.runtime_error(&format!(
                                "Operation not supported on Strings -> {} {} {}.",
                                left, $op, right
                            ));
                            return InterpretResult::RuntimeErr("BinaryOpError");
                        }
                    },
                    (ObjType::Str(string), ObjType::Int(int))
                    | (ObjType::Int(int), ObjType::Str(string)) => match $op {
                        BinaryOps::MUL => {
                            self.pop();
                            self.pop();
                            let mut value = String::new();
                            for _ in 0..int.value {
                                value.push_str(&string.value);
                            }
                            self.push(Value::string(value));
                        }
                        _ => {
                            self.runtime_error(&format!(
                                "Operation not supported on Strings -> {} {} {}.",
                                left, $op, right
                            ));
                            return InterpretResult::RuntimeErr("BinaryOpError");
                        }
                    },
                    _ => {
                        self.runtime_error(&format!(
                            "Operation not supported -> {} {} {}.",
                            left, $op, right
                        ));
                        return InterpretResult::RuntimeErr("BinaryOpError");
                    }
                }
            }};
        }

        macro_rules! pre_op {
            ($op:tt) => {{
                let value_ptr = self.pop();
                let value = value_ptr.as_ref();
                match value.borrow_mut().as_mut_object() {
                    ObjType::Int(i) => {
                        i.value = i.value $op 1;
                    }
                    ObjType::Float(f) => {
                        f.value.raw = f.value.raw $op 1.0;
                    }
                    _ => {
                        self.runtime_error(&format!("Increment on {} not supported", value.borrow()));
                        return InterpretResult::RuntimeErr("PreOpError");
                    }
                }
                self.push_pointer(value_ptr);
            }};
        }

        macro_rules! post_op {
            ($op:tt) => {{
                let value_ptr = self.pop();
                let value = value_ptr.as_ref();

                match value.borrow_mut().as_mut_object() {
                    ObjType::Int(i) => {
                        self.push(Value::int(i.value));
                        i.value = i.value $op 1;
                    }
                    ObjType::Float(f) => {
                        self.push(Value::float(f.value.raw));
                        f.value.raw = f.value.raw $op 1.0;
                    }
                    _ => {
                        self.runtime_error(&format!("Increment on {} not supported", value.borrow()));
                        return InterpretResult::RuntimeErr("PostOpError");
                    }
                };
            }};
        }

        loop {
            if DEBUG_PRINT_STACK {
                println!("{:#?}", self.stack);
            }
            if DEBUG_TRACE_EXECUTION {
                for slot in self.stack.iter() {
                    print!("[ ");
                    print!("{}", slot.as_ref().borrow());
                    print!(" ]");
                }
                println!();
                disassemble_instruction(&frame.closure.as_ref().borrow().function.chunk, frame.ip);
            }
            let instruction = read_byte!();
            match OpCode::from(instruction) {
                OpCode::ADD => binary_op!(BinaryOps::ADD),
                OpCode::SUBSTRACT => binary_op!(BinaryOps::SUB),
                OpCode::MULTIPLY => binary_op!(BinaryOps::MUL),
                OpCode::DIVIDE => binary_op!(BinaryOps::DIV),
                OpCode::GREATER => binary_op!(BinaryOps::GT),
                OpCode::LESS => binary_op!(BinaryOps::LT),
                OpCode::EQUAL => binary_op!(BinaryOps::EQ),
                OpCode::Inc => pre_op!(+),
                OpCode::Dec => pre_op!(-),
                OpCode::PreInc => pre_op!(+),
                OpCode::PreDec => pre_op!(-),
                OpCode::PostInc => post_op!(+),
                OpCode::PostDec => post_op!(-),
                OpCode::NONE => self.push(Value::none()),
                OpCode::TRUE => self.push(Value::bool(true)),
                OpCode::FALSE => self.push(Value::bool(false)),
                OpCode::Dup => {
                    let value = self.peek(0);
                    self.push_pointer(value.clone());
                }
                OpCode::DefineGlobal => {
                    let name = read_const!();
                    let name = name.as_string();
                    self.globals.insert(name.clone(), self.peek(0).clone());
                    self.pop();
                }
                OpCode::LoadGlobal => {
                    let name = read_const!().as_string().clone();
                    if let Some(value) = self.globals.get(&name) {
                        self.push_pointer(value.clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'", name.value));
                        return InterpretResult::RuntimeErr("VariableError");
                    }
                }
                OpCode::SetGlobal => {
                    let name = read_const!().as_string().clone();
                    if self.globals.get(&name).is_some() {
                        self.globals.insert(name, self.peek(0).clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'", name.value));
                        return InterpretResult::RuntimeErr("VariableError");
                    }
                }
                OpCode::LoadLocal => {
                    let slot = read_byte!();
                    self.push_pointer(access_frame_slot!(slot));
                }
                OpCode::SetLocal => {
                    let slot = read_byte!();
                    self.stack[frame.slots_offset + slot] = self.peek(0);
                }
                OpCode::LoadUpValue => {
                    let slot = read_byte!();
                    self.push_pointer(
                        frame.closure.as_ref().borrow().upvalues[slot]
                            .as_ref()
                            .borrow()
                            .location
                            .clone(),
                    );
                }
                OpCode::SetUpValue => {
                    let slot = read_byte!();
                    frame.closure.as_ref().borrow_mut().upvalues[slot]
                        .as_ref()
                        .borrow_mut()
                        .location = self.peek(0);
                }
                OpCode::CloseUpValue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop();
                }
                OpCode::Loop => {
                    let offset = read_short!();
                    frame.ip -= offset as usize;
                }
                OpCode::JumpIfFalse => {
                    let offset = read_short!();
                    if self.is_falsy(&self.peek(0).as_ref().borrow()) {
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
                OpCode::NOT => {
                    let value = self.pop();
                    self.push(Value::bool(self.is_falsy(&value.as_ref().borrow())));
                }
                OpCode::Compare => {
                    self.pop();
                    self.push(Value::bool(
                        self.peek(1).as_ref().borrow().as_object()
                            == self.peek(0).as_ref().borrow().as_object(),
                    ));
                }
                OpCode::CONST => {
                    let constant: Value = read_const!().clone();
                    self.push(constant);
                }
                OpCode::NEGATE => match self.pop().as_ref().borrow().as_object() {
                    ObjType::Int(int) => {
                        self.push(Value::int(-int.value));
                    }
                    ObjType::Float(float) => {
                        self.push(Value::float(-float.value.raw));
                    }
                    _ => {
                        self.runtime_error(&format!(
                            "Operand must be a number. Can't negate {:?}",
                            self.peek(0)
                        ));
                        return InterpretResult::RuntimeErr("NegationError");
                    }
                },
                OpCode::ECHO => {
                    let val = self.pop();
                    val.as_ref().borrow().print();
                    println!();
                }
                OpCode::Call => {
                    let arg_count = read_byte!();
                    self.frames[self.frame_count - 1] = frame.clone();
                    if !self.call_value(&self.peek(arg_count).as_ref().borrow(), arg_count) {
                        return InterpretResult::RuntimeErr("CallError");
                    }
                    frame = self.frames[self.frame_count - 1].to_owned();
                }
                OpCode::RETURN => {
                    let result = self.pop();
                    self.close_upvalues(frame.slots_offset);
                    self.frames.pop();
                    self.frame_count -= 1;

                    if self.frame_count == 0 {
                        self.pop();
                        return InterpretResult::Ok(());
                    }

                    self.stack_top -= frame.slots_len;
                    self.stack.truncate(self.stack_top);
                    self.push_pointer(result);
                    frame = self.frames[self.frame_count - 1].to_owned();
                }
                OpCode::Closure => {
                    let function = read_const!();
                    let mut closure = ObjClosure::new(function.as_function().clone());

                    for _ in 0..closure.upvalue_count {
                        let is_local = read_byte!();
                        let index = read_byte!();

                        if is_local == 1 {
                            let v = access_frame_slot!(index);
                            closure.upvalues.push(self.capture_upvalue(&v, index));
                        } else {
                            closure
                                .upvalues
                                .push(frame.closure.as_ref().borrow().upvalues[index].clone());
                        }
                    }

                    self.push(Value::closure(closure));
                }
                OpCode::List => {
                    let item_count = read_const!();
                    let mut items: Vec<Value> = vec![];
                    for _ in 0..item_count.as_int().value {
                        items.push(self.pop().as_ref().borrow().to_owned());
                    }
                    items.reverse();
                    self.push(Value::list(items));
                }
                OpCode::Map => {
                    let item_count = read_const!();
                    let mut dict: HashMap<Value, Value> = HashMap::new();
                    for _ in 0..item_count.as_int().value {
                        let value = self.pop();
                        let key = self.pop();
                        let value = value.as_ref().borrow();
                        let key = key.as_ref().borrow();

                        match key.as_object() {
                            ObjType::Int(_)
                            | ObjType::Float(_)
                            | ObjType::Str(_)
                            | ObjType::Bool(_) => {
                                dict.insert(key.to_owned(), value.to_owned());
                            }
                            _ => {
                                self.runtime_error(&format!(
                                    "Key Value must be Literal but got: {}",
                                    key
                                ));
                                return InterpretResult::RuntimeErr("IndexError");
                            }
                        };
                    }
                    self.push(Value::map(dict));
                }
                OpCode::GetIndex => {
                    let index = self.pop();
                    let obj = self.pop();

                    let index = index.as_ref().borrow();
                    let getter = obj.as_ref().borrow();

                    match getter.as_object() {
                        ObjType::List(list) => {
                            if !index.is_int() {
                                self.runtime_error(&format!(
                                    "Index Value must be Int but got: {}",
                                    index
                                ));
                                return InterpretResult::RuntimeErr("IndexError");
                            }
                            let index = index.as_int().value;
                            match self.check_bounds(&index, &(list.items.len() as i64)) {
                                Ok(()) => {
                                    if index.is_negative() {
                                        let offset = list.items.len() as i64 + index;
                                        self.push(list.items[offset as usize].clone());
                                    } else {
                                        self.push(list.items[index as usize].clone());
                                    }
                                }
                                Err(err) => {
                                    self.runtime_error(&err);
                                    return InterpretResult::RuntimeErr("IndexError");
                                }
                            }
                        }
                        ObjType::Map(map) => {
                            match map.dict.get(&index) {
                                Some(value) => self.push(value.clone()),
                                None => {
                                    self.runtime_error(&format!("Key not found in {:#?}", map));
                                    return InterpretResult::RuntimeErr("KeyError");
                                }
                            };
                        }
                        ObjType::Str(str) => {
                            if !index.is_int() {
                                self.runtime_error(&format!(
                                    "Index Value must be Int but got: {}",
                                    index
                                ));
                                return InterpretResult::RuntimeErr("IndexError");
                            }
                            let index = index.as_int().value;
                            match self.check_bounds(&index, &(str.value.len() as i64)) {
                                Ok(()) => {
                                    if index.is_negative() {
                                        let offset = str.value.len() as i64 + index;
                                        let chars: Vec<char> = str.value.chars().collect();
                                        self.push(Value::string(
                                            chars[offset as usize].to_string(),
                                        ));
                                    } else {
                                        let chars: Vec<char> = str.value.chars().collect();
                                        self.push(Value::string(chars[index as usize].to_string()));
                                    }
                                }
                                Err(err) => {
                                    self.runtime_error(&err);
                                    return InterpretResult::RuntimeErr("IndexError");
                                }
                            }
                        }
                        _ => {
                            self.runtime_error(&format!("Value is not indexable {}", getter));
                            return InterpretResult::RuntimeErr("IndexError");
                        }
                    }
                }
                OpCode::SetIndex => {
                    let value = self.pop();
                    let index = self.pop();
                    let obj = self.pop();

                    let setter = value.as_ref().borrow();
                    let index = index.as_ref().borrow();
                    let mut getter = obj.as_ref().borrow_mut();

                    match getter.as_mut_object() {
                        ObjType::List(list) => {
                            if !index.is_int() {
                                self.runtime_error(&format!(
                                    "Index Value must be Int but got: {}",
                                    index
                                ));
                                return InterpretResult::RuntimeErr("IndexError");
                            }
                            let index = index.as_int().value;
                            match self.check_bounds(&index, &(list.items.len() as i64)) {
                                Ok(()) => {
                                    if index.is_negative() {
                                        let offset = list.items.len() as i64 + index;
                                        list.items[offset as usize] = setter.to_owned();
                                    } else {
                                        list.items[index as usize] = setter.to_owned();
                                    }
                                }
                                Err(err) => {
                                    self.runtime_error(&err);
                                    return InterpretResult::RuntimeErr("IndexError");
                                }
                            };
                            self.push(Value::none());
                        }
                        ObjType::Map(map) => {
                            map.dict.insert(index.to_owned(), setter.to_owned());
                            self.push(Value::none());
                        }
                        _ => {
                            self.runtime_error(&format!("Value is not Setable {}", getter));
                            return InterpretResult::RuntimeErr("IndexError");
                        }
                    }
                }
                OpCode::Enum => {
                    let count = read_const!();
                    for _ in 0..count.as_int().value {
                        println!("{:#?}", self.pop());
                    }
                    self.push(Value::none());
                }
            }
        }
    }

    fn call_value(&mut self, callee: &Value, arg_count: usize) -> bool {
        match callee.as_object() {
            ObjType::Closure(closure) => return self.call(closure.clone(), arg_count),
            ObjType::Native(native) => {
                let result =
                    (native.function)(arg_count, &self.stack[self.stack_top - arg_count..]);

                match result {
                    Ok(value) => {
                        self.stack_top = self.stack.len() - (arg_count + 1);
                        self.stack.truncate(self.stack_top);
                        self.push(value);
                        return true;
                    }
                    Err(msg) => {
                        self.runtime_error(&msg);
                        return false;
                    }
                }
            }
            _ => {
                self.runtime_error(&format!(
                    "{} not callable, can only call Functions and Classes.",
                    callee
                ));
                false
            } // Non-callable object type
        }
    }

    fn capture_upvalue(
        &mut self,
        local: &Rc<RefCell<Value>>,
        index: usize,
    ) -> Rc<RefCell<ObjUpValue>> {
        let created_upvalue = ObjUpValue::new(local.clone());

        for (i, upvalue) in self.open_upvalues.iter().enumerate().rev() {
            if upvalue.location == *local && index == i {
                return Rc::new(RefCell::new(upvalue.clone()));
            }

            if upvalue.location == *local && i < index {
                self.open_upvalues.insert(i, created_upvalue.clone());
                return Rc::new(RefCell::new(created_upvalue));
            }
        }

        if self.open_upvalues.is_empty() {
            self.open_upvalues.push(created_upvalue.clone());
        }

        Rc::new(RefCell::new(created_upvalue))
    }

    fn close_upvalues(&mut self, index: usize) {
        for (i, upvalue) in self.open_upvalues.iter_mut().enumerate().rev() {
            if i >= index {
                upvalue.closed = Some(upvalue.location.clone());
                upvalue.location = upvalue.closed.clone().unwrap();
            }

            if i < index {
                break;
            }
        }
    }

    fn call(&mut self, closure: ObjClosure, arg_count: usize) -> bool {
        if arg_count != closure.function.arity {
            self.runtime_error(&format!(
                "Expected {} arguments but got {}",
                closure.function.arity, arg_count
            ));
            return false;
        }

        let offset = self.stack_top - arg_count - 1;
        let frame = CallFrame {
            closure: Rc::new(RefCell::new(closure)),
            ip: 0,
            slots_offset: offset,
            slots_len: arg_count,
        };

        self.frames.push(frame);
        self.frame_count += 1;
        true
    }

    fn check_bounds(&self, index: &i64, len: &i64) -> Result<(), String> {
        // positve index
        if index >= len {
            return Err(format!(
                "Index overflow index is {:?} but len is {:?}",
                index, len
            ));
        }

        // negative index
        if len + index < 0 {
            return Err(format!(
                "Index underflow index is {:?} but len is {:?}",
                index, len
            ));
        }
        Ok(())
    }

    fn define_native(
        &mut self,
        name: &str,
        function: fn(usize, &[Rc<RefCell<Value>>]) -> Result<Value, String>,
    ) {
        self.push(Value::string(name.to_string()));
        self.push(Value::native(function));
        self.globals.insert(
            self.stack[0].as_ref().borrow().as_string().clone(),
            self.stack[1].clone(),
        );
        self.pop();
        self.pop();
    }

    fn is_falsy(&self, value: &Value) -> bool {
        value.is_none() || (value.is_bool() && !value.as_bool().value)
    }

    fn push(&mut self, value: Value) {
        self.stack.push(Rc::new(RefCell::new(value)));
        self.stack_top += 1;
    }

    fn push_pointer(&mut self, value: Rc<RefCell<Value>>) {
        self.stack.push(value);
        self.stack_top += 1;
    }

    fn pop(&mut self) -> Rc<RefCell<Value>> {
        self.stack_top -= 1;
        self.stack.pop().unwrap()
    }

    fn peek(&self, distance: usize) -> Rc<RefCell<Value>> {
        self.stack[self.stack_top as usize - 1 - distance].clone()
    }

    fn reset_stack(&mut self) {
        self.stack_top = self.stack.len();
        self.stack.clear();
        self.frame_count = 0;
    }

    fn runtime_error(&mut self, format: &str) {
        let frame = &self.frames[self.frame_count - 1];
        let instruction = frame.ip;
        let location = frame
            .closure
            .as_ref()
            .borrow()
            .function
            .chunk
            .get_location(instruction);
        error::report_error(&self.source, format, location);
        for i in (0..self.frame_count).rev() {
            let frame = &self.frames[i];
            let function = &frame.closure.as_ref().borrow().function;
            let instruction = frame.ip - self.stack.len() - 1;
            let location = function.chunk.get_location(instruction);
            if let Some(name) = &function.name {
                error::report_error(&self.source, &format!("in {}()", name.value), location);
            } else {
                error::report_error(&self.source, "in script", location);
            }
        }
        self.reset_stack();
    }
}
