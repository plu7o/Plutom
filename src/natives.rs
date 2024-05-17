use std::{
    cell::RefCell,
    io::Write,
    rc::Rc,
    thread::sleep,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::{object::ObjType, value::Value};

pub fn type_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> type() takes 1 argument but got {}",
            arg_count
        ));
    }
    let arg = args[0].as_ref().borrow();
    println!("{}", arg.as_object());
    Ok(Value::none())
}

pub fn clock_native(_arg_count: usize, _args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    let now = SystemTime::now();
    let duration = now.duration_since(UNIX_EPOCH).unwrap();
    let miliseconds = duration.as_secs() * 1000 + duration.as_millis() as u64;
    Ok(Value::float(miliseconds as f64))
}

pub fn sleep_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> sleep() takes 1 argument but got {}",
            arg_count
        ));
    }
    let arg = args[0].as_ref().borrow();
    match arg.as_object() {
        ObjType::Int(int) => {
            if int.value.is_negative() {
                return Err(format!(
                    "<Native Fn> sleep() Argument cannot be negative but got {}",
                    arg
                ));
            }
            sleep(Duration::from_secs(arg.as_int().value as u64));
            Ok(Value::none())
        }
        ObjType::Float(float) => {
            if float.value.raw.is_sign_negative() {
                return Err(format!(
                    "<Native Fn> sleep() Argument cannot be negative but got {}",
                    arg
                ));
            }
            sleep(Duration::from_secs_f64(arg.as_float().value.raw));
            Ok(Value::none())
        }
        _ => {
            return Err(format!(
                "<Native Fn> sleep() Argument must be Int or Float but got {}",
                arg
            ))
        }
    }
}

pub fn print_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count == 0 {
        println!();
        return Ok(Value::none());
    }
    for value in args.iter() {
        value.as_ref().borrow().print();
        println!();
    }
    Ok(Value::none())
}

pub fn input_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count > 1 {
        return Err(format!(
            "<Native Fn> input() takes 0 or 1 argument but got {}",
            arg_count
        ));
    }
    if !args.is_empty() {
        let arg = args[0].as_ref().borrow();
        if !arg.is_string() {
            return Err(format!(
                "<Native Fn> input() argument must be a String but got {:#?}",
                args[0]
            ));
        }
        arg.print();
        std::io::stdout().flush().unwrap();
    }
    let mut buffer = String::new();
    match std::io::stdin().read_line(&mut buffer) {
        Ok(_) => Ok(Value::string(buffer.trim().to_string())),
        Err(err) => return Err(format!("<Native Fn> input {}", err)),
    }
}

pub fn exit_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count > 1 {
        return Err(format!(
            "<Native Fn> exit() takes 0 or 1 argument but got {}",
            arg_count
        ));
    }
    if !args.is_empty() {
        let arg = args[0].as_ref().borrow();
        if !arg.is_int() {
            return Err(format!(
                "<Native Fn> exit() argument must be Int but got {:#?}",
                args[0]
            ));
        }
        std::process::exit(arg.as_int().value as i32);
    }
    std::process::exit(0);
}

pub fn sqrt_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count != 1 && !args.is_empty() {
        return Err(format!(
            "<Native Fn> sqrt() takes 1 argument but got {}",
            arg_count
        ));
    }
    let arg = args[0].as_ref().borrow();
    match arg.as_object() {
        ObjType::Int(int) => Ok(Value::float((int.value as f64).sqrt())),
        ObjType::Float(float) => Ok(Value::float((float.value.raw).sqrt())),
        _ => Err(format!(
            "<Native Fn> sqrt() argument must be Int or Float but got {:#?}",
            args[0]
        )),
    }
}

pub fn pow_native(arg_count: usize, args: &[Rc<RefCell<Value>>]) -> Result<Value, String> {
    if arg_count != 2 && args.len() != 2 {
        return Err(format!(
            "<Native Fn> pow() takes 2 argument but got {}",
            arg_count
        ));
    }
    let a = args[0].as_ref().borrow();
    let b = args[1].as_ref().borrow();
    match (a.as_object(), b.as_object()) {
        (ObjType::Int(base), ObjType::Int(exp)) => {
            Ok(Value::float((base.value as f64).powf(exp.value as f64)))
        }
        (ObjType::Int(base), ObjType::Float(exp)) => {
            Ok(Value::float((base.value as f64).powf(exp.value.raw)))
        }
        (ObjType::Float(base), ObjType::Float(exp)) => {
            Ok(Value::float(base.value.raw.powf(exp.value.raw)))
        }
        (ObjType::Float(base), ObjType::Int(exp)) => {
            Ok(Value::float(base.value.raw.powf(exp.value as f64)))
        }
        _ => Err(format!(
            "<Native Fn> pow() arguments must be Int or Float but got {}, {}",
            a, b
        )),
    }
}
