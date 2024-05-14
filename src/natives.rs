use std::{
    io::Write,
    thread::sleep,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::value::Value;

pub fn clock_native(_arg_count: usize, _args: &[Value]) -> Result<Value, String> {
    let now = SystemTime::now();
    let duration = now.duration_since(UNIX_EPOCH).unwrap();
    let miliseconds = duration.as_secs() * 1000 + duration.as_millis() as u64;
    Ok(Value::number_val(miliseconds as i64))
}

pub fn sleep_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> sleep() takes 1 argument but got {}",
            arg_count
        ));
    }

    if !args[0].is_number() {
        return Err(format!(
            "<Native Fn> sleep() argument must be a number but got {:#?}",
            args[0]
        ));
    }

    sleep(Duration::from_secs(args[0].as_number() as u64));
    Ok(Value::none_val())
}

pub fn print_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count == 0 {
        println!();
        return Ok(Value::none_val());
    }

    for val in args.iter() {
        val.print();
        println!();
    }

    Ok(Value::none_val())
}

pub fn input_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count > 1 {
        return Err(format!(
            "<Native Fn> input() takes 0-1 argument but got {}",
            arg_count
        ));
    }

    if !args.is_empty() {
        if !args[0].is_string() {
            return Err(format!(
                "<Native Fn> input() argument must be a string but got {:#?}",
                args[0]
            ));
        }
        args[0].print();
        std::io::stdout().flush().unwrap();
    }

    let mut buffer = String::new();
    match std::io::stdin().read_line(&mut buffer) {
        Ok(_) => Ok(Value::string_val(buffer.trim().to_string())),
        Err(err) => return Err(format!("<Native Fn> input {}", err)),
    }
}

pub fn exit_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count > 1 {
        return Err(format!(
            "<Native Fn> exit() takes 0-1 argument but got {}",
            arg_count
        ));
    }

    if !args.is_empty() {
        if !args[0].is_number() {
            return Err(format!(
                "<Native Fn> input argument must be a number but got {:#?}",
                args[0]
            ));
        }
        std::process::exit(args[0].as_number() as i32);
    }

    std::process::exit(0);
}

pub fn sqrt_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> sqrt() takes 1 argument but got {}",
            arg_count
        ));
    }

    if !args[0].is_number() {
        return Err(format!(
            "<Native Fn> sqrt() argument must be a number but got {:#?}",
            args[0]
        ));
    }

    Ok(Value::number_val((args[0].as_number() as f64).sqrt() as i64))
}

pub fn pow_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 2 {
        return Err(format!(
            "<Native Fn> pow() takes 2 argument but got {}",
            arg_count
        ));
    }

    if !args[0].is_number() {
        return Err(format!(
            "<Native Fn> pow() arguments must be a numbers but got {:#?}",
            args[0]
        ));
    }

    Ok(Value::number_val(
        args[0].as_number().pow(args[0].as_number() as u32),
    ))
}
