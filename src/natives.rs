use std::{
    io::Write,
    thread::sleep,
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use crate::value::Value;

pub fn type_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> type() takes 1 argument but got {}",
            arg_count
        ));
    }
    println!("{}", args[0].as_object());
    Ok(Value::none())
}

pub fn clock_native(_arg_count: usize, _args: &[Value]) -> Result<Value, String> {
    let now = SystemTime::now();
    let duration = now.duration_since(UNIX_EPOCH).unwrap();
    let miliseconds = duration.as_secs() * 1000 + duration.as_millis() as u64;
    Ok(Value::float(miliseconds as f64))
}

pub fn sleep_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 1 {
        return Err(format!(
            "<Native Fn> sleep() takes 1 argument but got {}",
            arg_count
        ));
    }

    if !args[0].is_int() && !args[0].is_float() {
        return Err(format!(
            "<Native Fn> sleep() Argument must be Int or Float but got {:#?}",
            args[0]
        ));
    }

    if args[0].is_int() {
        if args[0].as_int().value < 0 {
            return Err(format!(
                "<Native Fn> sleep() Argument must be Int or Float but got {:#?}",
                args[0]
            ));
        }
        sleep(Duration::from_secs(args[0].as_int().value as u64));
        Ok(Value::none())
    } else {
        if args[0].as_float().value.raw < 0.0 {
            return Err(format!(
                "<Native Fn> sleep() Argument must be Int or Float but got {:#?}",
                args[0]
            ));
        }
        sleep(Duration::from_secs_f64(args[0].as_float().value.raw));
        Ok(Value::none())
    }
}

pub fn print_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count == 0 {
        println!();
        return Ok(Value::none());
    }
    for value in args.iter() {
        value.print();
        println!();
    }
    Ok(Value::none())
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
        Ok(_) => Ok(Value::string(buffer.trim().to_string())),
        Err(err) => return Err(format!("<Native Fn> input {}", err)),
    }
}

pub fn exit_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count > 1 {
        return Err(format!(
            "<Native Fn> exit() takes 0 or 1 argument but got {}",
            arg_count
        ));
    }
    if !args.is_empty() {
        if !args[0].is_int() {
            return Err(format!(
                "<Native Fn> exit() argument must be Int but got {:#?}",
                args[0]
            ));
        }
        std::process::exit(args[0].as_int().value as i32);
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

    if !args[0].is_int() && !args[0].is_float() {
        return Err(format!(
            "<Native Fn> sqrt() argument must be Int or Float but got {:#?}",
            args[0]
        ));
    }

    if args[0].is_int() {
        Ok(Value::float((args[0].as_int().value as f64).sqrt()))
    } else {
        Ok(Value::float((args[0].as_float().value.raw).sqrt()))
    }
}

pub fn pow_native(arg_count: usize, args: &[Value]) -> Result<Value, String> {
    if arg_count != 2 {
        return Err(format!(
            "<Native Fn> pow() takes 2 argument but got {}",
            arg_count
        ));
    }

    if !args[0].is_int() && !args[0].is_float() {
        return Err(format!(
            "<Native Fn> pow() argument must be Int or Float but got {:#?}",
            args[0]
        ));
    }

    if !args[1].is_int() && !args[1].is_float() {
        return Err(format!(
            "<Native Fn> pow() argument must be Int or Float but got {:#?}",
            args[0]
        ));
    }

    if args[0].is_int() {
        if args[1].is_float() {
            return Ok(Value::float(
                (args[0].as_int().value as f64).powf(args[1].as_float().value.raw),
            ));
        } else {
            return Ok(Value::float(
                (args[0].as_int().value as f64).powf(args[1].as_int().value as f64),
            ));
        }
    }

    if args[1].is_int() {
        return Ok(Value::float(
            args[0]
                .as_float()
                .value
                .raw
                .powf(args[1].as_int().value as f64),
        ));
    } else {
        return Ok(Value::float(
            args[0]
                .as_float()
                .value
                .raw
                .powf(args[1].as_float().value.raw),
        ));
    }
}
