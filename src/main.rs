mod compiler;
mod debug;
mod error;
mod lexer;
mod objects;
mod stdlib;
mod value;
mod vm;

use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::io::Write;
use std::process;

use vm::InterpretResult;

const DEBUG_TRACE_EXECUTION: bool = false;
const DEBUG_PRINT_STACK: bool = false;
const DEBUG_DUMP_CODE: bool = true;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: plutom [script]");
            process::exit(64);
        }
    }
}

fn repl() {
    let mut vm = vm::VM::init();
    let prompt = "PlutomV0.01 >> ";
    loop {
        let mut input = String::new();
        print!("{}", prompt);
        io::stdout().flush().unwrap();
        io::stdin()
            .read_line(&mut input)
            .expect("InputError: Failed reading input.");
        vm.interpret(input);
    }
}

fn run_file(path: &str) {
    let source = match read_file(path) {
        Ok(source) => source,
        Err(err) => {
            println!("FileError: {}.", err);
            process::exit(74);
        }
    };
    let mut vm = vm::VM::init();
    let result = vm.interpret(source);
    match result {
        InterpretResult::CompileErr(msg) => {
            println!("CompileError: {:?}.", msg);
            process::exit(65)
        }
        InterpretResult::RuntimeErr(msg) => {
            println!("RuntimeError: {:?}.", msg);
            process::exit(70)
        }
        _ => (),
    }
}

fn read_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = fs::File::open(path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    Ok(buf)
}
