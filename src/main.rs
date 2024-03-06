mod chunk;
mod compiler;
mod debug;
mod lexer;
mod value;
mod vm;

use std::env;
use std::fs;
use std::io;
use std::io::Read;
use std::io::Write;
use std::process;

use vm::InterpretResult;

const DEBUG_PRINT_CODE: bool = true;
const DEBUG_TRACE_EXECUTION: bool = true;

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage Plutom");
            process::exit(64);
        }
    }
}

fn repl() {
    let mut vm = vm::VM::init();

    const PROMPT: &str = "PlutomV0.01>> ";
    let mut input = String::new();
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();

        io::stdin()
            .read_line(&mut input)
            .expect("InputError: Failed reading input.");
        vm.interpret(&input.trim());
        input.clear();
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
    let result = vm.interpret(&source);
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
