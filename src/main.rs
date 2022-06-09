mod allocator;
mod builtins;
mod code_builder;
mod compiler;
mod eval;
mod interpretter;
mod lex;
mod location;
mod memory;
mod operation;
mod optimizer;
mod parser;
mod typecheck;
mod typesystem;
mod value;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;

fn main() {
    std::process::exit(real_main() as i32)
}

fn find_stdlib() -> String {
    format!("{}/git/copper/stdlib/stdlib.cu", env::var("HOME").unwrap())
}

fn real_main() -> i64 {
    let args: Vec<String> = env::args().collect();
    let mut typecheck_only = false;
    let mut file_or_cmd: Option<&str> = None;
    let mut is_cmd = false;
    let mut use_stdin = false;
    for arg in args[1..].iter() {
        if arg == "-t" || arg == "--typecheck" {
            typecheck_only = true;
        } else if arg == "-c" {
            is_cmd = true;
        } else if arg == "--stdin" {
            use_stdin = true;
        } else {
            file_or_cmd = Some(arg);
            break;
        }
    }

    let stdlib = find_stdlib();

    let mut intp = interpretter::Interpretter::new(typecheck_only);
    if let Err(s) = intp.interpret_file("__stdlib__".to_string(), &stdlib) {
        eprintln!("{} (ERROR IN STDLIB)", s);
        return 2;
    }

    let rv = match file_or_cmd {
        Some(cmd) if is_cmd => intp.interpret_cmd("__main__".to_string(), cmd),
        Some(filename) => intp.interpret_file("__main__".to_string(), filename),
        None if use_stdin => intp.interpret_stdin(),
        None => {
            return repl(intp);
        }
    };

    match rv {
        Err(s) => {
            eprintln!("{}", s);
            1
        }
        Ok(v) => match v {
            value::Value::Int(i) => i,
            _ => 0,
        },
    }
}

fn repl(mut intp: interpretter::Interpretter) -> i64 {
    let mut rl = Editor::<()>::new();
    let mut lineno: usize = 1;

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = lex::Lexer::new_with_lineno(
                    "<stdin>",
                    vec![format!("print({{ {} }})", line)].into_iter(),
                    lineno,
                );
                match intp.interpret_lexer("__main__".to_string(), lexer) {
                    Ok(value::Value::Uninitialized) => (),
                    Ok(a) => println!("{}", a),
                    Err(s) => println!("{}", s),
                }
                lineno += 1;
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => {
                println!("quit");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }
    0
}
