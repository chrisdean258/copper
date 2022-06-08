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
mod parser;
mod typecheck;
mod typesystem;
mod value;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};

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
            return repl(typecheck_only);
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

fn stdlib_env() -> Result<(typecheck::TypeChecker, compiler::Compiler, eval::Evaluator), String> {
    let mut typechecker = typecheck::TypeChecker::new();
    let mut evaluator = eval::Evaluator::new();
    let mut compiler = compiler::Compiler::new();
    let file = File::open(find_stdlib()).map_err(|e| format!("{}: {}", find_stdlib(), e))?;
    let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
    let stdlib_lexer = lex::Lexer::new(&find_stdlib(), &mut lines);
    let mut stdlib_tree = parser::parse(stdlib_lexer)?;
    typechecker.typecheck(&mut stdlib_tree)?;
    let (code, strings, entry) =
        compiler.compile("stdlib".to_string(), &stdlib_tree, &typechecker.system);

    evaluator.eval(code, strings, entry)?;
    Ok((typechecker, compiler, evaluator))
}

fn repl(typecheck_only: bool) -> i64 {
    let mut rl = Editor::<()>::new();
    let mut lineno: usize = 1;
    let mut typechecker;
    let _compiler;
    let _evaluator;

    match stdlib_env() {
        Ok((t, c, e)) => {
            typechecker = t;
            _compiler = c;
            _evaluator = e;
        }
        Err(e) => {
            eprintln!("{}", e);
            return 1;
        }
    }

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = lex::Lexer::new_with_lineno("<stdin>", vec![line].into_iter(), lineno);
                lineno += 1;
                let mut tree = match parser::parse(lexer) {
                    Ok(t) => t,
                    Err(s) => {
                        eprintln!("{}", s);
                        continue;
                    }
                };
                if let Err(s) = typechecker.typecheck(&mut tree) {
                    eprintln!("{}", s);
                    continue;
                };
                if typecheck_only {
                    continue;
                }
                // let _val = _evaluator.eval(code);
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
