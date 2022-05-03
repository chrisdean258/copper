mod code_emitter;
mod eval;
mod lex;
mod location;
mod operation;
mod parser;
mod typecheck;
mod typesystem;
mod value;
mod vm;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};

// static STDLIB: &'static str = "/home/chris/git/copper/stdlib/stdlib.cu";

fn main() {
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

    let rv = match file_or_cmd {
        Some(cmd) if is_cmd => eval_cmd(cmd, typecheck_only),
        Some(filename) => eval_file(filename, typecheck_only),
        None if use_stdin => {
            match eval_stdin(typecheck_only) {
                Ok(_) => (),
                Err(s) => eprintln!("{}", s),
            }
            return;
        }
        None => {
            repl(typecheck_only);
            return;
        }
    };

    match rv {
        Err(s) => eprintln!("{}", s),
        _ => (),
    };
}

fn eval_cmd(cmd: &str, typecheck_only: bool) -> Result<(), String> {
    let mut lines = vec![cmd.into()].into_iter();
    let lexer = lex::Lexer::new("<cmdline>", &mut lines);

    let mut tree = parser::parse(lexer)?;

    let mut typechecker = typecheck::TypeChecker::new();
    typechecker
        .typecheck(&mut tree)
        .map_err(|s| s.to_string())?;
    if !typecheck_only {
        let mut evaluator = eval::Evaluator::new();
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn stdlib_env() -> Result<(typecheck::TypeChecker, eval::Evaluator), String> {
    let typechecker = typecheck::TypeChecker::new();
    let evaluator = eval::Evaluator::new();
    /* let file = File::open(STDLIB).map_err(|e| format!("{}: {}", STDLIB, e))?;
    let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
    let stdlib_lexer = lex::Lexer::new(STDLIB, &mut lines);
    let mut stdlib_tree = parser::parse(stdlib_lexer)?;
    typechecker
        .typecheck(&mut stdlib_tree)
        .map_err(|e| e.to_string())?;
    evaluator.eval(&mut stdlib_tree)?; */
    Ok((typechecker, evaluator))
}

fn repl(typecheck_only: bool) {
    let mut rl = Editor::<()>::new();
    let mut lineno: usize = 1;
    let mut typechecker;
    let mut evaluator;

    match stdlib_env() {
        Ok((t, e)) => {
            typechecker = t;
            evaluator = e;
        }
        Err(e) => {
            eprintln!("{}", e);
            return;
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
                match typechecker.typecheck(&mut tree) {
                    Ok(_) => (),
                    Err(s) => {
                        eprintln!("{}", s.to_string());
                        continue;
                    }
                }
                if typecheck_only {
                    continue;
                }
                let _val = evaluator.eval(&mut tree);
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
}

fn eval_lexer<T: Iterator<Item = String>>(
    lexer: lex::Lexer<T>,
    typecheck_only: bool,
) -> Result<(), String> {
    let (mut typechecker, mut evaluator) = stdlib_env()?;

    let mut tree = parser::parse(lexer)?;
    typechecker
        .typecheck(&mut tree)
        .map_err(|s| s.to_string())?;
    if !typecheck_only {
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn eval_stdin(typecheck_only: bool) -> Result<(), String> {
    let mut lines = io::BufReader::new(io::stdin()).lines().map(|s| s.unwrap());
    eval_lexer(lex::Lexer::new("<stdin>", &mut lines), typecheck_only)
}

fn eval_file(filename: &str, typecheck_only: bool) -> Result<(), String> {
    let file = match File::open(filename) {
        Ok(f) => f,
        Err(e) => return Err(format!("{}: {}", filename, e)),
    };
    let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
    eval_lexer(lex::Lexer::new(&filename, &mut lines), typecheck_only)
}
