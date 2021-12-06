mod builtins;
mod code_formatter;
mod eval;
mod lex;
mod location;
mod parser;
mod typecheck;
mod typesystem;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut typecheck_only = false;
    let mut format = false;
    let mut file_or_cmd: Option<&str> = None;
    let mut is_cmd = false;
    let mut use_stdin = false;
    for arg in args[1..].iter() {
        if arg == "-t" || arg == "--typecheck" {
            typecheck_only = true;
        } else if arg == "-f" || arg == "--format" {
            format = true;
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
        Some(cmd) if is_cmd => eval_cmd(cmd, format, typecheck_only),
        Some(filename) => eval_file(filename, format, typecheck_only),
        None if format || use_stdin => {
            match eval_stdin(format, typecheck_only) {
                Ok(_) => (),
                Err(s) => eprintln!("{}", s),
            }
            return;
        }
        None => {
            repl(format, typecheck_only);
            return;
        }
    };

    match rv {
        Err(s) => eprintln!("{}", s),
        _ => (),
    };
}

fn eval_cmd(cmd: &str, format: bool, typecheck_only: bool) -> Result<(), String> {
    let mut lines = vec![cmd.into()].into_iter();
    let lexer = lex::Lexer::new("<cmdline>", &mut lines);
    let parser = parser::Parser::new(lexer);
    let mut tree = parser.parse()?;
    if format {
        println!("{}", tree);
        return Ok(());
    }
    let mut typechecker = typecheck::TypeChecker::new();
    typechecker.typecheck(&mut tree)?;
    if !typecheck_only {
        let mut evaluator = eval::Evaluator::new();
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn repl(format: bool, typecheck_only: bool) {
    let mut rl = Editor::<()>::new();
    let mut lineno: usize = 1;
    let mut typechecker = typecheck::TypeChecker::new();
    let mut evaluator = eval::Evaluator::new();
    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let lexer = lex::Lexer::new_with_lineno("<stdin>", vec![line].into_iter(), lineno);
                lineno += 1;
                let parser = parser::Parser::new(lexer);
                let mut tree = match parser.parse() {
                    Ok(t) => t,
                    Err(s) => {
                        eprintln!("{}", s);
                        continue;
                    }
                };
                if format {
                    println!("{}", tree);
                    continue;
                }
                match typechecker.typecheck(&mut tree) {
                    Ok(_) => (),
                    Err(s) => {
                        eprintln!("{}", s);
                        continue;
                    }
                }
                if !typecheck_only {
                    let val = evaluator.eval(&mut tree);
                    match val {
                        Ok(eval::Value::Null) => (),
                        Ok(t) => println!("{}", t),
                        Err(s) => eprintln!("{}", s),
                    }
                }
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
    format: bool,
    typecheck_only: bool,
) -> Result<(), String> {
    let parser = parser::Parser::new(lexer);
    let mut tree = parser.parse()?;
    if format {
        println!("{}", tree);
        return Ok(());
    }

    let mut typechecker = typecheck::TypeChecker::new();
    typechecker.typecheck(&mut tree)?;

    if !typecheck_only {
        let mut evaluator = eval::Evaluator::new();
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn eval_stdin(format: bool, typecheck_only: bool) -> Result<(), String> {
    let mut lines = io::BufReader::new(io::stdin()).lines().map(|s| s.unwrap());
    eval_lexer(
        lex::Lexer::new("<stdin>", &mut lines),
        format,
        typecheck_only,
    )
}

fn eval_file(filename: &str, format: bool, typecheck_only: bool) -> Result<(), String> {
    let file = match File::open(filename) {
        Ok(f) => f,
        Err(_) => return Err(format!("{}: no such file", filename)),
    };
    let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
    eval_lexer(
        lex::Lexer::new(&filename, &mut lines),
        format,
        typecheck_only,
    )
}
