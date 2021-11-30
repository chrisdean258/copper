mod builtins;
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
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut typecheck_only = false;
    let mut file_or_cmd: Option<&str> = None;
    let mut is_cmd = false;
    for arg in args[1..].iter() {
        if arg == "-t" || arg == "--typecheck" {
            typecheck_only = true;
        } else if arg == "-c" {
            is_cmd = true;
        } else {
            file_or_cmd = Some(arg);
            break;
        }
    }

    let rv = match file_or_cmd {
        Some(cmd) if is_cmd => eval_cmd(cmd, typecheck_only),
        Some(filename) => eval_file(filename, typecheck_only),
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
    let parser = parser::Parser::new(lexer);
    let mut tree = parser.parse()?;
    let mut typechecker = typecheck::TypeChecker::new();
    typechecker.typecheck(&tree)?;
    if !typecheck_only {
        let mut evaluator = eval::Evaluator::new();
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn repl(typecheck_only: bool) {
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
                        println!("{}", s);
                        continue;
                    }
                };
                match typechecker.typecheck(&tree) {
                    Ok(_) => (),
                    Err(s) => {
                        println!("{}", s);
                        continue;
                    }
                }
                if !typecheck_only {
                    let val = evaluator.eval(&mut tree);
                    match val {
                        Ok(eval::Value::Null) => (),
                        Ok(t) => println!("{}", t),
                        Err(s) => println!("{}", s),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => {
                println!("quit");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}

fn eval_file(filename: &str, typecheck_only: bool) -> Result<(), String> {
    let mut lines = read_lines(&filename).map(|s| s.unwrap());
    let lexer = lex::Lexer::new(&filename, &mut lines);
    let parser = parser::Parser::new(lexer);
    let mut tree = parser.parse()?;
    let mut typechecker = typecheck::TypeChecker::new();
    typechecker.typecheck(&tree)?;

    if !typecheck_only {
        let mut evaluator = eval::Evaluator::new();
        evaluator.eval(&mut tree)?;
    }
    Ok(())
}

fn read_lines<P>(filename: &P) -> io::Lines<io::BufReader<File>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).expect("No such file");
    io::BufReader::new(file).lines()
}
