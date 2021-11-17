mod builtins;
mod copper_type;
mod eval;
mod lex;
mod location;
mod parser;
mod reiter;
mod typecheck;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 2 {
        eval_file(&args[1]);
    } else {
        repl()
    }
}

fn repl() {
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
                let tree = match parser.parse() {
                    Ok(t) => t,
                    Err(s) => {
                        println!("{}", s);
                        continue;
                    }
                };
                match typechecker.typecheck(&tree) {
                    Ok(_) => (),
                    Err(s) => println!("{}", s),
                }
                let val = evaluator.eval(&tree);
                match val {
                    Ok(eval::Value::Null) => (),
                    Ok(t) => println!("{}", t),
                    Err(s) => println!("{}", s),
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

fn eval_file(filename: &str) {
    let mut lines = read_lines(&filename).map(|s| s.unwrap());
    let lexer = lex::Lexer::new(&filename, &mut lines);
    let parser = parser::Parser::new(lexer);
    let tree = parser.parse().unwrap();
    let mut typechecker = typecheck::TypeChecker::new();
    typechecker.typecheck(&tree).unwrap();

    let mut evaluator = eval::Evaluator::new();
    evaluator.eval(&tree).unwrap();
}

fn read_lines<P>(filename: &P) -> io::Lines<io::BufReader<File>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename).expect("No such file");
    io::BufReader::new(file).lines()
}

#[cfg(test)]
mod tests {
    use std::fs;

    fn check_output(fp1: &str, fp2: &str) -> bool {
        fp1 == fs::read_to_string(fp2).unwrap()
    }

    #[test]
    fn run_script_tests() {
        let paths = fs::read_dir("tests").unwrap();
        for path in paths {
            let s = path.unwrap().file_name().into_string().unwrap();
            if s.ends_with(".cu") {
                assert!(check_output(&s, &format!("tests/{}.out", &s)));
            }
        }
    }
}
