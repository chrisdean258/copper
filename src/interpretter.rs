use crate::compiler::Compiler;
use crate::eval::Evaluator;
use crate::lex::Lexer;
use crate::parser;
use crate::typecheck::TypeChecker;
use crate::value::Value;
use std::fs::File;
use std::io::{self, BufRead};

pub struct Interpretter {
    typechecker: TypeChecker,
    compiler: Compiler,
    evaluator: Evaluator,
    typecheck_only: bool,
}

impl Interpretter {
    pub fn new(typecheck_only: bool) -> Self {
        Self {
            typechecker: TypeChecker::new(),
            evaluator: Evaluator::new(),
            compiler: Compiler::new(),
            typecheck_only,
        }
    }

    pub fn interpret_lexer<T: Iterator<Item = String>>(
        &mut self,
        label: String,
        lexer: Lexer<T>,
    ) -> Result<Value, String> {
        let mut tree = parser::parse(lexer)?;
        self.typechecker.typecheck(&mut tree)?;
        if self.typecheck_only {
            return Ok(Value::Uninitialized);
        }
        let (code, strings, entry) = self
            .compiler
            .compile(label, &tree, &self.typechecker.system);
        self.evaluator.eval(code, strings, entry)
    }

    pub fn interpret_stdin(&mut self) -> Result<Value, String> {
        let mut lines = io::BufReader::new(io::stdin()).lines().map(|s| s.unwrap());
        self.interpret_lexer("__main__".to_string(), Lexer::new("<stdin>", &mut lines))
    }

    pub fn interpret_file(&mut self, label: String, filename: &str) -> Result<Value, String> {
        let file = File::open(filename).map_err(|e| format!("{}: {}", filename, e))?;
        let md = file
            .metadata()
            .map_err(|e| format!("{}: {}", filename, e))?;
        if md.is_dir() {
            return Err(format!("{}: Is a directory", filename));
        }
        let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
        self.interpret_lexer(label, Lexer::new(filename, &mut lines))
    }

    pub fn interpret_cmd(&mut self, label: String, cmd: &str) -> Result<Value, String> {
        let mut lines = vec![cmd.into()].into_iter();
        let lexer = Lexer::new("<cmdline>", &mut lines);
        self.interpret_lexer(label, lexer)
    }
}
