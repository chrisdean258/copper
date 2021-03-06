use crate::{
    compiler::Compiler, eval::Evaluator, lex::Lexer, memory::CODE, operation::MachineOperation,
    parser, typecheck::TypeChecker, value::Value,
};
use std::{
    fs::File,
    io::{self, BufRead},
};

pub struct Interpretter {
    typechecker: TypeChecker,
    compiler: Compiler,
    evaluator: Evaluator,
    typecheck_only: bool,
    debug: bool,
}

impl Interpretter {
    pub fn new(typecheck_only: bool, debug: bool) -> Self {
        let mut typechecker = TypeChecker::new();
        let compiler = Compiler::new(&mut typechecker.system);
        let evaluator = Evaluator::new(&mut typechecker.system);
        Self {
            typechecker,
            evaluator,
            compiler,
            typecheck_only,
            debug,
        }
    }

    pub fn print_code(&self, code: &[MachineOperation]) {
        for (i, instr) in code.iter().enumerate() {
            if let Some(name) = self.compiler.code.rev_functions.get(&i) {
                eprintln!("{}:", name)
            }
            eprint!("\t0x{:08x}: {}", i + CODE, instr);
            match instr {
                MachineOperation::CallKnown(addr) => {
                    eprint!(
                        " ({})",
                        self.compiler
                            .code
                            .rev_functions
                            .get(&(addr - CODE))
                            .unwrap()
                    )
                }
                MachineOperation::CallBuiltin(_addr) => {}
                _ => {}
            }
            eprintln!();
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
        if cfg!(debug_assertions) && self.debug {
            self.print_code(&code);
        }
        self.evaluator.eval(code, strings, entry, self.debug)
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
