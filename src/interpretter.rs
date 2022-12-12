use crate::{
    compiler::Compiler,
    eval::{Evaluator, ReturnState},
    lex::Lexer,
    memory::CODE,
    operation::MachineOperation,
    parser,
    typecheck::TypeChecker,
    value::Value,
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
                MachineOperation::CallKnownSize(addr, size) => {
                    eprint!(
                        " ({}, {})",
                        self.compiler
                            .code
                            .rev_functions
                            .get(&(addr - CODE))
                            .unwrap(),
                        size
                    )
                }
                MachineOperation::CallBuiltin(_addr) => {}
                _ => {}
            }
            eprintln!();
        }
    }

    pub fn get_string(&self, idx: usize) -> String {
        self.evaluator.memory.strings[idx].clone()
    }

    pub fn print_value(&self, value: Value) {
        match value {
            Value::Uninitialized => (),
            Value::Str(idx) => println!("{}", self.get_string(idx)),
            _ => println!("{}", value),
        }
    }

    pub fn interpret_lexer(
        &mut self,
        label: String,
        lexer: Lexer,
    ) -> Result<(Value, ReturnState), Box<dyn std::error::Error>> {
        let tree = parser::parse(lexer);
        let typedtree = self.typechecker.typecheck(tree)?;
        if self.typecheck_only {
            return Ok((Value::Uninitialized, ReturnState::Evaluated));
        }
        let (code, strings, entry) =
            self.compiler
                .compile(label, &typedtree, &self.typechecker.system);
        if cfg!(debug_assertions) && self.debug {
            self.typechecker.system.print_types();
            self.print_code(&code);
        }
        Ok(self.evaluator.eval(code, strings, entry, self.debug)?)
    }

    pub fn interpret_stdin(&mut self) -> Result<(Value, ReturnState), Box<dyn std::error::Error>> {
        let mut lines = io::BufReader::new(io::stdin()).lines().map(|s| s.unwrap());
        self.interpret_lexer(
            "__main__".to_string(),
            Lexer::from_lines("<stdin>".into(), &mut lines),
        )
    }

    pub fn interpret_file(
        &mut self,
        label: String,
        filename: String,
    ) -> Result<(Value, ReturnState), Box<dyn std::error::Error>> {
        let file = File::open(&filename).map_err(|e| format!("{}: {}", filename, e))?;
        let md = file
            .metadata()
            .map_err(|e| format!("{}: {}", filename, e))?;
        if md.is_dir() {
            Err(format!("{}: Is a directory", filename))?;
        }
        let mut lines = io::BufReader::new(file).lines().map(|s| s.unwrap());
        self.interpret_lexer(label, Lexer::from_lines(filename, &mut lines))
    }

    pub fn interpret_cmd(
        &mut self,
        label: String,
        cmd: String,
    ) -> Result<(Value, ReturnState), Box<dyn std::error::Error>> {
        let lexer = Lexer::new("<cmdline>".into(), cmd);
        self.interpret_lexer(label, lexer)
    }
}
