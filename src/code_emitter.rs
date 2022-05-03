#![allow(dead_code)]
use crate::operation::Operation;
use crate::typesystem::*;
use crate::value::Value;

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    pub active_functions: Vec<Function>,
    pub finished_functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<String>,
    pub code: Vec<Instruction>,
}

impl Function {
    fn new(name: Option<String>) -> Self {
        Self {
            name: name,
            code: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    op: Operation,
    types: Vec<Type>,
    values: Vec<Value>,
}

impl CodeBuilder {
    pub fn new(main_name: String) -> Self {
        Self {
            active_functions: vec![Function::new(Some(main_name))],
            finished_functions: Vec::new(),
        }
    }

    pub fn open_function(&mut self, name: Option<String>) -> usize {
        self.active_functions.push(Function::new(name));
        self.active_functions.len() - 1
    }

    pub fn emit(&mut self, op: Operation, types: Vec<Type>, values: Vec<Value>) -> usize {
        assert!(op.is_machineop());
        assert!(self.active_functions.len() > 0); // If not this is a bug
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, types, values });
        code.len() - 1
    }
}
