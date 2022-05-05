#![allow(dead_code)]
use crate::operation::Operation;
use crate::typesystem::*;
use crate::value::Value;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    pub active_functions: Vec<Function>,
    pub finished_functions: Vec<Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub code: Vec<Instruction>,
}

impl Function {
    fn new(name: String) -> Self {
        Self {
            name: name,
            code: Vec::new(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}:\n", self.name))?;
        for instr in self.code.iter() {
            f.write_fmt(format_args!("\t{}\n", instr))?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub op: Operation,
    pub types: Vec<Type>,
    pub values: Vec<Value>,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.op))?;

        f.write_fmt(format_args!(
            " ({})",
            self.types
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(", ")
        ))?;

        f.write_fmt(format_args!(
            " ({})",
            self.values
                .iter()
                .map(|t| format!("{}", t))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            active_functions: Vec::new(),
            finished_functions: Vec::new(),
        }
    }

    pub fn open_function(&mut self, name: String) -> usize {
        self.active_functions.push(Function::new(name));
        self.active_functions.len() - 1
    }

    pub fn close_function(&mut self) {
        assert!(self.active_functions.len() > 0);
        let f = self.active_functions.pop().unwrap();
        self.finished_functions.push(f);
    }

    pub fn emit(&mut self, op: Operation, types: Vec<Type>, values: Vec<Value>) -> usize {
        assert!(op.is_machineop(), "{}", op);
        assert!(self.active_functions.len() > 0, "{}", op); // If not this is a bug
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, types, values });
        code.len() - 1
    }

    pub fn code(&self) -> Vec<Instruction> {
        let mut rv = Vec::new();
        for func in self.finished_functions.iter() {
            rv.append(&mut func.code.clone());
        }
        rv
    }

    pub fn local_ref(&mut self, number: usize) -> usize {
        self.emit(Operation::RefFrame, vec![], vec![Value::PtrOffset(number)])
    }

    pub fn reserve(&mut self, size: usize) -> usize {
        self.emit(Operation::Reserve, vec![], vec![Value::PtrOffset(size)])
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.emit(Operation::Push, vec![], vec![value])
    }

    pub fn dup(&mut self) -> usize {
        self.emit(Operation::Dup, vec![], vec![])
    }

    pub fn load(&mut self) -> usize {
        self.emit(Operation::Load, vec![], vec![])
    }

    pub fn store(&mut self) -> usize {
        self.emit(Operation::Store, vec![], vec![])
    }
}
