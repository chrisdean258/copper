#![allow(dead_code)]
use crate::operation::Operation;
use crate::typesystem::*;
use crate::value::Value;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    pub active_functions: Vec<Function>,
    pub finished_functions: Vec<Instruction>,
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

    pub fn close_function(&mut self) -> usize {
        assert!(self.active_functions.len() > 0);
        let mut f = self.active_functions.pop().unwrap();
        let rv = self.finished_functions.len();
        self.finished_functions.append(&mut f.code);
        rv
    }

    pub fn emit(&mut self, op: Operation, types: Vec<Type>, values: Vec<Value>) -> usize {
        assert!(op.is_machineop(), "{}", op);
        assert!(self.active_functions.len() > 0); // If not this is a bug
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, types, values });
        code.len() - 1
    }

    pub fn code(&self) -> Vec<Instruction> {
        self.finished_functions.clone()
    }

    pub fn next_function_relative_addr(&self) -> usize {
        assert!(self.active_functions.len() > 0); // If not this is a bug
        self.active_functions.last().unwrap().code.len()
    }

    pub fn prep_function_call(&mut self) -> usize {
        self.emit(Operation::PrepCall, vec![], vec![])
    }

    pub fn call(&mut self) -> usize {
        self.emit(Operation::Call, vec![], vec![])
    }

    pub fn local_ref(&mut self, number: isize) -> usize {
        self.push(Value::PtrOffset(number));
        self.emit(Operation::RefFrame, vec![], vec![])
    }

    pub fn reserve(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit(Operation::Reserve, vec![], vec![])
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.emit(Operation::Push, vec![], vec![value])
    }

    pub fn pop(&mut self) -> usize {
        self.emit(Operation::Pop, vec![], vec![])
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

    pub fn return_(&mut self) -> usize {
        self.emit(Operation::Return, vec![], vec![])
    }

    pub fn crash(&mut self) -> usize {
        self.emit(Operation::Crash, vec![], vec![])
    }

    pub fn backpatch_jump(&mut self, jump_addr: usize, to: usize) {
        assert!(self.active_functions.len() >= 1);
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: Operation::Push,
            types: vec![],
            values: vec![Value::Ptr(to)],
        };
    }

    pub fn backpatch_jump_rel(&mut self, jump_addr: usize, to: isize) {
        assert!(self.active_functions.len() >= 1);
        let offset = to - jump_addr as isize;
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: Operation::Push,
            types: vec![],
            values: vec![Value::PtrOffset(offset)],
        };
    }

    pub fn jump(&mut self) -> usize {
        self.emit(Operation::Jump, vec![], vec![])
    }

    pub fn jump_to(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit(Operation::Jump, vec![], vec![])
    }

    pub fn jumpif(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit(Operation::JumpIf, vec![], vec![])
    }

    pub fn jump_relative_if(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize - 1;
        self.push(Value::PtrOffset(offset));
        self.emit(Operation::JumpRelIf, vec![], vec![])
    }

    pub fn jump_relative(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize;
        self.push(Value::PtrOffset(offset));
        self.emit(Operation::JumpRel, vec![], vec![])
    }

    pub fn rotate(&mut self, how_many: usize) -> usize {
        self.push(Value::Count(how_many));
        self.emit(Operation::Rotate, vec![], vec![])
    }

    pub fn swap(&mut self) -> usize {
        self.emit(Operation::Swap, vec![], vec![])
    }
}
