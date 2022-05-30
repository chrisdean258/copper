#![allow(dead_code)]
use crate::memory;
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
            name,
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
    pub value: Option<Value>,
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
            self.value
                .iter()
                .map(|t| format!("{:?}", t))
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
        assert!(!self.active_functions.is_empty());
        let mut f = self.active_functions.pop().unwrap();
        let rv = self.finished_functions.len();
        self.finished_functions.append(&mut f.code);
        rv + memory::CODE
    }

    pub fn close_function_and_patch(&mut self, patchpoints: &[usize]) -> usize {
        assert!(!self.active_functions.is_empty());
        let mut f = self.active_functions.pop().unwrap();
        let rv = self.finished_functions.len();

        for pp in patchpoints {
            f.code[*pp] = Instruction {
                op: Operation::Push,
                types: Vec::new(),
                value: Some(Value::Ptr(rv + memory::CODE)),
            }
        }

        self.finished_functions.append(&mut f.code);
        rv + memory::CODE
    }

    pub fn emit(&mut self, op: Operation, types: Vec<Type>, value: Option<Value>) -> usize {
        assert!(op.is_machineop(), "{}", op);
        assert!(!self.active_functions.is_empty());
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, types, value });
        code.len() - 1
    }

    pub fn code(&self) -> Vec<Instruction> {
        self.finished_functions.clone()
    }

    pub fn next_function_relative_addr(&self) -> usize {
        assert!(!self.active_functions.is_empty());
        self.active_functions.last().unwrap().code.len()
    }

    pub fn prep_function_call(&mut self) -> usize {
        self.emit(Operation::PrepCall, vec![], None)
    }

    pub fn call(&mut self) -> usize {
        self.emit(Operation::Call, vec![], None)
    }

    pub fn local_ref(&mut self, number: isize) -> usize {
        self.push(Value::PtrOffset(number));
        self.emit(Operation::RefFrame, vec![], None)
    }

    pub fn global_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::STACK))
    }

    pub fn builtin_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::BUILTIN_CODE))
    }

    pub fn code_ref(&mut self, addr: usize) -> usize {
        self.push(Value::Ptr(addr))
    }

    pub fn reserve(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit(Operation::Reserve, vec![], None)
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.emit(Operation::Push, vec![], Some(value))
    }

    pub fn pop(&mut self) -> usize {
        self.emit(Operation::Pop, vec![], None)
    }

    pub fn dup(&mut self) -> usize {
        self.emit(Operation::Dup, vec![], None)
    }

    pub fn load(&mut self) -> usize {
        self.emit(Operation::Load, vec![], None)
    }

    pub fn store(&mut self) -> usize {
        self.emit(Operation::Store, vec![], None)
    }

    pub fn store_n(&mut self, count: usize) -> usize {
        self.push(Value::Count(count));
        self.emit(Operation::StoreN, vec![], None)
    }

    pub fn return_(&mut self) -> usize {
        self.emit(Operation::Return, vec![], None)
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit(Operation::Alloc, vec![], None)
    }

    pub fn crash(&mut self) -> usize {
        self.emit(Operation::Crash, vec![], None)
    }

    pub fn conditional_fail(&mut self) -> usize {
        self.emit(Operation::ConditionalFail, vec![], None)
    }

    pub fn backpatch_jump(&mut self, jump_addr: usize, to: usize) {
        assert!(!self.active_functions.is_empty());
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: Operation::Push,
            types: vec![],
            value: Some(Value::Ptr(to)),
        };
    }

    pub fn backpatch_jump_rel(&mut self, jump_addr: usize, to: isize) {
        assert!(!self.active_functions.is_empty());
        let offset = to - jump_addr as isize;
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: Operation::Push,
            types: vec![],
            value: Some(Value::PtrOffset(offset)),
        };
    }

    pub fn jump(&mut self) -> usize {
        self.emit(Operation::Jump, vec![], None)
    }

    pub fn jump_to(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit(Operation::Jump, vec![], None)
    }

    pub fn jumpif(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit(Operation::JumpIf, vec![], None)
    }

    pub fn jump_relative_if(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize - 1;
        self.push(Value::PtrOffset(offset));
        self.emit(Operation::JumpRelIf, vec![], None)
    }

    pub fn jump_relative(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize;
        self.push(Value::PtrOffset(offset));
        self.emit(Operation::JumpRel, vec![], None)
    }

    pub fn rotate(&mut self, how_many: usize) -> usize {
        self.push(Value::Count(how_many));
        self.emit(Operation::Rotate, vec![], None)
    }

    pub fn swap(&mut self) -> usize {
        self.emit(Operation::Swap, vec![], None)
    }
}
