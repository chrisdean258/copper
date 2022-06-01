use crate::memory;
use crate::operation::MachineOperation;
// use crate::typesystem::*;
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
    pub op: MachineOperation,
    // pub types: Vec<Type>,
    pub value: Value,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.op))?;

        // f.write_fmt(format_args!(
        // " ({})",
        // self.types
        // .iter()
        // .map(|t| format!("{}", t))
        // .collect::<Vec<String>>()
        // .join(", ")
        // ))?;

        match self.value {
            Value::Uninitialized => Ok(()),
            _ => f.write_fmt(format_args!(" ({})", self.value)),
        }
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
        debug_assert!(!self.active_functions.is_empty());
        self.close_function_and_patch(&[])
    }

    pub fn close_function_and_patch(&mut self, patchpoints: &[usize]) -> usize {
        debug_assert!(!self.active_functions.is_empty());
        let mut f = self.active_functions.pop().unwrap();
        let rv = self.finished_functions.len();

        for pp in patchpoints {
            f.code[*pp] = Instruction {
                op: MachineOperation::Push,
                // types: Vec::new(),
                value: Value::Ptr(rv + memory::CODE),
            }
        }

        self.finished_functions.append(&mut f.code);
        rv + memory::CODE
    }

    pub fn emit(&mut self, op: MachineOperation, value: Value) -> usize {
        debug_assert!(!self.active_functions.is_empty());
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, value });
        code.len() - 1
    }

    pub fn emit_code(&mut self, op: MachineOperation) -> usize {
        self.emit(op, Value::Uninitialized)
    }

    pub fn code(&self) -> Vec<Instruction> {
        self.finished_functions.clone()
    }

    pub fn next_function_relative_addr(&self) -> usize {
        debug_assert!(!self.active_functions.is_empty());
        self.active_functions.last().unwrap().code.len()
    }

    pub fn call(&mut self) -> usize {
        self.emit_code(MachineOperation::Call)
    }

    pub fn local_ref(&mut self, number: isize) -> usize {
        self.push(Value::PtrOffset(number));
        self.emit_code(MachineOperation::RefFrame)
    }

    pub fn global_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::STACK))
    }

    pub fn builtin_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::BUILTIN_CODE))
    }

    pub fn reserve(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit_code(MachineOperation::Reserve)
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.emit(MachineOperation::Push, value)
    }

    pub fn pop(&mut self) -> usize {
        self.emit_code(MachineOperation::Pop)
    }

    pub fn dup(&mut self) -> usize {
        self.emit_code(MachineOperation::Dup)
    }

    pub fn load(&mut self) -> usize {
        self.emit_code(MachineOperation::Load)
    }

    pub fn store(&mut self) -> usize {
        self.emit_code(MachineOperation::Store)
    }

    pub fn store_n(&mut self, count: usize) -> usize {
        self.push(Value::Count(count));
        self.emit_code(MachineOperation::StoreN)
    }

    pub fn return_(&mut self) -> usize {
        self.emit_code(MachineOperation::Return)
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit_code(MachineOperation::Alloc)
    }

    pub fn _crash(&mut self) -> usize {
        self.emit_code(MachineOperation::Crash)
    }

    pub fn conditional_fail(&mut self) -> usize {
        self.emit_code(MachineOperation::ConditionalFail)
    }

    #[allow(dead_code)]
    pub fn backpatch_jump(&mut self, jump_addr: usize, to: usize) {
        debug_assert!(!self.active_functions.is_empty());
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: MachineOperation::Push,
            // types: vec![],
            value: Value::Ptr(to),
        };
    }

    pub fn backpatch_jump_rel(&mut self, jump_addr: usize, to: isize) {
        debug_assert!(!self.active_functions.is_empty());
        let offset = to - jump_addr as isize;
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: MachineOperation::Push,
            // types: vec![],
            value: Value::PtrOffset(offset),
        };
    }

    #[allow(dead_code)]
    pub fn jump(&mut self) -> usize {
        self.emit_code(MachineOperation::Jump)
    }

    #[allow(dead_code)]
    pub fn jump_to(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit_code(MachineOperation::Jump)
    }

    #[allow(dead_code)]
    pub fn jumpif(&mut self, location: usize) -> usize {
        self.push(Value::Ptr(location));
        self.emit_code(MachineOperation::JumpIf)
    }

    pub fn jump_relative_if(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize - 1;
        self.push(Value::PtrOffset(offset));
        self.emit_code(MachineOperation::JumpRelIf)
    }

    pub fn jump_relative(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize;
        self.push(Value::PtrOffset(offset));
        self.emit_code(MachineOperation::JumpRel)
    }

    pub fn rotate(&mut self, how_many: usize) -> usize {
        self.push(Value::Count(how_many));
        self.emit_code(MachineOperation::Rotate)
    }

    pub fn swap(&mut self) -> usize {
        self.emit_code(MachineOperation::Swap)
    }
}
