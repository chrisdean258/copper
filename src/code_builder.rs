use crate::memory;
use crate::operation::MachineOperation;
// use crate::typesystem::*;
use crate::value::Value;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct CodeBuilder {
    pub active_functions: Vec<Function>,
    pub finished_functions: Vec<MachineOperation>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub code: Vec<MachineOperation>,
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
            f.code[*pp] = MachineOperation::Push(Value::Ptr(rv + memory::CODE));
        }

        self.finished_functions.append(&mut f.code);
        rv + memory::CODE
    }

    pub fn emit(&mut self, op: MachineOperation) -> usize {
        debug_assert!(!self.active_functions.is_empty());
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(op);
        code.len() - 1
    }

    pub fn code(&self) -> Vec<MachineOperation> {
        self.finished_functions.clone()
    }

    pub fn next_function_relative_addr(&self) -> usize {
        debug_assert!(!self.active_functions.is_empty());
        self.active_functions.last().unwrap().code.len()
    }

    pub fn call(&mut self) -> usize {
        self.emit(MachineOperation::Call)
    }

    pub fn local_ref(&mut self, number: isize) -> usize {
        self.emit(MachineOperation::RefFrame(number))
    }

    pub fn global_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::STACK))
    }

    pub fn builtin_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + memory::BUILTIN_CODE))
    }

    pub fn reserve(&mut self, size: usize) -> usize {
        // if size == 0 {
        // self.active_functions.last().unwrap().code.len() - 1
        if size == 1 {
            self.push(Value::Uninitialized)
        } else {
            self.emit(MachineOperation::Reserve(size))
        }
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.emit(MachineOperation::Push(value))
    }

    pub fn pop(&mut self) -> usize {
        self.emit(MachineOperation::Pop)
    }

    pub fn dup(&mut self) -> usize {
        self.emit(MachineOperation::Dup)
    }

    pub fn load(&mut self) -> usize {
        self.emit(MachineOperation::Load)
    }

    pub fn load_n(&mut self, count: usize) -> usize {
        self.emit(MachineOperation::LoadN(count))
    }

    pub fn store(&mut self) -> usize {
        self.emit(MachineOperation::Store)
    }

    pub fn store_n(&mut self, count: usize) -> usize {
        self.emit(MachineOperation::StoreN(count))
    }

    pub fn return_(&mut self) -> usize {
        self.emit(MachineOperation::Return)
    }

    pub fn alloc(&mut self, size: usize) -> usize {
        self.push(Value::Count(size));
        self.emit(MachineOperation::Alloc)
    }

    pub fn _crash(&mut self) -> usize {
        self.emit(MachineOperation::Crash)
    }

    pub fn conditional_fail(&mut self) -> usize {
        self.emit(MachineOperation::ConditionalFail)
    }

    #[allow(dead_code)]
    pub fn backpatch_jump(&mut self, jump_addr: usize, to: usize) {
        debug_assert!(!self.active_functions.is_empty());
        match &mut self.active_functions.last_mut().unwrap().code[jump_addr] {
            MachineOperation::Jump(ref mut ip) => *ip = to,
            MachineOperation::JumpIf(ref mut ip) => *ip = to,
            _ => unreachable!(),
        }
    }

    pub fn backpatch_jump_rel(&mut self, jump_addr: usize, to: isize) {
        debug_assert!(!self.active_functions.is_empty());
        let offset = to - jump_addr as isize;
        match &mut self.active_functions.last_mut().unwrap().code[jump_addr] {
            MachineOperation::JumpRel(ref mut o) => *o = offset,
            MachineOperation::JumpRelIf(ref mut o) => *o = offset,
            _ => unreachable!(),
        }
    }

    #[allow(dead_code)]
    pub fn jump(&mut self, location: usize) -> usize {
        self.emit(MachineOperation::Jump(location))
    }

    #[allow(dead_code)]
    pub fn jumpif(&mut self, location: usize) -> usize {
        self.emit(MachineOperation::JumpIf(location))
    }

    pub fn jump_relative_if(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize;
        self.emit(MachineOperation::JumpRelIf(offset))
    }

    pub fn jump_relative(&mut self, location: isize) -> usize {
        let offset = location - self.next_function_relative_addr() as isize;
        self.emit(MachineOperation::JumpRel(offset))
    }

    pub fn rotate(&mut self, how_many: usize) -> usize {
        self.emit(MachineOperation::Rotate(how_many))
    }

    pub fn swap(&mut self) -> usize {
        self.emit(MachineOperation::Swap)
    }
}
