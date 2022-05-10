#![allow(dead_code)]
use crate::eval::Evaluator;
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
    pub value: Option<Value>,
}

impl Instruction {
    // fn encode(&self) -> u64 {
    // let mut encoding = match self.op {
    // Operation::Push => {
    // return 1 << 63 | self.value[0].encode();
    // }
    // Operation::Nop => 0,
    // Operation::Crash => 1,
    // Operation::Pop => 2,
    // Operation::Dup => 3,
    // Operation::Store => 4,
    // Operation::Load => 5,
    // Operation::Rotate => 6,
    // Operation::Swap => 7,
    // Operation::Reserve => 8,
    // Operation::RefFrame => 9,
    // Operation::Jump => 10,
    // Operation::JumpRel => 11,
    // Operation::JumpIf => 12,
    // Operation::JumpRelIf => 13,
    // Operation::Call => 14,
    // Operation::PrepCall => 15,
    // Operation::Return => 16,
    // Operation::BoolOr => 17,
    // Operation::BoolXor => 18,
    // Operation::BoolAnd => 19,
    // Operation::BitOr => 20,
    // Operation::BitXor => 21,
    // Operation::BitAnd => 22,
    // Operation::CmpGE => 23,
    // Operation::CmpGT => 24,
    // Operation::CmpLE => 25,
    // Operation::CmpLT => 26,
    // Operation::CmpEq => 27,
    // Operation::CmpNotEq => 28,
    // Operation::BitShiftLeft => 29,
    // Operation::BitShiftRight => 30,
    // Operation::Minus => 31,
    // Operation::Plus => 32,
    // Operation::Times => 33,
    // Operation::Mod => 34,
    // Operation::Div => 35,
    // Operation::Equal => 36,
    // Operation::BoolNot => 37,
    // Operation::BitNot => 38,
    // _ => unreachable!(),
    // } << 56;
    // assert!(self.types.len() <= 2);
    // for (i, t) in self.types.iter().enumerate() {
    // encoding |= (t.encode() & 0xFFFF) << i * 16;
    // }
    // encoding
    // }

    // fn decode(op: u64) -> Self {
    // if (op & 1 << 63) != 0 {
    // return Instruction {
    // operation: Operation::Push,
    // types: Vec::new(),
    // values: vec![op & !(1<< 63)],
    // }
    // }
    // let mut op = match self.op {
    // Operation::Push => {
    // return 1 << 63 | self.values[0].encode();
    // }
    // Operation::Nop => 0,
    // Operation::Crash => 1,
    // Operation::Pop => 2,
    // Operation::Dup => 3,
    // Operation::Store => 4,
    // Operation::Load => 5,
    // Operation::Rotate => 6,
    // Operation::Swap => 7,
    // Operation::Reserve => 8,
    // Operation::RefFrame => 9,
    // Operation::Jump => 10,
    // Operation::JumpRel => 11,
    // Operation::JumpIf => 12,
    // Operation::JumpRelIf => 13,
    // Operation::Call => 14,
    // Operation::PrepCall => 15,
    // Operation::Return => 16,
    // Operation::BoolOr => 17,
    // Operation::BoolXor => 18,
    // Operation::BoolAnd => 19,
    // Operation::BitOr => 20,
    // Operation::BitXor => 21,
    // Operation::BitAnd => 22,
    // Operation::CmpGE => 23,
    // Operation::CmpGT => 24,
    // Operation::CmpLE => 25,
    // Operation::CmpLT => 26,
    // Operation::CmpEq => 27,
    // Operation::CmpNotEq => 28,
    // Operation::BitShiftLeft => 29,
    // Operation::BitShiftRight => 30,
    // Operation::Minus => 31,
    // Operation::Plus => 32,
    // Operation::Times => 33,
    // Operation::Mod => 34,
    // Operation::Div => 35,
    // Operation::Equal => 36,
    // Operation::BoolNot => 37,
    // Operation::BitNot => 38,
    // _ => unreachable!(),
    // } << 56;
    // assert!(self.types.len() <= 2);
    // for (i, t) in self.types.iter().enumerate() {
    // encoding |= (t.encode() & 0xFFFF) << i * 16;
    // }
    // encoding
    // Instruction {
    // op: Operation::Nop,
    // types: Vec::new(),
    // values: Vec::new(),
    // }
    // }
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
        rv + Evaluator::CODE
    }

    pub fn emit(&mut self, op: Operation, types: Vec<Type>, value: Option<Value>) -> usize {
        assert!(op.is_machineop(), "{}", op);
        assert!(self.active_functions.len() > 0); // If not this is a bug
        let code = &mut self.active_functions.last_mut().unwrap().code;
        code.push(Instruction { op, types, value });
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
        self.push(Value::Ptr(number + Evaluator::STACK_BOTTOM))
    }

    pub fn builtin_ref(&mut self, number: usize) -> usize {
        self.push(Value::Ptr(number + Evaluator::BUILTIN_CODE))
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

    pub fn return_(&mut self) -> usize {
        self.emit(Operation::Return, vec![], None)
    }

    pub fn crash(&mut self) -> usize {
        self.emit(Operation::Crash, vec![], None)
    }

    pub fn backpatch_jump(&mut self, jump_addr: usize, to: usize) {
        assert!(self.active_functions.len() >= 1);
        self.active_functions.last_mut().unwrap().code[jump_addr - 1] = Instruction {
            op: Operation::Push,
            types: vec![],
            value: Some(Value::Ptr(to)),
        };
    }

    pub fn backpatch_jump_rel(&mut self, jump_addr: usize, to: isize) {
        assert!(self.active_functions.len() >= 1);
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
