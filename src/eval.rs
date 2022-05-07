#![allow(dead_code)]
use crate::code_emitter::Instruction;
use crate::operation::Operation;
use crate::value::Value;

// #[derive(Clone, Debug)]
// pub struct Object {}

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<Instruction>,
    pub stack: Vec<Value>,
    ip: usize,
    bp: usize,
}

macro_rules! pop_stack {
    ($self:ident, $type:path) => {{
        assert!($self.stack.len() >= 1);
        let val = $self.stack.pop().unwrap();
        match val {
            $type(t) => t,
            t => unreachable!("Unexpected {} on stack. Expected {}", t, stringify!($type)),
        }
    }};
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            stack: Vec::new(),
            ip: 0,
            bp: 0,
        }
    }

    pub fn eval(&mut self, mut code: Vec<Instruction>, entry: usize) -> Result<Value, String> {
        use crate::value::Value::*;
        self.ip = self.code.len() + entry;
        self.code.append(&mut code);
        while self.ip < self.code.len() {
            print!(
                "stack: {:?}\n{:05}: {:<20} ",
                self.stack,
                self.ip,
                self.code[self.ip].to_string(),
            );
            match self.code[self.ip].op {
                Operation::Nop => (),
                Operation::Crash => return Err("Crash Operation".to_string()),
                Operation::Push => self.stack.push(self.code[self.ip].values[0]),
                Operation::Pop => {
                    self.stack.pop();
                }
                Operation::Load => {
                    let addr = pop_stack!(self, Value::Ptr);
                    self.stack.push(self.stack[addr].clone());
                }
                Operation::Store => {
                    assert!(self.stack.len() >= 2);
                    let value = self.stack.pop().unwrap();
                    let addr = pop_stack!(self, Value::Ptr);
                    self.stack[addr] = value;
                    self.stack.push(value);
                }
                Operation::Reserve => {
                    let size = pop_stack!(self, Value::Count) + self.stack.len();
                    self.stack.resize(size, Value::Uninitialized);
                }
                Operation::Rotate => {
                    let num = pop_stack!(self, Value::Count);
                    assert!(num > 2 && self.stack.len() >= num);
                    let val = *self.stack.last().unwrap();
                    let idx = self.stack.len() - num as usize;
                    for i in (idx..(self.stack.len() - 1)).rev() {
                        self.stack[i + 1] = self.stack[i];
                    }
                    self.stack[idx] = val;
                }
                Operation::Dup => {
                    assert!(self.stack.len() >= 1);
                    self.stack.push(*self.stack.last().unwrap())
                }
                Operation::Swap => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(a);
                    self.stack.push(b);
                }
                Operation::RefFrame => {
                    let o = pop_stack!(self, PtrOffset);
                    self.stack.push(Value::Ptr((self.bp as isize + o) as usize))
                }
                Operation::Jump => {
                    self.ip = pop_stack!(self, Ptr);
                    continue;
                }
                Operation::JumpRel => {
                    let o = pop_stack!(self, PtrOffset);
                    self.ip = (self.ip as isize + o) as usize;
                    continue;
                }
                Operation::JumpIf => {
                    assert!(self.stack.len() >= 2);
                    let addr = pop_stack!(self, Value::Ptr);
                    let cond = pop_stack!(self, Value::Bool);
                    if cond != 0 {
                        self.ip = addr;
                        continue;
                    }
                }
                Operation::JumpRelIf => {
                    assert!(self.stack.len() >= 2);
                    let offset = pop_stack!(self, Value::PtrOffset);
                    let cond = pop_stack!(self, Value::Bool);
                    if cond != 0 {
                        self.ip = (self.ip as isize + offset) as usize;
                        continue;
                    }
                }
                Operation::Return => {
                    assert!(self.stack.len() >= 3);
                    let rv = self.stack.pop().unwrap();
                    self.stack.truncate(self.bp);
                    assert!(self.stack.len() >= 2);
                    self.bp = pop_stack!(self, Value::Ptr);
                    self.ip = pop_stack!(self, Value::Ptr);
                    self.stack.push(rv);
                    continue;
                }
                Operation::Call => {
                    assert!(self.stack.len() >= 2);
                    let ip = pop_stack!(self, Value::Ptr);
                    let num_args = pop_stack!(self, Value::Count);
                    let bp = self.stack.len() - num_args;
                    self.stack[bp - 1] = Value::Ptr(self.bp);
                    self.stack[bp - 2] = Value::Ptr(self.ip + 1);
                    self.bp = bp;
                    self.ip = ip;
                    continue;
                }
                Operation::BoolOr => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Bool(aa), Bool(bb)) => Bool(bb | aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BoolXor => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Bool(aa), Bool(bb)) => Bool(bb ^ aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BoolAnd => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Bool(aa), Bool(bb)) => Bool(bb & aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BitOr => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb | aa),
                        (Int(aa), Int(bb)) => Int(bb | aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BitXor => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb ^ aa),
                        (Int(aa), Int(bb)) => Int(bb ^ aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BitAnd => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb & aa),
                        (Int(aa), Int(bb)) => Int(bb & aa),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpGE => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb >= aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb >= aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb >= aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpGT => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb > aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb > aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb > aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpLE => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb <= aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb <= aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb <= aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpLT => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb < aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb < aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb < aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpEq => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb == aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb == aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb == aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::CmpNotEq => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Bool(if bb != aa { 1 } else { 0 }),
                        (Int(aa), Int(bb)) => Bool(if bb != aa { 1 } else { 0 }),
                        (Float(aa), Float(bb)) => Bool(if bb != aa { 1 } else { 0 }),
                        _ => unreachable!(),
                    })
                }
                Operation::BitShiftLeft => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb << aa),
                        (Int(aa), Char(bb)) => Char(bb << aa),
                        (Int(aa), Int(bb)) => Int(bb << aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BitShiftRight => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb >> aa),
                        (Int(aa), Char(bb)) => Char(bb >> aa as u8),
                        (Int(aa), Int(bb)) => Int(bb >> aa),
                        _ => unreachable!(),
                    })
                }
                Operation::Minus => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb - aa),
                        (Int(aa), Int(bb)) => Int(bb - aa),
                        (Float(aa), Float(bb)) => Float(bb - aa),
                        _ => unreachable!(),
                    })
                }
                Operation::Plus => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb + aa),
                        (Int(aa), Int(bb)) => Int(bb + aa),
                        (Float(aa), Float(bb)) => Float(bb + aa),
                        _ => unreachable!(),
                    })
                }
                Operation::Times => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Int(aa), Int(bb)) => Int(bb * aa),
                        (Float(aa), Float(bb)) => Float(bb * aa),
                        _ => unreachable!(),
                    })
                }
                Operation::Mod => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb % aa),
                        (Int(aa), Int(bb)) => Int(bb % aa),
                        _ => unreachable!(),
                    })
                }
                Operation::Div => {
                    assert!(self.stack.len() >= 2);
                    let a = self.stack.pop().unwrap();
                    let b = self.stack.pop().unwrap();
                    //TODO: Check for divide by 0
                    self.stack.push(match (a, b) {
                        (Char(aa), Char(bb)) => Char(bb / aa),
                        (Int(aa), Int(bb)) => Int(bb / aa),
                        (Float(aa), Float(bb)) => Float(bb / aa),
                        _ => unreachable!(),
                    })
                }
                Operation::BoolNot => {
                    assert!(self.stack.len() >= 1);
                    if let Value::Bool(b) = self.stack.pop().unwrap() {
                        self.stack.push(Value::Bool(1 - b));
                    } else {
                        unreachable!()
                    }
                }
                Operation::BitNot => {
                    assert!(self.stack.len() >= 1);
                    let val = self.stack.pop().unwrap();
                    self.stack.push(match val {
                        Value::Char(c) => Value::Char(!c),
                        Value::Int(b) => Value::Int(!b),
                        _ => unreachable!(),
                    })
                }
                t => unreachable!("{}", t),
            }
            self.ip += 1;
        }
        println!("stack: {:?}", self.stack);
        Ok(Value::Null)
    }
}
