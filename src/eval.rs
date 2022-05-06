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

impl Evaluator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            stack: Vec::new(),
            ip: 0,
            bp: 0,
        }
    }

    pub fn eval(&mut self, mut code: Vec<Instruction>) -> Result<Value, String> {
        use crate::value::Value::*;
        self.ip = self.code.len();
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
                    assert!(self.stack.len() >= 1);
                    if let Value::Ptr(addr) = self.stack.pop().unwrap() {
                        self.stack.push(self.stack[addr].clone());
                    } else {
                        unreachable!()
                    }
                }
                Operation::Store => {
                    assert!(self.stack.len() >= 2);
                    let value = self.stack.pop().unwrap();
                    let maybe_addr = self.stack.pop().unwrap();
                    if let Value::Ptr(addr) = maybe_addr {
                        // println!("Storing {} => {}", value, addr);
                        self.stack[addr] = value;
                        self.stack.push(value);
                    } else {
                        unreachable!("addr was {}", maybe_addr)
                    }
                }
                Operation::Reserve => {
                    if let Value::PtrOffset(l) = self.code[self.ip].values[0] {
                        // println!("Reserving {}", l);
                        for _ in 0..l {
                            self.stack.push(Value::Null);
                        }
                    } else {
                        unreachable!()
                    }
                }
                Operation::Rotate => {
                    if let Value::PtrOffset(num) = self.code[self.ip].values[0] {
                        assert!(num > 2 && self.stack.len() >= num as usize);

                        let val = *self.stack.last().unwrap();

                        let idx = self.stack.len() - num as usize;
                        for i in (idx..(self.stack.len() - 1)).rev() {
                            self.stack[i + 1] = self.stack[i];
                        }
                        self.stack[idx] = val;
                    } else {
                        unreachable!()
                    }
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
                    if let Value::PtrOffset(o) = self.code[self.ip].values[0] {
                        self.stack.push(Value::Ptr((self.bp as isize + o) as usize))
                    } else {
                        unreachable!()
                    }
                }
                Operation::Jump => {
                    if let Value::Ptr(p) = self.code[self.ip].values[0] {
                        self.ip = p;
                    } else {
                        unreachable!(
                            "Non Ptr in value for argument: {}",
                            self.code[self.ip].values[0]
                        );
                    }
                    continue;
                }
                Operation::JumpRel => {
                    if let Value::PtrOffset(o) = self.code[self.ip].values[0] {
                        self.ip = (self.ip as isize + o) as usize;
                    } else {
                        unreachable!(
                            "Non PtrOffset in value for argument: {}",
                            self.code[self.ip].values[0]
                        );
                    }
                    continue;
                }
                Operation::JumpIf => {
                    assert!(self.stack.len() >= 1);
                    let cond = self.stack.pop().unwrap();
                    if let Value::Bool(b) = cond {
                        if b != 0 {
                            if let Value::Ptr(p) = self.code[self.ip].values[0] {
                                self.ip = p;
                            } else {
                                unreachable!(
                                    "Non Ptr in value for argument: {}",
                                    self.code[self.ip].values[0]
                                );
                            }
                            continue;
                        }
                    }
                }
                Operation::JumpRelIf => {
                    assert!(self.stack.len() >= 1);
                    let cond = self.stack.pop().unwrap();
                    if let Value::Bool(b) = cond {
                        if b != 0 {
                            if let Value::PtrOffset(o) = self.code[self.ip].values[0] {
                                self.ip = (self.ip as isize + o) as usize;
                            } else {
                                unreachable!(
                                    "Non PtrOffset in value for argument: {}",
                                    self.code[self.ip].values[0]
                                );
                            }
                            continue;
                        }
                    }
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
