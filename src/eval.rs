#![allow(dead_code)]
use crate::builtins::BuiltinFunction;
use crate::code_builder::Instruction;
use crate::memory::Memory;
use crate::operation::Operation;
use crate::typesystem;
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<Instruction>,
    pub memory: Memory,
    builtin_table: Vec<BuiltinFunction>,
    ip: usize,
    bp: usize,
}

macro_rules! do_comparison {
    ($self:ident, $op:tt, $($ts:ident, $pop:tt => $push:tt),+ $(,)?) => {
        if false { }
        $(else if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = $self.memory.$pop();
            let b = $self.memory.$pop();
            $self.memory.push_bool(if b $op a { 1 } else { 0 });
        })+
    };
}

macro_rules! do_binop {
    ($self:ident, $op:tt, $($ts:ident, $pop:tt => $push:tt),+ $(,)?) => {
        if false { }
        $(else if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = $self.memory.$pop();
            let b = $self.memory.$pop();
            $self.memory.$push(b $op a);
        })+
    };
}

macro_rules! do_unop {
    ($self:ident, $op:tt, $($ts:ident, $pop:tt => $push:tt),+) => {
        if false { }
        $(else if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = $self.memory.$pop();
            $self.memory.$push($op a);
        })+
    };
}

impl Evaluator {
    pub const STACK_BOTTOM: usize = 0x1000000;
    pub const CODE: usize = 0x100000;
    pub const BUILTIN_CODE: usize = 0x10000;

    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            memory: Memory::new(),
            builtin_table: BuiltinFunction::get_table(),
            ip: Self::CODE,
            bp: Self::STACK_BOTTOM,
        }
    }

    pub fn eval(&mut self, mut code: Vec<Instruction>, entry: usize) -> Result<Value, String> {
        self.ip = self.code.len() + entry;
        self.code.append(&mut code);
        loop {
            match self.code[self.ip - Self::CODE].op {
                Operation::Nop => (),
                Operation::Crash => {
                    break;
                }
                Operation::Push => self
                    .memory
                    .push(self.code[self.ip - Self::CODE].value.unwrap()),
                Operation::Pop => {
                    self.memory.pop();
                }
                Operation::Load => {
                    let addr = self.memory.pop() as usize;
                    self.memory.push_enc(self.memory[addr]);
                }
                Operation::Store => {
                    let value = self.memory.pop();
                    let addr = self.memory.pop() as usize;
                    self.memory[addr] = value;
                    self.memory.push_enc(value);
                }
                Operation::Reserve => {
                    let size = self.memory.pop() as usize;
                    self.memory.reserve(size);
                }
                Operation::Rotate => {
                    let num = self.memory.pop() as usize;
                    self.memory.rotate(num);
                }
                Operation::Dup => self.memory.dup(),
                Operation::Swap => self.memory.swap(),
                Operation::RefFrame => {
                    let o = self.memory.pop_int() as isize;
                    self.memory.push_enc((self.bp as isize + o) as u64)
                }
                Operation::Jump => {
                    self.ip = self.memory.pop() as usize;
                    continue;
                }
                Operation::JumpRel => {
                    let o = self.memory.pop_int() as isize;
                    self.ip = (self.ip as isize + o) as usize;
                    continue;
                }
                Operation::JumpIf => {
                    let addr = self.memory.pop() as usize;
                    let cond = self.memory.pop_bool();
                    if cond != 0 {
                        self.ip = addr;
                        continue;
                    }
                }
                Operation::JumpRelIf => {
                    let offset = self.memory.pop_int() as isize;
                    let cond = self.memory.pop_bool();
                    if cond != 0 {
                        self.ip = (self.ip as isize + offset) as usize;
                        continue;
                    }
                }
                Operation::Return => {
                    let rv = self.memory.pop();
                    self.memory.truncate_stack(self.bp);
                    self.bp = self.memory.pop() as usize;
                    self.ip = self.memory.pop() as usize;
                    self.memory.push_enc(rv);
                    continue;
                }
                Operation::Call => {
                    let ip = self.memory.pop() as usize;
                    let num_args = self.memory.pop() as usize;
                    let bp = self.memory.stack_top() - num_args;
                    self.memory[bp - 1] = self.bp as u64;
                    self.memory[bp - 2] = (self.ip + 1) as u64;
                    self.bp = bp;
                    self.ip = ip;
                    if ip < Self::CODE {
                        let builtin_idx = ip - Self::BUILTIN_CODE;
                        let rv = (self.builtin_table[builtin_idx].func)(self, self.bp, num_args);
                        self.memory.truncate_stack(self.bp);
                        self.bp = self.memory.pop() as usize;
                        self.ip = self.memory.pop() as usize;
                        self.memory.push_enc(rv as u64);
                    }
                    continue;
                }
                Operation::BoolOr => {
                    do_binop!(self, |, BOOL, pop_bool => push_bool);
                }
                Operation::BoolXor => {
                    do_binop!(self, ^, BOOL, pop_bool => push_bool);
                }
                Operation::BoolAnd => {
                    do_binop!(self, &, BOOL, pop_bool => push_bool);
                }
                Operation::BitOr => {
                    do_binop!(self, |,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::BitXor => {
                    do_binop!(self, ^,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::BitAnd => {
                    do_binop!(self, &,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::CmpGE => {
                    do_comparison!(self, >=,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::CmpGT => {
                    do_comparison!(self, > ,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::CmpLE => {
                    do_comparison!(self, <=,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::CmpLT => {
                    do_comparison!(self, <,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::CmpEq => {
                    do_comparison!(self, ==,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                        BOOL, pop_bool => push_bool,
                    );
                }
                Operation::CmpNotEq => {
                    do_comparison!(self, !=,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                        BOOL, pop_bool => push_bool,
                    );
                }
                Operation::BitShiftLeft => {
                    if self.code[self.ip].types[0] == typesystem::CHAR {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_char();
                        self.memory.push_char(b << a);
                    } else if self.code[self.ip].types[0] == typesystem::INT {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_int();
                        self.memory.push_int(b << a);
                    }
                }
                Operation::BitShiftRight => {
                    if self.code[self.ip].types[0] == typesystem::CHAR {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_char();
                        self.memory.push_char(b << a);
                    } else if self.code[self.ip].types[0] == typesystem::INT {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_int();
                        self.memory.push_int(b << a);
                    }
                }
                Operation::Minus => {
                    do_binop!(self, -,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::Plus => {
                    do_binop!(self, +,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::Times => {
                    do_binop!(self, *,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::Mod => {
                    do_binop!(self, %,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::Div => {
                    do_binop!(self, /,
                        FLOAT, pop_float => push_float,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char,
                    );
                }
                Operation::BoolNot => {
                    let a = self.memory.pop_bool();
                    self.memory.push_enc((1 - a) as u64);
                }
                Operation::BitNot => {
                    do_unop!(self, !,
                        INT, pop_int => push_int,
                        CHAR, pop_char => push_char);
                }
                t => unreachable!("{}", t),
            }
            self.ip += 1;
        }
        Ok(Value::Null)
    }
}
