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

macro_rules! _pop_stack {
    ($self:ident, $type:expr) => {{
        $self.memory.pop_as($type)
    }};
}

macro_rules! value_as {
    ($val:expr, $type:path) => {{
        match $val {
            $type(t) => t,
            t => unreachable!("Unexpected {} on stack. Expected {}", t, stringify!($type)),
        }
    }};
}

macro_rules! pop_stack {
    ($self:ident, $type:path, $typ:ty) => {{
        let val = _pop_stack!($self, $type(0 as $typ));
        value_as!(val, $type)
    }};
    ($self:ident, $type:path) => {{
        let val = _pop_stack!($self, $type(0));
        value_as!(val, $type)
    }};
}

macro_rules! do_comparison {
    ($self:ident, $op:expr, $($ts:ident, $typ:ty => $v:ident),+ $(,)?) => {
        let mut run = false;
        $(if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = pop_stack!($self, Value::$v, $typ);
            let b = pop_stack!($self, Value::$v, $typ);
            $self.memory.push(Value::Bool(if $op(a, b) { 1 } else { 0 }));
            run = true;
        })+
        if !run { panic!("Unsupported type in comparison");}
    };
}

macro_rules! do_binop {
    ($self:ident, $op:tt, $($ts:ident, $pop:tt => $push:tt),+ $(,)?) => {
        let mut run = false;
        $(if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = $self.memory.$pop();
            let b = $self.memory.$pop();
            $self.memory.$push(b $op a);
            run = true;
        })+
        if !run { panic!("Unsupported type in binop");}
    };
}

macro_rules! do_unop {
    ($self:ident, $($ts:ident => $v:ident),+; $op:tt) => {
        let mut run = false;
        $(if $self.code[$self.ip - Self::CODE].types[0] == typesystem::$ts {
            let a = pop_stack!($self, Value::$v);
            $self.memory.push($v($op a));
            run = true;
        })+
        if !run { panic!("Unsupported type in unop");}
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
        use crate::value::Value::*;
        self.ip = self.code.len() + entry;
        self.code.append(&mut code);
        loop {
            // eprintln!("stack: {:?}", self.stack);
            // eprint!("ip: 0x{:x}: ", self.ip);
            // eprint!("bp: 0x{:x}: ", self.bp);
            // eprint!("{:<20}", self.code[self.ip - Self::CODE].to_string());
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
                        (self.builtin_table[builtin_idx].func)(self, num_args);
                        let rv = self.memory.pop();
                        self.memory.truncate_stack(self.bp);
                        self.bp = self.memory.pop() as usize;
                        self.ip = self.memory.pop() as usize;
                        self.memory.push_enc(rv);
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
                    do_comparison!(self, |a, b| b >= a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                    );
                }
                Operation::CmpGT => {
                    do_comparison!(self, |a, b| b > a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                    );
                }
                Operation::CmpLE => {
                    do_comparison!(self, |a, b| b <= a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                    );
                }
                Operation::CmpLT => {
                    do_comparison!(self, |a, b| b < a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                    );
                }
                Operation::CmpEq => {
                    do_comparison!(self, |a, b| b == a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                        BOOL, u8 => Bool
                    );
                }
                Operation::CmpNotEq => {
                    do_comparison!(self, |a, b| b != a,
                        FLOAT, f64 => Float,
                        CHAR, u8 => Char,
                        INT, i64 => Int,
                        BOOL, u8 => Bool
                    );
                }
                Operation::BitShiftLeft => {
                    if self.code[self.ip].types[0] == typesystem::CHAR {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_char();
                        self.memory.push(Value::Char(b << a));
                    } else if self.code[self.ip].types[0] == typesystem::INT {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_int();
                        self.memory.push(Value::Int(b << a));
                    } else {
                        panic!("Weird type");
                    }
                }
                Operation::BitShiftRight => {
                    if self.code[self.ip].types[0] == typesystem::CHAR {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_char();
                        self.memory.push(Value::Char(b << a));
                    } else if self.code[self.ip].types[0] == typesystem::INT {
                        let a = self.memory.pop_int();
                        let b = self.memory.pop_int();
                        self.memory.push(Value::Int(b << a));
                    } else {
                        panic!("Weird type");
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
                    do_unop!(self, INT => Int, CHAR => Char; !);
                }
                t => unreachable!("{}", t),
            }
            self.ip += 1;
        }
        Ok(Value::Null)
    }
}
