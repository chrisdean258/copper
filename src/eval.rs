#![allow(dead_code)]
use crate::builtins::BuiltinFunction;
use crate::code_builder::Instruction;
use crate::memory::Memory;
use crate::operation::Operation;
// use crate::typesystem;
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<Instruction>,
    pub memory: Memory,
    builtin_table: Vec<BuiltinFunction>,
    ip: usize,
    bp: usize,
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

    pub fn eval(
        &mut self,
        mut code: Vec<Instruction>,
        mut strings: Vec<String>,
        entry: usize,
    ) -> Result<Value, String> {
        self.memory.add_strings(&mut strings);
        macro_rules! pop {
            ($typ:path) => {
                match self.memory.pop() {
                    $typ(a) => a,
                    t => unreachable!("Trying to pop {} found {:?}", stringify!($typ), t),
                }
            };
        }

        macro_rules! do_binop {
            ($op:tt, $($t1:ident, $t2:ident => $to:ident),+ $(,)?) => {
                let a = self.memory.pop();
                let b = self.memory.pop();
                self.memory.push(match (a, b) {
                    $(
                        (Value::$t1(aa), Value::$t2(bb)) => Value::$to(bb $op aa),
                    )+
                    _ => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!(op), b),
                })
            };
        }
        macro_rules! do_comparison {
            ($op:tt, $($t1:ident, $t2:ident),+ $(,)?) => {
                let a = self.memory.pop();
                let b = self.memory.pop();
                self.memory.push(match (a, b) {
                    $((Value::$t1(aa), Value::$t2(bb)) => Value::Bool(if bb $op aa {1} else {0}),)+
                    _ => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!(op), b),
                })
            };
        }

        macro_rules! do_unop {
            ($op:tt, $($t:ident),+ $(,)?) => {
                let a = self.memory.pop();
                self.memory.push(match a {
                    $(Value::$t(aa) => Value::$t($op aa),)+
                    _ => unreachable!("Trying to apply binop {} {:?}", stringify!(op), a),
                })
            };
        }
        self.ip = self.code.len() + entry;
        // for (i, instr) in code.iter().enumerate() { eprintln!("{:05x}: {}", i + Self::CODE, instr); }
        self.code.append(&mut code);
        loop {
            // eprintln!("Stack: {:?}", self.memory.stack);
            // eprint!("IP: 0x{:08x}:  ", self.ip);
            // eprint!("{:20}  ", self.code[self.ip - Self::CODE].to_string());
            // eprint!("BP: 0x{:08x}     ", self.bp);
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
                    let addr = pop!(Value::Ptr);
                    self.memory.push(self.memory[addr]);
                }
                Operation::Store => {
                    let value = self.memory.pop();
                    let addr = pop!(Value::Ptr);
                    self.memory[addr] = value;
                    self.memory.push(value);
                }
                Operation::Reserve => {
                    let size = pop!(Value::Count);
                    self.memory.reserve(size);
                }
                Operation::Rotate => {
                    let num = pop!(Value::Count);
                    self.memory.rotate(num);
                }
                Operation::Dup => self.memory.dup(),
                Operation::Swap => self.memory.swap(),
                Operation::RefFrame => {
                    let o = pop!(Value::PtrOffset);
                    self.memory
                        .push(Value::Ptr((self.bp as isize + o) as usize));
                }
                Operation::Jump => {
                    self.ip = pop!(Value::Ptr);
                    continue;
                }
                Operation::JumpRel => {
                    let o = pop!(Value::PtrOffset);
                    self.ip = (self.ip as isize + o) as usize;
                    continue;
                }
                Operation::JumpIf => {
                    let addr = pop!(Value::Ptr);
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        self.ip = addr;
                        continue;
                    }
                }
                Operation::JumpRelIf => {
                    let offset = pop!(Value::PtrOffset);
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        self.ip = (self.ip as isize + offset) as usize;
                        continue;
                    }
                }
                Operation::Return => {
                    let rv = self.memory.pop();
                    self.memory.truncate_stack(self.bp);
                    self.bp = pop!(Value::Ptr);
                    self.ip = pop!(Value::Ptr);
                    self.memory.push(rv);
                    continue;
                }
                Operation::Call => {
                    let ip = pop!(Value::Ptr);
                    let num_args = pop!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    self.memory[bp - 1] = Value::Ptr(self.bp);
                    self.memory[bp - 2] = Value::Ptr(self.ip + 1);
                    self.bp = bp;
                    self.ip = ip;
                    if ip < Self::CODE {
                        let builtin_idx = ip - Self::BUILTIN_CODE;
                        let rv = (self.builtin_table[builtin_idx].func)(self, self.bp, num_args);
                        self.memory.truncate_stack(self.bp);
                        self.bp = pop!(Value::Ptr);
                        self.ip = pop!(Value::Ptr);
                        self.memory.push(rv);
                    }
                    continue;
                }
                Operation::BoolOr => {
                    do_binop!(|, Bool, Bool => Bool);
                }
                Operation::BoolXor => {
                    do_binop!(^, Bool, Bool => Bool);
                }
                Operation::BoolAnd => {
                    do_binop!(&, Bool, Bool => Bool);
                }
                Operation::BitOr => {
                    do_binop!(|,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                Operation::BitXor => {
                    do_binop!(^,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                Operation::BitAnd => {
                    do_binop!(&,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                Operation::CmpGE => {
                    do_comparison!(>=,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                Operation::CmpGT => {
                    do_comparison!(>,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                Operation::CmpLE => {
                    do_comparison!(<=,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                Operation::CmpLT => {
                    do_comparison!(<,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                Operation::CmpEq => {
                    do_comparison!(==,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                        Bool, Bool,
                    );
                }
                Operation::CmpNotEq => {
                    do_comparison!(!=,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                        Bool, Bool,
                    );
                }
                Operation::BitShiftLeft => {
                    do_binop!(<<,
                        Int, Int => Int,
                        Char, Char => Char,
                        // Char, Int => Char,
                    );
                }
                Operation::BitShiftRight => {
                    do_binop!(>>,
                        Int, Int => Int,
                        Char, Char => Char,
                        // Char, Int => Char,
                    );
                }
                Operation::Minus => {
                    do_binop!(-,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                Operation::Plus => {
                    let a = self.memory.pop();
                    let b = self.memory.pop();
                    let val = match (a, b) {
                        (Value::Int(aa), Value::Int(bb)) => Value::Int(bb + aa),
                        (Value::Char(aa), Value::Char(bb)) => Value::Char(bb + aa),
                        (Value::Float(aa), Value::Float(bb)) => Value::Float(bb + aa),
                        (Value::Str(aa), Value::Str(bb)) => Value::Str(self.memory.alloc_string(
                            format!("{}{}", self.memory.strings[bb], self.memory.strings[aa]),
                        )),
                        _ => {
                            unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!(op), b)
                        }
                    };
                    // println!("Got this value from binop plus {:?}", val);
                    self.memory.push(val);
                }
                Operation::Times => {
                    do_binop!(*,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                Operation::Mod => {
                    do_binop!(%,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                Operation::Div => {
                    do_binop!(/,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                Operation::BoolNot => {
                    let a = self.memory.pop();
                    self.memory.push(match a {
                        Value::Bool(aa) => Value::Bool(1 - aa),
                        _ => unreachable!("Trying to apply binop !{:?}", a),
                    })
                }
                Operation::BitNot => {
                    do_unop!(!, Int, Char);
                }
                t => unreachable!("{}", t),
            }
            self.ip += 1;
        }
        Ok(Value::Null)
    }
}
