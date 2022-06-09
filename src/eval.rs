#![allow(dead_code)]
use crate::builtins::BuiltinFunction;
use crate::memory::{Memory, BUILTIN_CODE, CODE, STACK};
use crate::operation::MachineOperation;
// use crate::typesystem;
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<MachineOperation>,
    pub memory: Memory,
    builtin_table: Vec<BuiltinFunction>,
    ip: usize,
    bp: usize,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            memory: Memory::new(),
            builtin_table: BuiltinFunction::get_table(),
            ip: CODE,
            bp: STACK,
        }
    }

    #[allow(clippy::assign_op_pattern)]
    pub fn eval(
        &mut self,
        code: Vec<MachineOperation>,
        mut strings: Vec<String>,
        entry: usize,
    ) -> Result<Value, String> {
        self.memory.add_strings(&mut strings);

        macro_rules! as_type {
            ($ex:expr, $typ:path) => {
                match $ex {
                    $typ(a) => a,
                    t => unreachable!("Trying to interpret {:?} as {}", t, stringify!($typ)),
                }
            };
        }

        macro_rules! pop {
            ($typ:path) => {
                as_type!(self.memory.pop(), $typ)
            };
        }

        macro_rules! last {
            ($typ:path) => {
                as_type!(self.memory.last().clone(), $typ)
            };
        }

        macro_rules! do_binop {
            ($op:tt, $($t1:ident, $t2:ident => $to:ident),+ $(,)?) => {
                let a = self.memory.pop();
                match (a, self.memory.last_mut()) {
                    $( (Value::$t1(aa), Value::$t2(ref mut bb)) => {*bb = *bb $op aa ; })+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }
        macro_rules! do_comparison {
            ($op:tt, $($t1:ident, $t2:ident),+ $(,)?) => {
                let a = self.memory.pop();
                *self.memory.last_mut() = match (a, self.memory.last()) {
                    $((Value::$t1(aa), Value::$t2(bb)) => Value::Bool(if *bb $op aa {1} else {0}),)+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }

        macro_rules! do_unop {
            ($op:tt, $($t:ident),+ $(,)?) => {
                match self.memory.last_mut() {
                    $(Value::$t(ref mut aa) => {*aa = $op *aa;})+
                    a => unreachable!("Trying to apply binop {} {:?}", stringify!($op), a),
                }
            };
        }
        self.ip = entry;
        // for (i, instr) in code.iter().enumerate() {
        // eprintln!("0x{:08x}: {}", i + CODE, instr);
        // }
        self.code = code;
        while self.ip < self.code.len() + CODE {
            // eprintln!("Stack: {:?}", self.memory.stack);
            // eprint!("IP: 0x{:08x}:  ", self.ip);
            // eprint!("{:23}  ", self.code[self.ip - CODE].to_string());
            // eprint!("BP: 0x{:08x}     ", self.bp);
            match self.code[self.ip - CODE] {
                MachineOperation::Nop => (),
                MachineOperation::Crash => {
                    break;
                }
                MachineOperation::ConditionalFail => {
                    if pop!(Value::Bool) != 0 {
                        return Err(
                            "Conditional Failure point hit. Currently only index OOB".into()
                        );
                    }
                }
                MachineOperation::Push(v) => self.memory.push(v),
                MachineOperation::Inplace(v) => *self.memory.last_mut() = v,
                MachineOperation::Pop => {
                    self.memory.pop();
                }
                MachineOperation::Load => {
                    let addr = last!(Value::Ptr);
                    *self.memory.last_mut() = self.memory[addr];
                }
                MachineOperation::LoadLocal(o) => {
                    let ptr = (self.bp as isize + o) as usize;
                    self.memory.push(self.memory[ptr]);
                }
                MachineOperation::Store => {
                    let value = self.memory.pop();
                    let addr = last!(Value::Ptr);
                    self.memory[addr] = value;
                    *self.memory.last_mut() = value;
                }
                MachineOperation::FastStore => {
                    let value = self.memory.pop();
                    let addr = pop!(Value::Ptr);
                    self.memory[addr] = value;
                }
                MachineOperation::StoreN(num) => {
                    let dst = as_type!(self.memory[self.memory.stack_top() - num - 1], Value::Ptr);
                    let src = self.memory.stack_top() - num;
                    self.memory.memcpy(dst, src, num);
                    self.memory
                        .truncate_stack(self.memory.stack_top() - num - 1);
                }
                MachineOperation::LoadN(num) => {
                    let src = pop!(Value::Ptr);
                    let dst = self.memory.stack_top();
                    self.memory.reserve(num);
                    self.memory.memcpy(dst, src, num);
                }
                MachineOperation::Alloc => {
                    let val = last!(Value::Count);
                    let addr = self.memory.malloc(val);
                    *self.memory.last_mut() = Value::Ptr(addr);
                }
                MachineOperation::Reserve(size) => {
                    self.memory.reserve(size);
                }
                MachineOperation::Rotate(num) => {
                    self.memory.rotate(num);
                }
                MachineOperation::Dup => self.memory.dup(),
                MachineOperation::Swap => self.memory.swap(),
                MachineOperation::RefFrame(o) => {
                    self.memory
                        .push(Value::Ptr((self.bp as isize + o) as usize));
                }
                MachineOperation::Jump(ip) => {
                    self.ip = ip;
                    continue;
                }
                MachineOperation::JumpRel(offset) => {
                    self.ip = (self.ip as isize + offset) as usize;
                    continue;
                }
                MachineOperation::JumpIf(ip) => {
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        self.ip = ip;
                        continue;
                    }
                }
                MachineOperation::JumpRelIf(offset) => {
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        self.ip = (self.ip as isize + offset) as usize;
                        continue;
                    }
                }
                MachineOperation::Return => {
                    let rv = self.memory.pop();
                    self.memory.truncate_stack(self.bp);
                    self.bp = pop!(Value::Ptr);
                    self.ip = last!(Value::Ptr);
                    *self.memory.last_mut() = rv;
                    continue;
                }
                MachineOperation::Call => {
                    let ip = pop!(Value::Ptr);
                    let num_args = pop!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    self.memory[bp - 1] = Value::Ptr(self.bp);
                    self.memory[bp - 2] = Value::Ptr(self.ip + 1);
                    self.bp = bp;
                    self.ip = ip;
                    if ip < CODE {
                        let builtin_idx = ip - BUILTIN_CODE;
                        let rv = (self.builtin_table[builtin_idx].func)(self, self.bp, num_args);
                        self.memory.truncate_stack(self.bp);
                        self.bp = pop!(Value::Ptr);
                        self.ip = pop!(Value::Ptr);
                        self.memory.push(rv);
                    }
                    continue;
                }
                MachineOperation::BoolOr => {
                    do_binop!(|, Bool, Bool => Bool);
                }
                MachineOperation::BoolXor => {
                    do_binop!(^, Bool, Bool => Bool);
                }
                MachineOperation::BoolAnd => {
                    do_binop!(&, Bool, Bool => Bool);
                }
                MachineOperation::BitOr => {
                    do_binop!(|,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                MachineOperation::BitXor => {
                    do_binop!(^,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                MachineOperation::BitAnd => {
                    do_binop!(&,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                MachineOperation::CmpGE => {
                    do_comparison!(>=,
                        Count, Count,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                MachineOperation::CmpGT => {
                    do_comparison!(>,
                        Count, Count,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                MachineOperation::CmpLE => {
                    do_comparison!(<=,
                        Count, Count,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                MachineOperation::CmpLT => {
                    do_comparison!(<,
                        Count, Count,
                        Float, Float,
                        Int, Int,
                        Char, Char,
                    );
                }
                MachineOperation::CmpEq => {
                    let a = self.memory.pop();
                    *self.memory.last_mut() = match (a, self.memory.last()) {
                        (Value::Count(aa), Value::Count(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        (Value::Float(aa), Value::Float(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        (Value::Int(aa), Value::Int(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        (Value::Char(aa), Value::Char(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        (Value::Bool(aa), Value::Bool(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        // (Value::Null, Value::Null) => Value::Bool(1),
                        // (Value::Null, Value::None(_)) => Value::Bool(1),
                        // (Value::None(_), Value::Null) => Value::Bool(1),
                        (Value::None(aa), Value::None(bb)) => {
                            Value::Bool(if *bb == aa { 1 } else { 0 })
                        }
                        // (Value::Null, _) => Value::Bool(0),
                        // (_, Value::Null) => Value::Bool(0),
                        (Value::None(_), _) => Value::Bool(0),
                        (_, Value::None(_)) => Value::Bool(0),
                        (_, b) => {
                            unreachable!("Trying to apply binop {:?} == {:?}", a, b)
                        }
                    };
                }
                MachineOperation::CmpNotEq => {
                    let a = self.memory.pop();
                    let b = *self.memory.last();
                    *self.memory.last_mut() = match (a, b) {
                        (Value::Count(aa), Value::Count(bb)) => {
                            Value::Bool(if bb != aa { 1 } else { 0 })
                        }
                        (Value::Float(aa), Value::Float(bb)) => {
                            Value::Bool(if bb != aa { 1 } else { 0 })
                        }
                        (Value::Int(aa), Value::Int(bb)) => {
                            Value::Bool(if bb != aa { 1 } else { 0 })
                        }
                        (Value::Char(aa), Value::Char(bb)) => {
                            Value::Bool(if bb != aa { 1 } else { 0 })
                        }
                        (Value::Bool(aa), Value::Bool(bb)) => {
                            Value::Bool(if bb != aa { 1 } else { 0 })
                        }
                        // (Value::Null, Value::Null) => Value::Bool(0),
                        // (Value::Null, Value::None(_)) => Value::Bool(0),
                        // (Value::None(_), Value::Null) => Value::Bool(0),
                        (Value::None(aa), Value::None(bb)) => {
                            Value::Bool(if bb == aa { 0 } else { 1 })
                        }
                        // (Value::Null, _) => Value::Bool(1),
                        // (_, Value::Null) => Value::Bool(1),
                        (Value::None(_), _) => Value::Bool(1),
                        (_, Value::None(_)) => Value::Bool(1),
                        _ => {
                            unreachable!("Trying to apply binop {:?} != {:?}", a, b)
                        }
                    };
                }
                MachineOperation::BitShiftLeft => {
                    do_binop!(<<,
                        Int, Int => Int,
                        Char, Char => Char,
                        // Char, Int => Char,
                    );
                }
                MachineOperation::BitShiftRight => {
                    do_binop!(>>,
                        Int, Int => Int,
                        Char, Char => Char,
                        // Char, Int => Char,
                    );
                }
                MachineOperation::Minus => {
                    do_binop!(-,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                MachineOperation::Plus => {
                    let a = self.memory.pop();
                    let b = *self.memory.last();
                    *self.memory.last_mut() = match (a, b) {
                        (Value::PtrOffset(aa), Value::Ptr(bb)) => {
                            Value::Ptr((bb as isize + aa) as usize)
                        }
                        (Value::Int(aa), Value::Ptr(bb)) => Value::Ptr((bb as i64 + aa) as usize),
                        (Value::Ptr(aa), Value::Int(bb)) => Value::Ptr((bb + aa as i64) as usize),
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
                }
                MachineOperation::Times => {
                    do_binop!(*,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                MachineOperation::Mod => {
                    do_binop!(%,
                        Int, Int => Int,
                        Char, Char => Char,
                    );
                }
                MachineOperation::Div => {
                    do_binop!(/,
                        Int, Int => Int,
                        Char, Char => Char,
                        Float, Float => Float
                    );
                }
                MachineOperation::BoolNot => {
                    *self.memory.last_mut() = match self.memory.last() {
                        Value::Bool(aa) => Value::Bool(1 - aa),
                        a => unreachable!("Trying to apply binop !{:?}", a),
                    }
                }
                MachineOperation::BitNot => {
                    do_unop!(!, Int, Char);
                }
            }
            self.ip += 1;
        }
        Ok(Value::Uninitialized)
    }
}
