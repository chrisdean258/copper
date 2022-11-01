#![allow(dead_code)]
use crate::{
    builtins::BuiltinFunction,
    memory::{Memory, BUILTIN_CODE, CODE, STACK},
    operation::MachineOperation,
    typesystem::TypeSystem,
    value::Value,
};
use std::mem::{replace, swap};

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<MachineOperation>,
    pub memory: Memory,
    builtin_table: Vec<BuiltinFunction>,
    //ip: usize,
    reg: Value,
    bp: usize,
}

impl Evaluator {
    pub fn new(types: &mut TypeSystem) -> Self {
        Self {
            code: Vec::new(),
            memory: Memory::new(),
            builtin_table: BuiltinFunction::get_table(types),
            bp: STACK + 1,
            reg: Value::Uninitialized,
        }
    }

    #[allow(clippy::assign_op_pattern)]
    #[allow(unused_assignments)]
    pub fn eval(
        &mut self,
        code: Vec<MachineOperation>,
        mut strings: Vec<String>,
        mut ip: usize,
        debug: bool,
    ) -> Result<Value, String> {
        self.memory.add_strings(&mut strings);
        let mut reg = self.reg;
        let mut retstack = Vec::new();
        let mut retval = Value::Uninitialized;

        macro_rules! as_type {
            ($ex:expr, $typ:path) => {
                match $ex {
                    $typ(a) => a,
                    t => unreachable!("Trying to interpret {:?} as {}", t, stringify!($typ)),
                }
            };
        }

        macro_rules! inplace {
            ($typ:path) => {
                as_type!(reg, $typ)
            };
        }

        macro_rules! pop {
            ($typ:path) => {
                as_type!(pop!(), $typ)
            };
            () => {{
                replace(&mut reg, self.memory.pop())
            }};
        }

        macro_rules! push {
            ($val:expr) => {
                self.memory.push(reg);
                reg = $val;
            };
        }

        macro_rules! do_binop {
            ($op:tt, $($t1:ident, $t2:ident => $to:ident),+ $(,)?) => {
                let a = pop!();
                match (a, &mut reg) {
                    $( (Value::$t1(aa), Value::$t2(ref mut bb)) => {*bb = *bb $op aa;})+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }
        macro_rules! do_comparison {
            ($op:tt, $($t1:ident, $t2:ident),+ $(,)?) => {
                let a = pop!();
                reg = match (a, reg) {
                    $((Value::$t1(aa), Value::$t2(bb)) => Value::Bool(if bb $op aa {1} else {0}),)+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }

        macro_rules! do_unop {
            ($op:tt, $($t:ident),+ $(,)?) => {
                match &mut reg {
                    $(Value::$t(ref mut aa) => {*aa = $op *aa;})+
                    a => unreachable!("Trying to apply unop {} {:?}", stringify!($op), a),
                }
            };
        }
        self.code = code;
        while let Some(instr) = self.code.get(ip - CODE) {
            if cfg!(debug_assertions) && debug {
                eprintln!("Stack: {:?} {:?}", self.memory.stack, reg);
                eprint!("IP: 0x{:08x}:  ", ip);
                eprint!("{:23}  ", self.code[ip - CODE].to_string());
                eprint!("BP: 0x{:08x}     ", self.bp);
            }
            match instr {
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
                MachineOperation::Push(v) => {
                    push!(*v);
                }
                MachineOperation::Inplace(v) => reg = *v,
                MachineOperation::Pop => {
                    reg = self.memory.pop();
                }
                MachineOperation::PopAndSave => {
                    retval = pop!();
                }
                MachineOperation::Load => {
                    let addr = inplace!(Value::Ptr);
                    if let Some(v) = self.memory.get(addr) {
                        reg = *v
                    } else if cfg!(debug_assertions) {
                        panic!("Reading out of bounds 0x{:x}", addr);
                    }
                }
                MachineOperation::LoadLocal(o) => {
                    let ptr = (self.bp as isize + o) as usize;
                    self.memory.push(reg);
                    if let Some(v) = self.memory.get(ptr) {
                        reg = *v;
                    }
                }
                MachineOperation::Store => {
                    // intentional direct stack manipulation for optiization
                    let addr = as_type!(self.memory.pop(), Value::Ptr);
                    if let Some(v) = self.memory.get_mut(addr) {
                        *v = reg;
                    } else if cfg!(debug_assertions) {
                        panic!("Writing out of bounds 0x{:x}", addr);
                    }
                }
                MachineOperation::FastStore => {
                    let addr = as_type!(self.memory.pop(), Value::Ptr);
                    self.memory[addr] = reg;
                    reg = self.memory.pop();

                    // This is the old impl. Haven't worked out if this new one is 100% yet
                    //let value = pop!();
                    // do not try to optimize to a pop! as we might write to that spot in memory
                    //let addr = as_type!(reg, Value::Ptr);
                    //self.memory[addr] = value;
                    //reg = self.memory.pop();
                }
                MachineOperation::StoreN(num) => {
                    //intentional direct stack manipulation
                    self.memory.push(reg);
                    let dst = as_type!(self.memory[self.memory.stack_top() - num - 1], Value::Ptr);
                    let src = self.memory.stack_top() - num;
                    self.memory.memcpy(dst, src, *num);
                    self.memory
                        .truncate_stack(self.memory.stack_top() - num - 1);
                    reg = self.memory.pop();
                }
                MachineOperation::LoadN(num) => {
                    //intentional direct stack manipulation
                    let src = inplace!(Value::Ptr);
                    let dst = self.memory.stack_top();
                    self.memory.reserve(*num);
                    self.memory.memcpy(dst, src, *num);
                    reg = self.memory.pop();
                }
                MachineOperation::Alloc => {
                    let val = inplace!(Value::Count);
                    let addr = self.memory.malloc(val);
                    reg = Value::Ptr(addr);
                }
                MachineOperation::Reserve(size) => {
                    self.memory.reserve(*size);
                }
                MachineOperation::Rotate(num) => {
                    // intentional direct stack manipulation
                    // This is possible without pushing but its waaay simpler with it
                    self.memory.push(reg);
                    self.memory.rotate(*num);
                    reg = self.memory.pop();
                }
                MachineOperation::Dup => {
                    self.memory.push(reg);
                }
                MachineOperation::Swap => {
                    //intentionally accessing last_mut here
                    swap(self.memory.last_mut(), &mut reg);
                }
                MachineOperation::RefFrame(o) => {
                    push!(Value::Ptr((self.bp as isize + o) as usize));
                }
                MachineOperation::Jump(newip) => {
                    ip = *newip;
                    continue;
                }
                MachineOperation::JumpRel(offset) => {
                    ip = (ip as isize + offset) as usize;
                    continue;
                }
                MachineOperation::JumpIf(newip) => {
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        ip = *newip;
                        continue;
                    }
                }
                MachineOperation::JumpRelIf(offset) => {
                    let cond = pop!(Value::Bool);
                    if cond != 0 {
                        ip = (ip as isize + offset) as usize;
                        continue;
                    }
                }
                MachineOperation::Return => {
                    self.memory.truncate_stack(self.bp);
                    (self.bp, ip) = retstack.pop().unwrap();
                    continue;
                }
                MachineOperation::Call => {
                    let newip = pop!(Value::Ptr);
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;

                    if newip < CODE {
                        let builtin_idx = newip - BUILTIN_CODE;
                        let rv = (self.builtin_table[builtin_idx].func)(self, self.bp, num_args);
                        self.memory.truncate_stack(self.bp);
                        reg = rv;
                    } else {
                        retstack.push((self.bp, ip + 1));
                        self.bp = bp;
                        ip = newip;
                        reg = self.memory.pop();
                        continue;
                    }
                }
                MachineOperation::CallBuiltin(newip) => {
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    let builtin_idx = newip - BUILTIN_CODE;
                    let rv = (self.builtin_table[builtin_idx].func)(self, bp, num_args);
                    self.memory.truncate_stack(self.bp);
                    reg = rv;
                }
                MachineOperation::CallKnown(newip) => {
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    retstack.push((self.bp, ip + 1));
                    self.bp = bp;
                    ip = *newip;
                    reg = self.memory.pop();
                    continue;
                }
                MachineOperation::CallBuiltinSize(newip, num_args) => {
                    let num_args = *num_args;
                    self.memory.push(reg);
                    let bp = self.memory.stack_top() - num_args;
                    let builtin_idx = newip - BUILTIN_CODE;
                    let rv = (self.builtin_table[builtin_idx].func)(self, bp, num_args);
                    self.memory.truncate_stack(self.bp);
                    reg = rv;
                }
                MachineOperation::CallKnownSize(newip, num_args) => {
                    self.memory.push(reg);
                    let bp = self.memory.stack_top() - num_args;
                    retstack.push((self.bp, ip + 1));
                    self.bp = bp;
                    ip = *newip;
                    reg = self.memory.pop();
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
                    let a = pop!();
                    let b = reg;
                    reg = Value::Bool(0);
                    reg = match (a, b) {
                        (Value::Count(aa), Value::Count(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Float(aa), Value::Float(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Int(aa), Value::Int(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Char(aa), Value::Char(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Bool(aa), Value::Bool(bb)) => Value::Bool(u8::from(bb == aa)),
                        // (Value::Null, Value::Null) => Value::Bool(1),
                        // (Value::Null, Value::None(_)) => Value::Bool(1),
                        // (Value::None(_), Value::Null) => Value::Bool(1),
                        (Value::None(aa), Value::None(bb)) => Value::Bool(u8::from(bb == aa)),
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
                    let a = pop!();
                    reg = match (a, reg) {
                        (Value::Count(aa), Value::Count(bb)) => Value::Bool(u8::from(bb != aa)),
                        (Value::Float(aa), Value::Float(bb)) => Value::Bool(u8::from(bb != aa)),
                        (Value::Int(aa), Value::Int(bb)) => Value::Bool(u8::from(bb != aa)),
                        (Value::Char(aa), Value::Char(bb)) => Value::Bool(u8::from(bb != aa)),
                        (Value::Bool(aa), Value::Bool(bb)) => Value::Bool(u8::from(bb != aa)),
                        // (Value::Null, Value::Null) => Value::Bool(0),
                        // (Value::Null, Value::None(_)) => Value::Bool(0),
                        // (Value::None(_), Value::Null) => Value::Bool(0),
                        (Value::None(aa), Value::None(bb)) => Value::Bool(u8::from(bb != aa)),
                        // (Value::Null, _) => Value::Bool(1),
                        // (_, Value::Null) => Value::Bool(1),
                        (Value::None(_), _) => Value::Bool(1),
                        (_, Value::None(_)) => Value::Bool(1),
                        (_, b) => {
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
                    let a = pop!();
                    match (a, &mut reg) {
                        (Value::PtrOffset(aa), Value::Ptr(ref mut bb)) => {
                            *bb = (*bb as isize + aa) as usize;
                        }
                        (Value::Int(aa), Value::Ptr(ref mut bb)) => {
                            *bb = (*bb as isize + aa as isize) as usize;
                        }
                        (Value::Ptr(aa), Value::Int(bb)) => {
                            reg = Value::Ptr((*bb + aa as i64) as usize);
                        }
                        (Value::Int(aa), Value::Int(ref mut bb)) => {
                            *bb += aa;
                        }
                        (Value::Char(aa), Value::Char(ref mut bb)) => {
                            *bb += aa;
                        }
                        (Value::Float(aa), Value::Float(ref mut bb)) => {
                            *bb += aa;
                        }
                        (Value::Str(aa), Value::Str(bb)) => {
                            let bb = *bb;
                            let val = self.memory.strcat(bb, aa);
                            reg = Value::Str(val);
                        }
                        (a, b) => {
                            unreachable!("Trying to apply binop {:?} + {:?}", a, b)
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
                MachineOperation::BoolNot => match reg {
                    Value::Bool(ref mut aa) => *aa = 1 - *aa,
                    a => unreachable!("Trying to apply unop !{:?}", a),
                },
                MachineOperation::BitNot => {
                    do_unop!(!, Int, Char);
                }
            }
            ip += 1;
        }
        self.reg = reg;
        Ok(retval)
    }
}
