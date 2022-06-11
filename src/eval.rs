#![allow(dead_code)]
use crate::{
    builtins::BuiltinFunction,
    memory::{Memory, BUILTIN_CODE, CODE, STACK},
    operation::MachineOperation,
    typesystem::TypeSystem,
    value::Value,
};
use std::{collections::HashMap, mem::swap};

#[derive(Clone, Debug)]
pub struct Evaluator {
    code: Vec<MachineOperation>,
    pub memory: Memory,
    builtin_table: Vec<BuiltinFunction>,
    ip: usize,
    bp: usize,
}

impl Evaluator {
    pub fn new(types: &mut TypeSystem) -> Self {
        Self {
            code: Vec::new(),
            memory: Memory::new(),
            builtin_table: BuiltinFunction::get_table(types),
            ip: CODE,
            bp: STACK + 1,
        }
    }

    #[allow(clippy::assign_op_pattern)]
    #[allow(unused_assignments)]
    pub fn eval(
        &mut self,
        code: Vec<MachineOperation>,
        mut strings: Vec<String>,
        entry: usize,
        debug: bool,
        funcs: &HashMap<usize, String>,
    ) -> Result<Value, String> {
        self.memory.add_strings(&mut strings);
        let mut reg = Value::Uninitialized;

        macro_rules! as_type {
            ($ex:expr, $typ:path) => {
                match $ex {
                    $typ(a) => a,
                    t => unreachable!("Trying to interpret {:?} as {}", t, stringify!($typ)),
                }
            };
        }

        macro_rules! inplace {
            () => {
                reg
            };
            ($typ:path) => {
                as_type!(inplace!(), $typ)
            };
        }

        macro_rules! pop {
            ($typ:path) => {
                as_type!(pop!(), $typ)
            };
            () => {{
                let rv = inplace!();
                inplace!() = self.memory.pop();
                rv
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
                match (a, &mut inplace!()) {
                    $( (Value::$t1(aa), Value::$t2(ref mut bb)) => {*bb = *bb $op aa;})+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }
        macro_rules! do_comparison {
            ($op:tt, $($t1:ident, $t2:ident),+ $(,)?) => {
                let a = pop!();
                inplace!() = match (a, inplace!()) {
                    $((Value::$t1(aa), Value::$t2(bb)) => Value::Bool(if bb $op aa {1} else {0}),)+
                    (_, b) => unreachable!("Trying to apply binop {:?} {} {:?}", a, stringify!($op), b),
                };
            };
        }

        macro_rules! do_unop {
            ($op:tt, $($t:ident),+ $(,)?) => {
                match &mut inplace!() {
                    $(Value::$t(ref mut aa) => {*aa = $op *aa;})+
                    a => unreachable!("Trying to apply binop {} {:?}", stringify!($op), a),
                }
            };
        }
        self.ip = entry;
        if cfg!(debug_assertions) && debug {
            for (i, instr) in code.iter().enumerate() {
                if let Some(name) = funcs.get(&i) {
                    eprintln!("{}:", name)
                }
                eprint!("\t0x{:08x}: {}", i + CODE, instr);
                match instr {
                    MachineOperation::CallKnown(addr) => {
                        eprint!(" ({})", funcs.get(&(addr - CODE)).unwrap())
                    }
                    MachineOperation::CallBuiltin(_addr) => {}
                    _ => {}
                }
                eprintln!();
            }
        }
        self.code = code;
        while self.ip < self.code.len() + CODE {
            if cfg!(debug_assertions) && debug {
                eprintln!("Stack: {:?} {:?}", self.memory.stack, reg);
                eprint!("IP: 0x{:08x}:  ", self.ip);
                eprint!("{:23}  ", self.code[self.ip - CODE].to_string());
                eprint!("BP: 0x{:08x}     ", self.bp);
            }
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
                MachineOperation::Push(v) => {
                    push!(v);
                }
                MachineOperation::Inplace(v) => inplace!() = v,
                MachineOperation::Pop => {
                    pop!();
                }
                MachineOperation::Load => {
                    let addr = inplace!(Value::Ptr);
                    inplace!() = self.memory[addr];
                }
                MachineOperation::LoadLocal(o) => {
                    let ptr = (self.bp as isize + o) as usize;
                    push!(self.memory[ptr]);
                }
                MachineOperation::Store => {
                    let value = pop!();
                    let addr = inplace!(Value::Ptr);
                    self.memory[addr] = value;
                    inplace!() = value;
                }
                MachineOperation::FastStore => {
                    let value = pop!();
                    let addr = pop!(Value::Ptr);
                    self.memory[addr] = value;
                }
                MachineOperation::StoreN(num) => {
                    //intentional direct stack manipulation
                    self.memory.push(inplace!());
                    let dst = as_type!(self.memory[self.memory.stack_top() - num - 1], Value::Ptr);
                    let src = self.memory.stack_top() - num;
                    self.memory.memcpy(dst, src, num);
                    self.memory
                        .truncate_stack(self.memory.stack_top() - num - 1);
                    inplace!() = self.memory.pop();
                }
                MachineOperation::LoadN(num) => {
                    //intentional direct stack manipulation
                    let src = inplace!(Value::Ptr);
                    let dst = self.memory.stack_top();
                    self.memory.reserve(num);
                    self.memory.memcpy(dst, src, num);
                    reg = self.memory.pop();
                }
                MachineOperation::Alloc => {
                    let val = inplace!(Value::Count);
                    let addr = self.memory.malloc(val);
                    inplace!() = Value::Ptr(addr);
                }
                MachineOperation::Reserve(size) => {
                    self.memory.reserve(size);
                }
                MachineOperation::Rotate(num) => {
                    // intentional direct stack manipulation
                    // This is possible without pushing but its waaay simpler with it
                    self.memory.push(reg);
                    self.memory.rotate(num);
                    reg = self.memory.pop();
                }
                MachineOperation::Dup => {
                    push!(inplace!());
                }
                MachineOperation::Swap => {
                    //intentionally accessing last_mut here
                    swap(self.memory.last_mut(), &mut inplace!());
                }
                MachineOperation::RefFrame(o) => {
                    push!(Value::Ptr((self.bp as isize + o) as usize));
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
                    let rv = inplace!();
                    self.memory.truncate_stack(self.bp);
                    self.bp = as_type!(self.memory.pop(), Value::Ptr);
                    self.ip = as_type!(self.memory.pop(), Value::Ptr);
                    inplace!() = rv;
                    continue;
                }
                MachineOperation::Call => {
                    let ip = pop!(Value::Ptr);
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;

                    if ip < CODE {
                        let builtin_idx = ip - BUILTIN_CODE;
                        let rv = (self.builtin_table[builtin_idx].func)(self, self.bp, num_args);
                        self.memory.truncate_stack(self.bp);
                        inplace!() = rv;
                    } else {
                        // these must be written into memory
                        self.memory[bp - 1] = Value::Ptr(self.bp);
                        self.memory[bp - 2] = Value::Ptr(self.ip + 1);
                        self.bp = bp;
                        self.ip = ip;
                        inplace!() = self.memory.pop();
                        continue;
                    }
                }
                MachineOperation::CallBuiltin(ip) => {
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    let builtin_idx = ip - BUILTIN_CODE;
                    let rv = (self.builtin_table[builtin_idx].func)(self, bp, num_args);
                    self.memory.truncate_stack(self.bp);
                    inplace!() = rv;
                }
                MachineOperation::CallKnown(ip) => {
                    let num_args = inplace!(Value::Count);
                    let bp = self.memory.stack_top() - num_args;
                    self.memory[bp - 1] = Value::Ptr(self.bp);
                    self.memory[bp - 2] = Value::Ptr(self.ip + 1);
                    self.bp = bp;
                    self.ip = ip;
                    inplace!() = self.memory.pop();
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
                    let b = inplace!();
                    inplace!() = Value::Bool(0);
                    inplace!() = match (a, b) {
                        (Value::Count(aa), Value::Count(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
                        }
                        (Value::Float(aa), Value::Float(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
                        }
                        (Value::Int(aa), Value::Int(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
                        }
                        (Value::Char(aa), Value::Char(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
                        }
                        (Value::Bool(aa), Value::Bool(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
                        }
                        // (Value::Null, Value::Null) => Value::Bool(1),
                        // (Value::Null, Value::None(_)) => Value::Bool(1),
                        // (Value::None(_), Value::Null) => Value::Bool(1),
                        (Value::None(aa), Value::None(bb)) => {
                            Value::Bool(if bb == aa { 1 } else { 0 })
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
                    let a = pop!();
                    inplace!() = match (a, inplace!()) {
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
                    // let b = *self.memory.last();
                    match (a, &mut inplace!()) {
                        (Value::PtrOffset(aa), Value::Ptr(ref mut bb)) => {
                            *bb = (*bb as isize + aa) as usize;
                        }
                        (Value::Int(aa), Value::Ptr(ref mut bb)) => {
                            *bb = (*bb as isize + aa as isize) as usize;
                        }
                        (Value::Ptr(aa), Value::Int(bb)) => {
                            inplace!() = Value::Ptr((*bb + aa as i64) as usize);
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
                            inplace!() = Value::Str(val);
                        }
                        (a, b) => {
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
                MachineOperation::BoolNot => match inplace!() {
                    Value::Bool(ref mut aa) => *aa = 1 - *aa,
                    a => unreachable!("Trying to apply binop !{:?}", a),
                },
                MachineOperation::BitNot => {
                    do_unop!(!, Int, Char);
                }
            }
            self.ip += 1;
        }
        Ok(Value::Uninitialized)
    }
}
