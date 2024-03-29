use crate::{
    builtins::BuiltinFunction,
    memory::{Memory, BUILTIN_CODE, CODE, STACK},
    operation::MachineOperation,
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

#[derive(Clone, Debug)]
pub enum ReturnState {
    Exited,
    Evaluated,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            memory: Memory::new(),
            builtin_table: BuiltinFunction::get_table(),
            bp: STACK,
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
    ) -> Result<(Value, ReturnState), String> {
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
                    $((Value::$t1(aa), Value::$t2(bb)) => Value::Bool(u8::from(bb $op aa)),)+
                    (Value::Str(aa), Value::Str(bb)) => Value::Bool(u8::from(bb $op aa || self.memory.strings[bb] $op self.memory.strings[aa])),
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

        macro_rules! print_stack {
            () => {
                if self.bp - STACK >= self.memory.stack.len() {
                    eprintln!("Stack: {:?} {:?} ", self.memory.stack, reg);
                } else {
                    eprintln!(
                        "Stack: {:?} {:?} {:?}",
                        &self.memory.stack[..(self.bp - STACK)],
                        &self.memory.stack[(self.bp - STACK)..],
                        reg
                    );
                }
            };
        }
        macro_rules! casts {
            ( $v:expr, $( $from:path => $to:path ),* $(,)?) => {
                reg = match (reg, $v) {
                    $( ($from(val), $to(_)) => $to(val), )*
                    _ => return Err(format!("Internal error. Cannot cast {reg:?} to {:?}. This is a bug in ode generation", $v))
                }
            };
        }

        self.code = code;
        while let Some(instr) = self.code.get(ip - CODE) {
            if cfg!(debug_assertions) && debug {
                print_stack!();
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
                MachineOperation::Save => {
                    retval = reg;
                }
                MachineOperation::Load => match reg {
                    Value::Ptr(addr) => {
                        reg = self.memory[addr];
                    }
                    Value::StrIdx(s, i) => {
                        let s = self.memory.strings[s as usize].as_bytes();
                        let Some(ch) = s.get(i as usize) else {
                            return Err(
                                format!("Indexing into string with index {i} but string len is {}", s.len())
                            );
                        };
                        reg = Value::Char(*ch);
                    }
                    a => panic!("Trying to deref `{a:?}`"),
                },
                MachineOperation::LoadAddr(addr) => {
                    push!(self.memory[*addr]);
                }
                MachineOperation::LoadLocal(o) => {
                    let ptr = (self.bp as isize + o) as usize;
                    self.memory.push(reg);
                    reg = self.memory[ptr];
                }
                MachineOperation::Store => {
                    // intentional direct stack manipulation for optiization
                    let addr = as_type!(self.memory.pop(), Value::Ptr);
                    self.memory[addr] = reg;
                }
                MachineOperation::FastStore => {
                    let addr = as_type!(self.memory.pop(), Value::Ptr);
                    self.memory[addr] = reg;
                    reg = self.memory.pop();
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
                    self.memory.push(reg);
                    self.memory.reserve(*size);
                    reg = self.memory.pop();
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
                MachineOperation::Cast(v) => {
                    casts!(v,
                        Value::Ptr => Value::List,
                        Value::List => Value::Ptr,
                    )
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
                MachineOperation::ExitWith => {
                    self.reg = reg;
                    return Ok((reg, ReturnState::Exited));
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

                    // This may be a bug with a path thats never hit yet
                    // Should be using bp in the following not self.bp
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
                    self.memory.truncate_stack(bp);
                    reg = rv;
                }
                MachineOperation::CallKnownSize(newip, num_args) => {
                    let bp = self.memory.stack_top() - num_args + 1;
                    retstack.push((self.bp, ip + 1));
                    self.bp = bp;
                    ip = *newip;
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
                        (Value::Ptr(aa), Value::Ptr(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Count(aa), Value::Count(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Float(aa), Value::Float(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Int(aa), Value::Int(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Char(aa), Value::Char(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::Bool(aa), Value::Bool(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::None(aa), Value::None(bb)) => Value::Bool(u8::from(bb == aa)),
                        (Value::None(_), _) => Value::Bool(0),
                        (_, Value::None(_)) => Value::Bool(0),
                        (Value::Str(aa), Value::Str(bb)) => Value::Bool(u8::from(
                            bb == aa || self.memory.strings[bb] == self.memory.strings[aa],
                        )),
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
                        (Value::None(aa), Value::None(bb)) => Value::Bool(u8::from(bb != aa)),
                        (Value::None(_), _) => Value::Bool(1),
                        (_, Value::None(_)) => Value::Bool(1),
                        (Value::Str(aa), Value::Str(bb)) => Value::Bool(u8::from(
                            // Check identity before doing full string comparison
                            bb != aa && self.memory.strings[bb] != self.memory.strings[aa],
                        )),
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
                        (Value::PtrOffset(aa), Value::Str(bb)) => {
                            reg = Value::StrIdx(*bb as u32, aa as u32)
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
                        (Value::Char(aa), Value::Int(ref mut bb)) => {
                            *bb += aa as i64;
                        }
                        (Value::Int(aa), Value::Char(bb)) => {
                            reg = Value::Int(aa + *bb as i64);
                        }
                        (Value::Char(aa), Value::Char(ref mut bb)) => {
                            *bb += aa;
                        }
                        (Value::Float(aa), Value::Float(ref mut bb)) => {
                            *bb += aa;
                        }
                        (Value::PtrOffset(aa), Value::List(p)) => {
                            reg = Value::Ptr((*p as isize + aa) as usize);
                        }
                        (Value::Int(aa), Value::List(p)) => {
                            let len = as_type!(self.memory[*p - 1], Value::Int);
                            let mut idx = aa;
                            if idx < 0 {
                                idx += len;
                            }
                            if idx < 0 || idx >= len {
                                return Err(format!("Trying to index list at index {aa} when list length is only length {len}"));
                            }
                            reg = Value::Ptr((*p as i64 + aa) as usize);
                        }
                        (Value::List(aa), Value::List(bb)) => {
                            let aelems = aa;
                            let belems = *bb;
                            let alen = as_type!(self.memory[aa - 1], Value::Int) as usize;
                            let blen = as_type!(self.memory[*bb - 1], Value::Int) as usize;
                            let new_len = alen + blen;
                            let new_ptr = self.memory.malloc(new_len + 1);
                            self.memory[new_ptr] = Value::Int(new_len as i64);
                            self.memory.memcpy(new_ptr + 1, belems, blen);
                            self.memory.memcpy(new_ptr + blen + 1, aelems, alen);
                            *bb = new_ptr + 1;
                        }
                        (Value::Int(aa), Value::Str(bb)) => {
                            let len = self.memory.strings[*bb].len() as i64;
                            let mut idx = aa;
                            if idx < 0 {
                                idx += len;
                            }
                            if idx < 0 || idx >= len {
                                return Err(format!("Trying to index list at index {aa} when list length is only length {len}"));
                            }

                            reg = Value::StrIdx(*bb as u32, idx as u32)
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
                MachineOperation::Negate => {
                    do_unop!(-, Int, Float);
                }
            }
            ip += 1;
        }
        self.reg = reg;
        Ok((retval, ReturnState::Evaluated))
    }
}
