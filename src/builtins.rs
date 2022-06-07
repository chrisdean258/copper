use crate::eval::Evaluator;
use crate::memory;
use crate::typesystem::*;
use crate::value::Value;
use std::fs::File;
use std::io::Write;
use std::mem;
use std::os::unix::io::FromRawFd;

use std::fmt::{Debug, Formatter};

macro_rules! as_type {
    ($ex:expr, $typ:path, $fname:expr, $argnum:expr) => {
        match $ex {
            $typ(a) => a,
            t => panic!(
                "Unexecpeted argument {} to function {}. Expected {} found {:?}",
                $argnum,
                $fname,
                stringify!($typ),
                t
            ),
        }
    };
}

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&mut Evaluator, usize, usize) -> Value,
    pub name: String,
    pub signature: Signature,
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("BuiltinFunction(\"{}\"", self.name))
    }
}

macro_rules! builtin_func {
    ($name:ident, $($ty:ident),+ $(,)? => $outty:ident) => {
        BuiltinFunction {
            name: stringify!($name).to_string(),
            func: $name,
            signature: sig!($($ty),+ => $outty),
        }
    };
    ($name:ident, $($ty:ident),* $(,)? + $repty:ident => $outty:ident) => {
        BuiltinFunction {
            name: stringify!($name).to_string(),
            func: $name,
            signature: sig!($($ty),+ + $repty=> $outty),
        }
    }
}

impl BuiltinFunction {
    pub fn get_table() -> Vec<BuiltinFunction> {
        vec![
            builtin_func!(write, INT + ANY => UNIT),
            builtin_func!(alloc, INT => PTR),
            builtin_func!(len, ANY => INT),
        ]
    }
}

fn write(eval: &mut Evaluator, first: usize, count: usize) -> Value {
    let fd = match eval.memory[first] {
        Value::Int(i) => i as i32,
        t => panic!("Unexpected first arg to write. Expected Int got {:?}", t),
    };
    let mut f = unsafe { File::from_raw_fd(fd) };
    for arg in 1..count {
        let a = match eval.memory[first + arg] {
            Value::Str(p) => write!(&mut f, "{}", eval.memory.strings[p]),
            a => write!(&mut f, "{}", a),
        };
        match a {
            Ok(_) => (),
            Err(s) => panic!("{}", s),
        }
    }
    mem::forget(f);
    Value::Null
}

fn alloc(eval: &mut Evaluator, first: usize, count: usize) -> Value {
    debug_assert!(count == 1, "alloc requires exactly 1 arg");
    let size = as_type!(eval.memory[first], Value::Int, "alloc", 0);
    let rv = eval.memory.malloc(size as usize);
    Value::Ptr(rv)
}

fn len(eval: &mut Evaluator, first: usize, count: usize) -> Value {
    debug_assert!(count == 1, "len requires exactly 1 arg");
    Value::Int(match eval.memory[first] {
        Value::Ptr(p) => {
            debug_assert!(
                p >= memory::HEAP,
                "Can only take len of heap allocated pointers, not {}",
                p
            );
            1 << (p / memory::HEAP)
        }
        Value::Str(s) => eval.memory.strings[s].len() as i64,
        t => panic!("Canot type len of {:?}", t),
    })
}
