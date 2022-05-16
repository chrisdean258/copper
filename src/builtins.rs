use crate::eval::Evaluator;
use crate::typesystem::*;
use crate::value::Value;
use std::fs::File;
use std::io::Write;
use std::mem;
use std::os::unix::io::FromRawFd;

use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&mut Evaluator, usize, usize) -> Value,
    pub name: String,
    pub returns: Type,
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("BuiltinFunction(\"{}\"", self.name))
    }
}

impl BuiltinFunction {
    pub fn get_table() -> Vec<BuiltinFunction> {
        vec![BuiltinFunction {
            name: "write".to_string(),
            func: write,
            returns: UNIT,
        }]
    }
}

fn write(eval: &mut Evaluator, first: usize, count: usize) -> Value {
    let mut f = unsafe { File::from_raw_fd(1) };
    for arg in 0..count {
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
