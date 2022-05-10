use crate::eval::Evaluator;
use crate::typesystem::*;
use crate::value::Value;

use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&mut Evaluator, usize),
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
            name: "print".to_string(),
            func: print_value,
            returns: UNIT,
        }]
    }
}

fn print_value(eval: &mut Evaluator, num_args: usize) {
    for i in 0..num_args {
        print!(
            "{} ",
            eval.memory.stack[eval.memory.stack.len() - num_args + i]
        );
    }
    println!("");
    eval.memory.push(Value::Null)
}
