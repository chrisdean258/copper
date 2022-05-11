use crate::eval::Evaluator;
use crate::typesystem::*;
use crate::value::Value;

use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&mut Evaluator, usize, usize, usize) -> usize,
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
// println!(
// "stack_len: {}, first: {:x}, first_type: {:x}, count: {}",
// eval.memory.stack.len(),
// first,
// first_type,
// count
// );

fn print_value(eval: &mut Evaluator, first: usize, first_type: usize, count: usize) -> usize {
    for arg in 0..count {
        if arg != 0 {
            print!(" ");
        }
        print!(
            "{}",
            Value::decode_from_type(eval.memory[first + arg], eval.memory[first_type + arg])
        );
    }
    println!("");
    0
}
