use crate::eval::Evaluator;
use crate::typesystem::*;

use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct BuiltinFunction {
    pub func: fn(&mut Evaluator, usize, usize) -> usize,
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

fn print_value(eval: &mut Evaluator, first: usize, count: usize) -> usize {
    for arg in first..(first + count) {
        print!("{} ", eval.memory[arg]);
    }
    println!("");
    0
}
