use crate::eval::{Evaluator, Value};

pub fn copper_print(ctx: &mut Evaluator, args: Vec<usize>) -> Value {
    for idx in args {
        print!("{}", ctx.values[idx]);
    }
    print!("\n");
    Value::Null
}
