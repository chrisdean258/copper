use crate::eval::{Evaluator, Value};

pub fn copper_print(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    for arg in args {
        match &arg {
            Value::Reference(_, _) => print!("{}", ctx.deref(arg)),
            _ => print!("{}", arg),
        }
    }
    print!("\n");
    Value::Null
}
