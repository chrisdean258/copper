use crate::eval::{Evaluator, Value};

pub fn copper_print(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    copper_print_no_newline(ctx, args);
    print!("\n");
    Value::Null
}

pub fn copper_print_no_newline(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    for arg in args {
        match &arg {
            Value::Reference(_, _) => print!("{}", ctx.deref(arg)),
            Value::List(v) => copper_print_list(ctx, v),
            _ => print!("{}", arg),
        }
    }
    Value::Null
}

fn copper_print_list(ctx: &mut Evaluator, vals: &Vec<Value>) {
    let mut first = true;
    print!("[");
    for val in vals.iter() {
        if !first {
            print!(", ");
        }
        copper_print_no_newline(ctx, vec![val.clone()]);
        first = false;
    }
    print!("]");
}
