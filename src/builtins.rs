use crate::eval::{Evaluator, Value};
use std::io;
use std::rc::Rc;

pub fn copper_print(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    copper_print_no_newline(ctx, args);
    print!("\n");
    Value::Null
}

pub fn copper_print_no_newline(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    for arg in args {
        let val = ctx.deref(arg);
        match &val {
            Value::List(v) => copper_print_list(ctx, v),
            _ => print!("{}", val),
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

pub fn copper_getline(_: &mut Evaluator, _: Vec<Value>) -> Value {
    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(0) => Value::Null,
        Ok(_) => {
            if buffer.ends_with('\n') {
                buffer.pop();
                if buffer.ends_with('\r') {
                    buffer.pop();
                }
            }
            Value::Str(Rc::new(buffer))
        }
        Err(_) => Value::Null,
    }
}

pub fn copper_len(ctx: &mut Evaluator, args: Vec<Value>) -> Value {
    assert_eq!(args.len(), 1);
    let val = ctx.deref(args[0].clone());
    Value::Int(match &val {
        Value::List(v) => v.len() as i64,
        Value::Str(v) => v.len() as i64,
        _ => 0,
    })
}
