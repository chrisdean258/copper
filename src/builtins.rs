use crate::eval::{Evaluator, Object, Value};
use std::io;
use std::rc::Rc;

pub fn copper_print(ctx: &mut Evaluator, args: Vec<Object>) -> Object {
    copper_print_no_newline(ctx, args);
    print!("\n");
    ctx.null()
}

pub fn copper_print_no_newline(ctx: &mut Evaluator, args: Vec<Object>) -> Object {
    for arg in args {
        let val = ctx.deref(arg);
        match &val.value {
            Value::List(v) => copper_print_list(ctx, v),
            _ => print!("{}", val.value),
        }
    }
    ctx.null()
}

fn copper_print_list(ctx: &mut Evaluator, vals: &Vec<Object>) {
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

pub fn copper_getline(ctx: &mut Evaluator, _: Vec<Object>) -> Object {
    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(0) => ctx.undefined(),
        Ok(_) => {
            if buffer.ends_with('\n') {
                buffer.pop();
                if buffer.ends_with('\r') {
                    buffer.pop();
                }
            }
            ctx.string(Rc::new(buffer))
        }
        Err(_) => ctx.undefined(),
    }
}

pub fn copper_len(ctx: &mut Evaluator, args: Vec<Object>) -> Object {
    assert_eq!(args.len(), 1);
    let val = ctx.deref(args[0].clone());
    ctx.int(match &val.value {
        Value::List(v) => v.len() as i64,
        Value::Str(v) => v.len() as i64,
        _ => 0,
    })
}
