use crate::eval::{Evaluator, Value};

pub fn copper_print(ctx: &mut Evaluator, args: Vec<usize>) -> Value {
    for idx in args {
        let mut val = idx;
        let printable = loop {
            match &ctx.values[val] {
                Value::Str(s) => break format!("{}", s),
                Value::Int(i) => break format!("{}", i),
                Value::Float(f) => break format!("{}", f),
                Value::Char(c) => break format!("{}", c),
                Value::Null => break format!("null"),
                Value::Reference(u) => {
                    val = *u;
                    if val == idx {
                        panic!("refernce loop in print");
                    }
                }
                _ => unreachable!(),
            }
        };
        print!("{}", printable);
    }
    print!("\n");
    Value::Null
}
