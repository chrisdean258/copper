use crate::eval::Value;

pub fn copper_print(vars: &mut Vec<Value>, args: Vec<usize>) -> Value {
    for idx in args {
        let val = vars[idx].clone();
        match val {
            Value::Str(s) => print!("{}", s),
            Value::Int(i) => print!("{}", i),
            Value::Float(f) => print!("{}", f),
            Value::Char(c) => print!("{}", c),
            Value::Null => print!("null"),
            _ => unreachable!(),
        }
    }
    print!("\n");
    Value::Null
}
