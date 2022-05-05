use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Null,
    Bool(u8),
    Char(u8),
    Int(i64),
    Float(f64),
    Ptr(usize),
    PtrOffset(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Null => f.write_str("null"),
            Value::Bool(b) => f.write_str(if *b == 0 { "false" } else { "true" }),
            Value::Char(c) => f.write_fmt(format_args!("'{}'", *c as char)),
            Value::Int(i) => f.write_fmt(format_args!("{}", i)),
            Value::Float(fl) => f.write_fmt(format_args!("{}", fl)),
            Value::Ptr(p) => f.write_fmt(format_args!("0x{:x}", p)),
            Value::PtrOffset(o) => f.write_fmt(format_args!("+0x{:x}", o)),
        }
    }
}
