use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Copy)]
pub enum Value {
    Uninitialized,
    None(usize),
    Bool(u8),
    Char(u8),
    Int(i64),
    Float(f64),
    Ptr(usize),
    PtrOffset(isize),
    Count(usize),
    Str(usize),
    StrIdx(u32, u32),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Uninitialized => write!(f, "uninit"),
            Value::None(_) => write!(f, "null"),
            Value::Bool(b) => write!(f, "{}", if *b == 0 { "false" } else { "true" }),
            Value::Char(c) => write!(f, "{}", *c as char),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Ptr(p) => write!(f, "0x{:x}", p),
            Value::Str(s) => write!(f, "0x{:x}", s),
            Value::StrIdx(s, i) => write!(f, "0x{:x}[{i}]", s),
            Value::Count(c) => write!(f, "+{}", c),
            Value::PtrOffset(o) => {
                if *o >= 0 {
                    write!(f, "+0x{:x}", o)
                } else {
                    write!(f, "-0x{:x}", -o)
                }
            }
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Uninitialized => write!(f, "Uninit"),
            Value::None(t) => write!(f, "None({t})"),
            Value::Bool(b) => write!(f, "Bool({self} = {b})"),
            Value::Char(c) => write!(f, "Char({:?})", *c as char),
            Value::Int(i) => write!(f, "Int({i})"),
            Value::Float(fl) => write!(f, "Float({fl})"),
            Value::Ptr(p) => write!(f, "Ptr(0x{p:x})"),
            Value::Str(s) => write!(f, "Str(0x{s:x})"),
            Value::StrIdx(s, i) => write!(f, "StrIdx(0x{s:x},{i})"),
            Value::Count(c) => write!(f, "Count(+{c})"),
            Value::PtrOffset(_) => write!(f, "PtrOffset({})", self),
        }
    }
}
