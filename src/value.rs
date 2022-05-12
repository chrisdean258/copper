use crate::typesystem::*;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone, Copy)]
pub enum Value {
    Uninitialized,
    Null,
    Bool(u8),
    Char(u8),
    Int(i64),
    Float(f64),
    Ptr(usize),
    PtrOffset(isize),
    Count(usize),
    Type(usize),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Uninitialized => f.write_str("uninit"),
            Value::Null => f.write_str("null"),
            Value::Bool(b) => f.write_str(if *b == 0 { "false" } else { "true" }),
            Value::Char(c) => f.write_fmt(format_args!("{}", *c as char)),
            Value::Int(i) => f.write_fmt(format_args!("{}", i)),
            Value::Float(fl) => f.write_fmt(format_args!("{}", fl)),
            Value::Ptr(p) => f.write_fmt(format_args!("0x{:x}", p)),
            Value::Count(c) => f.write_fmt(format_args!("+{}", c)),
            Value::PtrOffset(o) => {
                if *o >= 0 {
                    f.write_fmt(format_args!("+0x{:x}", o))
                } else {
                    f.write_fmt(format_args!("-0x{:x}", -o))
                }
            }
            Value::Type(u) => f.write_fmt(format_args!("{}", u)),
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Uninitialized => f.write_str("Uninit"),
            Value::Null => f.write_str("Null"),
            Value::Bool(b) => f.write_fmt(format_args!(
                "Bool({} = {})",
                if *b == 0 { "false" } else { "true" },
                *b
            )),
            Value::Char(c) => f.write_fmt(format_args!("Char('{}')", *c as char)),
            Value::Int(i) => f.write_fmt(format_args!("Int({})", i)),
            Value::Float(fl) => f.write_fmt(format_args!("Float({})", fl)),
            Value::Ptr(p) => f.write_fmt(format_args!("Ptr(0x{:x})", p)),
            Value::Count(c) => f.write_fmt(format_args!("Count(+{})", c)),
            Value::PtrOffset(o) => {
                if *o >= 0 {
                    f.write_fmt(format_args!("PtrOffset(+0x{:x})", o))
                } else {
                    f.write_fmt(format_args!("PtrOffset(-0x{:x})", -o))
                }
            }
            Value::Type(u) => f.write_fmt(format_args!("{:?}", u)),
        }
    }
}

impl Value {
    pub fn encode_full(&self) -> u64 {
        match self {
            Value::Uninitialized => 0,
            Value::Null => 0,
            Value::Bool(b) => *b as u64,
            Value::Char(c) => *c as u64,
            Value::Int(i) => *i as u64,
            Value::Float(f) => f.to_bits(),
            Value::Ptr(p) => *p as u64,
            Value::PtrOffset(o) => *o as u64,
            Value::Count(u) => *u as u64,
            Value::Type(u) => *u as u64,
        }
    }

    pub fn decode_from_type(bytes: u64, type_enc: u64) -> Value {
        match type_enc as usize {
            UNIT => panic!("Unit type in live system"),
            UNKNOWN_RETURN => panic!("Unknown return in live system"),
            BUILTIN_FUNCTION => Value::decode_bytes(bytes, Value::Ptr(0)),
            NULL => Value::decode_bytes(bytes, Value::Null),
            PTR => Value::decode_bytes(bytes, Value::Ptr(bytes as usize)),
            BOOL => Value::decode_bytes(bytes, Value::Bool(0)),
            CHAR => Value::decode_bytes(bytes, Value::Char(0)),
            INT => Value::decode_bytes(bytes, Value::Int(0)),
            FLOAT => Value::decode_bytes(bytes, Value::Float(0.0)),
            STR => todo!(),
            _ => panic!("No!"),
        }
    }

    pub fn decode_bytes(bytes: u64, mut what: Value) -> Value {
        match &mut what {
            Value::Uninitialized => (),
            Value::Null => (),
            Value::Bool(b) => {
                *b = (bytes & 0x1) as u8;
            }
            Value::Char(c) => {
                *c = (bytes & 0xff) as u8;
            }
            Value::Int(i) => {
                *i = bytes as i64;
            }
            Value::Float(f) => {
                *f = f64::from_bits(bytes);
            }
            Value::Ptr(p) => *p = bytes as usize,
            Value::PtrOffset(o) => *o = bytes as isize,
            Value::Count(u) => *u = bytes as usize,
            Value::Type(u) => *u = bytes as usize,
        }
        what
    }
}
