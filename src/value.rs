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
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Uninitialized => f.write_str("uninit"),
            Value::Null => f.write_str("null"),
            Value::Bool(b) => f.write_str(if *b == 0 { "false" } else { "true" }),
            Value::Char(c) => f.write_fmt(format_args!("'{}'", *c as char)),
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
        }
    }
}

impl Value {
    pub fn encode_for_push(&self) -> u64 {
        match self {
            Value::Uninitialized => 0,
            Value::Null => 0,
            Value::Bool(b) => *b as u64,
            Value::Char(c) => *c as u64,
            Value::Int(i) => *i as u64 & !(1 << 63),
            Value::Float(f) => f.to_bits() >> 1,
            Value::Ptr(p) => {
                assert!(p & 1 << 63 == 0);
                *p as u64
            }
            Value::PtrOffset(o) => *o as u64 & !(1 << 63),
            Value::Count(u) => {
                assert!(u & 1 << 63 == 0);
                *u as u64
            }
        }
    }

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
        }
    }

    pub fn decode_full(bytes: u64, mut what: Value) -> Value {
        match &mut what {
            Value::Uninitialized => assert_eq!(bytes, 0),
            Value::Null => assert_eq!(bytes, 0),
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
            Value::PtrOffset(o) => {
                *o = bytes as isize;
            }
            Value::Count(u) => *u = bytes as usize,
        }
        what
    }

    pub fn decode_push(bytes: u64, mut what: Value) -> Value {
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
                *i = ((bytes as i64) << 1) >> 1;
            }
            Value::Float(f) => {
                *f = f64::from_bits(bytes << 1);
            }
            Value::Ptr(p) => *p = bytes as usize,
            Value::PtrOffset(o) => {
                *o = ((bytes as isize) << 1) >> 1;
            }
            Value::Count(u) => *u = bytes as usize,
        }
        what
    }
}
