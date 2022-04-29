#![allow(dead_code)]
use crate::parser::BinOpType;
use std::collections::HashSet;

pub struct TypeSystem {
    types: Vec<TypeEntry>,
}

enum TypeEntry {
    RefType(Type),
    ConcreteType(ConcreteType),
}

#[derive(Debug, Clone)]
struct ConcreteType {
    name: String,
}

#[derive(Debug, Clone, Copy)]
pub struct Type {
    index: usize,
}

pub const NULL: Type = Type { index: 0 };
pub const BOOL: Type = Type { index: 1 };
pub const CHAR: Type = Type { index: 2 };
pub const INT: Type = Type { index: 3 };
pub const FLOAT: Type = Type { index: 4 };
pub const STR: Type = Type { index: 5 };

impl TypeSystem {
    pub fn new() -> Self {
        let mut rv = Self { types: Vec::new() };
        rv.new_concrete(String::from("null"));
        rv.new_concrete(String::from("bool"));
        rv.new_concrete(String::from("char"));
        rv.new_concrete(String::from("int"));
        rv.new_concrete(String::from("float"));
        rv.new_concrete(String::from("str"));
        rv
    }

    pub fn new_concrete(&mut self, name: String) -> Type {
        self.types
            .push(TypeEntry::ConcreteType(ConcreteType { name: name }));
        return Type {
            index: self.types.len() - 1,
        };
    }

    pub fn typename(&self, t: Type) -> String {
        let tt = self.lookup(t);
        match &self.types[tt.index] {
            TypeEntry::ConcreteType(ct) => ct.name.clone(),
            _ => unreachable!(),
        }
    }

    pub fn lookup(&self, t: Type) -> Type {
        // TODO: Differentiate between release builds and dont do cycle checking
        let mut seen = HashSet::new();
        let mut cur = t;
        loop {
            let idx = cur.index;
            if !seen.insert(idx) {
                panic!("Type lookup loop. This is a bug")
            }
            match self.types[idx] {
                TypeEntry::RefType(rt) => cur = rt,
                TypeEntry::ConcreteType(_) => return Type { index: idx },
            }
        }
    }

    pub fn lookup_binop(&self, _op: BinOpType, _lhs: Type, _rhs: Type) -> Option<Type> {
        None
    }
}
