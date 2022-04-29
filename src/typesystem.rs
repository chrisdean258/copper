#![allow(dead_code)]
use crate::parser::AssignType;
use crate::parser::BinOpType;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone)]
pub struct TypeSystem {
    types: Vec<TypeEntry>,
    types_by_name: HashMap<String, Type>,
    operations: Vec<Operation>,
    ops_by_name: HashMap<String, Op>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeEntryType {
    RefType(Type),
    ConcreteType,
}

#[derive(Debug, Clone)]
struct TypeEntry {
    name: String,
    te_type: TypeEntryType,
}

#[derive(Debug, Clone)]
struct Operation {
    name: String,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct Signature {
    inputs: Vec<Type>,
    output: Type,
}

#[derive(Clone, Copy, PartialEq)]
pub struct Type {
    index: usize,
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.index))
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Op {
    index: usize,
}

impl Debug for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.index))
    }
}

pub const UNKNOWN: Type = Type { index: 0 };
pub const NULL: Type = Type { index: 1 };
pub const BOOL: Type = Type { index: 2 };
pub const CHAR: Type = Type { index: 3 };
pub const INT: Type = Type { index: 4 };
pub const FLOAT: Type = Type { index: 5 };
pub const STR: Type = Type { index: 6 };

pub const BOOLOR: Op = Op { index: 0 };
pub const BOOLXOR: Op = Op { index: 1 };
pub const BOOLAND: Op = Op { index: 2 };
pub const BITOR: Op = Op { index: 3 };
pub const BITXOR: Op = Op { index: 4 };
pub const BITAND: Op = Op { index: 5 };
pub const CMPGE: Op = Op { index: 6 };
pub const CMPGT: Op = Op { index: 7 };
pub const CMPLE: Op = Op { index: 8 };
pub const CMPLT: Op = Op { index: 9 };
pub const CMPEQ: Op = Op { index: 10 };
pub const CMPNOTEQ: Op = Op { index: 11 };
pub const BITSHIFTLEFT: Op = Op { index: 12 };
pub const BITSHIFTRIGHT: Op = Op { index: 13 };
pub const MINUS: Op = Op { index: 14 };
pub const PLUS: Op = Op { index: 15 };
pub const TIMES: Op = Op { index: 16 };
pub const MOD: Op = Op { index: 17 };
pub const DIV: Op = Op { index: 18 };

pub const EQUAL: Op = Op { index: 19 };
pub const ANDEQ: Op = Op { index: 20 };
pub const XOREQ: Op = Op { index: 21 };
pub const OREQ: Op = Op { index: 22 };
pub const PLUSEQ: Op = Op { index: 23 };
pub const MINUSEQ: Op = Op { index: 24 };
pub const TIMESEQ: Op = Op { index: 25 };
pub const DIVEQ: Op = Op { index: 26 };
pub const MODEQ: Op = Op { index: 27 };
pub const BITSHIFTRIGHTEQ: Op = Op { index: 28 };
pub const BITSHIFTLEFTEQ: Op = Op { index: 29 };

impl TypeSystem {
    pub fn new() -> Self {
        let mut rv = Self {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            operations: Vec::new(),
            ops_by_name: HashMap::new(),
        };
        rv.add_default_types();
        rv.add_default_ops();
        rv.sanity_check();
        rv
    }

    fn add_default_types(&mut self) {
        self.new_type(String::from("unknown"), TypeEntryType::ConcreteType);
        self.new_type(String::from("null"), TypeEntryType::ConcreteType);
        self.new_type(String::from("bool"), TypeEntryType::ConcreteType);
        self.new_type(String::from("char"), TypeEntryType::ConcreteType);
        self.new_type(String::from("int"), TypeEntryType::ConcreteType);
        self.new_type(String::from("float"), TypeEntryType::ConcreteType);
        self.new_type(String::from("str"), TypeEntryType::ConcreteType);
    }

    fn add_default_ops(&mut self) {
        macro_rules! make_op {
            ($($operation:literal),+ | $($( $input: ident),+ => $returntype:ident),* $(,)? ) => {
                let ops = vec![$(self.find_or_make_op(String::from($operation))),+];
                for op in ops {
                    $(self.add_signature(op, Signature {
                        inputs: vec![$( $input ),+],
                        output: $returntype,
                    });)*
                }
            };
        }
        make_op!("||", "^^", "&&" |
            BOOL, BOOL => BOOL,
        );
        make_op!("|", "^", "&" |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );
        make_op!(">=", ">", "<=", "<", "==", "!=" |
            INT, INT => BOOL,
            CHAR, CHAR => BOOL,
            FLOAT, FLOAT => BOOL,
            STR, STR => BOOL,
        );

        make_op!("<<", ">>" |
            CHAR, CHAR => CHAR,
            CHAR, INT => CHAR,
            INT, INT => INT,
        );

        make_op!("-", "+", "*", "%", "/"  |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!("-", "+", "*", "/"  |
            FLOAT, FLOAT => FLOAT
        );

        make_op!("+" |
             STR, STR => STR
        );

        make_op!("=" |
           NULL, NULL => NULL,
           BOOL, BOOL => BOOL,
           CHAR, CHAR => CHAR,
           INT, INT => INT,
           FLOAT, FLOAT => FLOAT,
           STR, STR => STR,
        );

        make_op!("&=", "^=", "|=" |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!("+=", "-=", "*=", "/=", "%=", ">>=", "<<=" |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!("-=", "+=", "*=", "/="  |
            FLOAT, FLOAT => FLOAT
        );
    }

    fn sanity_check(&self) {
        // This makes sure that we havent messed with anything
        assert_eq!(self.types[UNKNOWN.index].name, "unknown");
        assert_eq!(self.types[NULL.index].name, "null");
        assert_eq!(self.types[BOOL.index].name, "bool");
        assert_eq!(self.types[CHAR.index].name, "char");
        assert_eq!(self.types[INT.index].name, "int");
        assert_eq!(self.types[FLOAT.index].name, "float");
        assert_eq!(self.types[STR.index].name, "str");
        assert_eq!(self.operations[BOOLOR.index].name, "||");
        assert_eq!(self.operations[BOOLXOR.index].name, "^^");
        assert_eq!(self.operations[BOOLAND.index].name, "&&");
        assert_eq!(self.operations[BITOR.index].name, "|");
        assert_eq!(self.operations[BITXOR.index].name, "^");
        assert_eq!(self.operations[BITAND.index].name, "&");
        assert_eq!(self.operations[CMPGE.index].name, ">=");
        assert_eq!(self.operations[CMPGT.index].name, ">");
        assert_eq!(self.operations[CMPLE.index].name, "<=");
        assert_eq!(self.operations[CMPLT.index].name, "<");
        assert_eq!(self.operations[CMPEQ.index].name, "==");
        assert_eq!(self.operations[CMPNOTEQ.index].name, "!=");
        assert_eq!(self.operations[BITSHIFTLEFT.index].name, "<<");
        assert_eq!(self.operations[BITSHIFTRIGHT.index].name, ">>");
        assert_eq!(self.operations[MINUS.index].name, "-");
        assert_eq!(self.operations[PLUS.index].name, "+");
        assert_eq!(self.operations[TIMES.index].name, "*");
        assert_eq!(self.operations[MOD.index].name, "%");
        assert_eq!(self.operations[DIV.index].name, "/");
        assert_eq!(self.operations[EQUAL.index].name, "=");
        assert_eq!(self.operations[ANDEQ.index].name, "&=");
        assert_eq!(self.operations[XOREQ.index].name, "^=");
        assert_eq!(self.operations[OREQ.index].name, "|=");
        assert_eq!(self.operations[PLUSEQ.index].name, "+=");
        assert_eq!(self.operations[MINUSEQ.index].name, "-=");
        assert_eq!(self.operations[TIMESEQ.index].name, "*=");
        assert_eq!(self.operations[DIVEQ.index].name, "/=");
        assert_eq!(self.operations[MODEQ.index].name, "%=");
        assert_eq!(self.operations[BITSHIFTRIGHTEQ.index].name, ">>=");
        assert_eq!(self.operations[BITSHIFTLEFTEQ.index].name, "<<=");
    }

    pub fn find_or_make_type(&mut self, name: String, te_type: TypeEntryType) -> Type {
        match self.types_by_name.get(&name) {
            Some(a) => {
                let typ = self.lookup_type(*a);
                assert_eq!(self.types[typ.index].te_type, te_type);
                typ
            }
            None => self.new_type(name, te_type),
        }
    }

    pub fn find_or_make_op(&mut self, name: String) -> Op {
        match self.ops_by_name.get(&name) {
            None => self.new_op(name),
            Some(a) => *a,
        }
    }

    fn new_type(&mut self, name: String, te_type: TypeEntryType) -> Type {
        self.types.push(TypeEntry {
            name: name.clone(),
            te_type,
        });
        let rv = Type {
            index: self.types.len() - 1,
        };
        self.types_by_name.insert(name, rv);
        rv
    }

    fn new_op(&mut self, name: String) -> Op {
        self.operations.push(Operation {
            name: name.clone(),
            signatures: Vec::new(),
        });
        let rv = Op {
            index: self.operations.len() - 1,
        };
        assert!(self.ops_by_name.insert(name, rv).is_none());
        rv
    }

    pub fn add_signature(&mut self, op: Op, sig: Signature) {
        self.operations[op.index].signatures.push(sig);
    }

    pub fn typename(&self, t: Type) -> String {
        self.types[self.lookup_type(t).index].name.clone()
    }

    pub fn lookup_type(&self, t: Type) -> Type {
        // TODO: Differentiate between release builds and dont do cycle checking
        let mut seen = HashSet::new();
        let mut cur = t;
        loop {
            let idx = cur.index;
            if !seen.insert(idx) {
                panic!("Type lookup loop. This is a bug")
            }
            match self.types[idx].te_type {
                TypeEntryType::RefType(rt) => cur = rt,
                TypeEntryType::ConcreteType => return Type { index: idx },
            }
        }
    }

    pub fn lookup_binop(&self, binop: BinOpType, lhs: Type, rhs: Type) -> Option<Type> {
        let op = match binop {
            BinOpType::BoolOr => BOOLOR,
            BinOpType::BoolXor => BOOLXOR,
            BinOpType::BoolAnd => BOOLAND,
            BinOpType::BitOr => BITOR,
            BinOpType::BitXor => BITXOR,
            BinOpType::BitAnd => BITAND,
            BinOpType::CmpGE => CMPGE,
            BinOpType::CmpGT => CMPGT,
            BinOpType::CmpLE => CMPLE,
            BinOpType::CmpLT => CMPLT,
            BinOpType::CmpEq => CMPEQ,
            BinOpType::CmpNotEq => CMPNOTEQ,
            BinOpType::BitShiftLeft => BITSHIFTLEFT,
            BinOpType::BitShiftRight => BITSHIFTRIGHT,
            BinOpType::Minus => MINUS,
            BinOpType::Plus => PLUS,
            BinOpType::Times => TIMES,
            BinOpType::Mod => MOD,
            BinOpType::Div => DIV,
        };
        for sig in self.operations[op.index].signatures.iter() {
            if sig.inputs.len() == 2 && sig.inputs[0] == lhs && sig.inputs[1] == rhs {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_assign(&self, aop: AssignType, lhs: Type, rhs: Type) -> Option<Type> {
        let op = match aop {
            AssignType::Equal => EQUAL,
            AssignType::AndEq => ANDEQ,
            AssignType::XorEq => XOREQ,
            AssignType::OrEq => OREQ,
            AssignType::PlusEq => PLUSEQ,
            AssignType::MinusEq => MINUSEQ,
            AssignType::TimesEq => TIMESEQ,
            AssignType::DivEq => DIVEQ,
            AssignType::ModEq => MODEQ,
            AssignType::BitShiftRightEq => BITSHIFTRIGHTEQ,
            AssignType::BitShiftLeftEq => BITSHIFTLEFTEQ,
        };
        for sig in self.operations[op.index].signatures.iter() {
            if sig.inputs.len() == 2 && sig.inputs[0] == lhs && sig.inputs[1] == rhs {
                return Some(sig.output);
            }
        }
        None
    }
}
