#![allow(dead_code)]
use crate::parser::{AssignType, BinOpType, PostUnOpType, PreUnOpType};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone)]
pub struct TypeSystem {
    types: Vec<TypeEntry>,
    types_by_name: HashMap<String, Type>,
    operations: Vec<Operation>,
    ops_by_name: HashMap<String, Op>,
}

#[derive(Debug, Clone)]
pub enum TypeEntryType {
    UnitType,
    BasicType,
    ContainerType(Type),
    FunctionType(FunctionType),
}

impl TypeEntryType {
    fn new_basic() -> TypeEntryType {
        TypeEntryType::BasicType
    }
    fn new_unit() -> TypeEntryType {
        TypeEntryType::UnitType
    }
}

#[derive(Debug, Clone)]
struct TypeEntry {
    name: String,
    te_type: TypeEntryType,
    list_type: Option<Type>,
}

#[derive(Debug, Clone)]
struct Operation {
    name: String,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub output: Type,
}

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
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

pub const UNIT: Type = Type { index: 0 };
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

pub const BOOLNOT: Op = Op { index: 30 };
pub const BITNOT: Op = Op { index: 31 };
pub const INC: Op = Op { index: 32 };
pub const DEC: Op = Op { index: 33 };

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
        self.new_type(String::from("unit"), TypeEntryType::new_unit());
        self.new_type(String::from("null"), TypeEntryType::new_basic());
        self.new_type(String::from("bool"), TypeEntryType::new_basic());
        self.new_type(String::from("char"), TypeEntryType::new_basic());
        self.new_type(String::from("int"), TypeEntryType::new_basic());
        self.new_type(String::from("float"), TypeEntryType::new_basic());
        self.new_type(String::from("str"), TypeEntryType::new_basic());
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
            FLOAT, FLOAT => FLOAT,
        );

        make_op!("!" |
            BOOL => BOOL,
        );

        make_op!("~", "++", "--" |
            CHAR => CHAR,
            INT => INT,
        );
    }

    fn sanity_check(&self) {
        // This makes sure that we havent messed with anything
        assert_eq!(self.types[UNIT.index].name, "unit");
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
        assert_eq!(self.operations[BOOLNOT.index].name, "!");
        assert_eq!(self.operations[BITNOT.index].name, "~");
        assert_eq!(self.operations[INC.index].name, "++");
        assert_eq!(self.operations[DEC.index].name, "--");
    }

    pub fn find_or_make_type(&mut self, name: String, te_type: TypeEntryType) -> Type {
        match self.types_by_name.get(&name) {
            Some(a) => *a,
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
            list_type: None,
            name: name.clone(),
            te_type,
        });
        let rv = Type {
            index: self.types.len() - 1,
        };
        self.types_by_name.insert(name, rv);
        rv
    }

    pub fn list_type(&mut self, t: Type) -> Type {
        match self.types[t.index].list_type {
            Some(t) => return t,
            None => (),
        }
        let new_name = format!("list<{}>", self.typename(t));
        let list_type = self.new_type(new_name, TypeEntryType::ContainerType(t));
        self.types[t.index].list_type = Some(list_type);

        self.add_signature(
            EQUAL,
            Signature {
                inputs: vec![list_type, list_type],
                output: list_type,
            },
        );

        return list_type;
    }

    pub fn underlying_type(&mut self, t: Type) -> Option<Type> {
        match self.types[t.index].te_type {
            TypeEntryType::ContainerType(t) => Some(t),
            _ => None,
        }
    }

    pub fn function_type(&mut self, name: String) -> Type {
        let te_type = TypeEntryType::FunctionType(FunctionType {
            signatures: Vec::new(),
        });
        self.new_type(name, te_type)
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
        self.types[t.index].name.clone()
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

    pub fn lookup_preunop(&self, puop: PreUnOpType, t: Type) -> Option<Type> {
        let op = match puop {
            PreUnOpType::BoolNot => BOOLNOT,
            PreUnOpType::BitNot => BITNOT,
            PreUnOpType::Minus => MINUS,
            PreUnOpType::Plus => PLUS,
            PreUnOpType::Inc => INC,
            PreUnOpType::Dec => DEC,
            PreUnOpType::Times => TIMES,
        };

        for sig in self.operations[op.index].signatures.iter() {
            if sig.inputs.len() == 1 && sig.inputs[0] == t {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_postunop(&self, puop: PostUnOpType, t: Type) -> Option<Type> {
        let op = match puop {
            PostUnOpType::Inc => INC,
            PostUnOpType::Dec => DEC,
        };

        for sig in self.operations[op.index].signatures.iter() {
            if sig.inputs.len() == 1 && sig.inputs[0] == t {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn is_function(&self, func: Type) -> bool {
        match &self.types[func.index].te_type {
            TypeEntryType::FunctionType(_) => true,
            _ => false,
        }
    }

    pub fn match_signature(&self, func: Type, inputs: &Vec<Type>) -> Option<Type> {
        let ft = match &self.types[func.index].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to match a signatures with a non function"),
        };
        for sig in ft.signatures.iter() {
            if &sig.inputs == inputs {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn add_function_signature(&mut self, func: Type, sig: Signature) {
        let ft = match &mut self.types[func.index].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to add a signature to a non function"),
        };
        ft.signatures.push(sig);
    }
}
