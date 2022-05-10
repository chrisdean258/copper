#![allow(dead_code)]
use crate::operation::Operation;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Clone)]
pub struct TypeSystem {
    pub types: Vec<TypeEntry>,
    types_by_name: HashMap<String, Type>,
    operations: HashMap<Operation, GenericOperation>,
    ops_by_name: HashMap<String, Op>,
    func_type_cache: HashMap<Signature, Type>,
}

#[derive(Debug, Clone)]
pub enum TypeEntryType {
    UnitType,
    UnknownReturnType,
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
    fn new_unknown_return() -> TypeEntryType {
        TypeEntryType::UnknownReturnType
    }
}

#[derive(Debug, Clone)]
pub struct TypeEntry {
    name: String,
    te_type: TypeEntryType,
    list_type: Option<Type>,
}

#[derive(Debug, Clone)]
struct GenericOperation {
    operation: Operation,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub output: Type,
}

#[derive(Clone, Copy, PartialEq, Hash, Eq)]
pub struct Type {
    index: usize,
}

impl Type {
    #[inline]
    pub fn encode(&self) -> u64 {
        self.index as u64
    }
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
pub const UNKNOWN_RETURN: Type = Type { index: 1 };
pub const BUILTIN_FUNCTION: Type = Type { index: 2 };
pub const NULL: Type = Type { index: 3 };
pub const BOOL: Type = Type { index: 4 };
pub const CHAR: Type = Type { index: 5 };
pub const INT: Type = Type { index: 6 };
pub const FLOAT: Type = Type { index: 7 };
pub const STR: Type = Type { index: 8 };

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let s = format!("{}", self.index);
        f.write_str(match self.index {
            0 => "unit",
            1 => "UNKNOWN RETURN",
            2 => "builtin function",
            3 => "null",
            4 => "bool",
            5 => "char",
            6 => "int",
            7 => "float",
            8 => "str",
            _ => &s,
        })
    }
}

impl TypeSystem {
    pub fn new() -> Self {
        let mut rv = Self {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            operations: HashMap::new(),
            ops_by_name: HashMap::new(),
            func_type_cache: HashMap::new(),
        };
        rv.add_default_types();
        rv.add_default_ops();
        rv
    }

    fn add_default_types(&mut self) {
        self.new_type(String::from("unit"), TypeEntryType::new_unit());
        self.new_type(
            String::from("UNKNOWN RETURN"),
            TypeEntryType::new_unknown_return(),
        );
        self.new_type(String::from("builtin function"), TypeEntryType::new_basic());
        self.new_type(String::from("null"), TypeEntryType::new_basic());
        self.new_type(String::from("bool"), TypeEntryType::new_basic());
        self.new_type(String::from("char"), TypeEntryType::new_basic());
        self.new_type(String::from("int"), TypeEntryType::new_basic());
        self.new_type(String::from("float"), TypeEntryType::new_basic());
        self.new_type(String::from("str"), TypeEntryType::new_basic());
    }

    fn add_default_ops(&mut self) {
        macro_rules! make_op {
            ($($operation: ident),+ | $($( $input: ident),+ => $returntype:ident),* $(,)? ) => {
                let ops = vec![$(Operation::$operation),+];
                for op in ops {
                    self.ensure_op(op);
                    $(self.add_signature(op, Signature {
                        inputs: vec![$( $input ),+],
                        output: $returntype,
                    });)*
                }
            };
        }
        make_op!(BoolOr, BoolXor, BoolAnd |
            BOOL, BOOL => BOOL,
        );
        make_op!(BitOr, BitXor, BitAnd |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );
        make_op!(CmpGE, CmpGT, CmpLE, CmpLT, CmpEq, CmpNotEq |
            INT, INT => BOOL,
            CHAR, CHAR => BOOL,
            FLOAT, FLOAT => BOOL,
            STR, STR => BOOL,
        );

        make_op!(BitShiftLeft, BitShiftRight |
            CHAR, CHAR => CHAR,
            CHAR, INT => CHAR,
            INT, INT => INT,
        );

        make_op!(Minus, Plus, Times, Mod, Div  |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!(Minus, Plus, Times, Div  |
            FLOAT, FLOAT => FLOAT
        );

        make_op!(Plus |
             STR, STR => STR
        );

        make_op!(Equal |
           NULL, NULL => NULL,
           BOOL, BOOL => BOOL,
           CHAR, CHAR => CHAR,
           INT, INT => INT,
           FLOAT, FLOAT => FLOAT,
           STR, STR => STR,
        );

        make_op!(AndEq, XorEq, OrEq |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!(PlusEq, MinusEq, TimesEq, DivEq, ModEq, BitShiftRightEq, BitShiftLeftEq |
            CHAR, CHAR => CHAR,
            INT, INT => INT,
        );

        make_op!(MinusEq, PlusEq, TimesEq, DivEq  |
            FLOAT, FLOAT => FLOAT,
        );

        make_op!(BoolNot |
            BOOL => BOOL,
        );

        make_op!(BitNot, PreInc, PreDec, PostInc, PostDec |
            CHAR => CHAR,
            INT => INT,
        );
    }

    pub fn find_or_make_type(&mut self, name: String, te_type: TypeEntryType) -> Type {
        match self.types_by_name.get(&name) {
            Some(a) => *a,
            None => self.new_type(name, te_type),
        }
    }

    pub fn ensure_op(&mut self, operation: Operation) {
        self.operations
            .entry(operation)
            .or_insert_with(|| GenericOperation {
                operation,
                signatures: Vec::new(),
            });
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

    /* pub fn func_type(&mut self, sig: Signature) -> Type {
        match self.func_type_cache.get(&sig) {
            Some(val) => return *val,
            None => (),
        }
        let name = format!("function{}", self.format_signature(sig));
        let rv = self.new_type(name, TypeEntryType::ConcreteFunctionType(sig.clone()));
        self.func_type_cache.insert(sig, rv);
        rv
    } */

    pub fn list_type(&mut self, t: Type) -> Type {
        match self.types[t.index].list_type {
            Some(t) => return t,
            None => (),
        }
        let new_name = format!("list<{}>", self.typename(t));
        let list_type = self.new_type(new_name, TypeEntryType::ContainerType(t));
        self.types[t.index].list_type = Some(list_type);

        self.add_signature(
            Operation::Equal,
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

    pub fn add_signature(&mut self, op: Operation, sig: Signature) {
        self.operations.get_mut(&op).unwrap().signatures.push(sig);
    }

    pub fn typename(&self, t: Type) -> String {
        self.types[t.index].name.clone()
    }

    pub fn lookup_binop(&self, binop: Operation, lhs: Type, rhs: Type) -> Option<Type> {
        assert!(binop.is_binop());
        if lhs == UNKNOWN_RETURN || rhs == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }
        for sig in self.operations.get(&binop).unwrap().signatures.iter() {
            if sig.inputs.len() == 2 && sig.inputs[0] == lhs && sig.inputs[1] == rhs {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_assign(&self, aop: Operation, lhs: Type, rhs: Type) -> Option<Type> {
        assert!(aop.is_assignop());
        if lhs == UNKNOWN_RETURN || rhs == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }
        for sig in self.operations.get(&aop).unwrap().signatures.iter() {
            if sig.inputs.len() == 2 && sig.inputs[0] == lhs && sig.inputs[1] == rhs {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_preunop(&self, puop: Operation, t: Type) -> Option<Type> {
        assert!(puop.is_preunop());
        if t == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }

        for sig in self.operations.get(&puop).unwrap().signatures.iter() {
            if sig.inputs.len() == 1 && sig.inputs[0] == t {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_postunop(&self, puop: Operation, t: Type) -> Option<Type> {
        assert!(puop.is_postunop());
        if t == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }
        for sig in self.operations.get(&puop).unwrap().signatures.iter() {
            if sig.inputs.len() == 1 && sig.inputs[0] == t {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn is_function(&self, func: Type) -> bool {
        match &self.types[func.index].te_type {
            TypeEntryType::UnknownReturnType => true,
            TypeEntryType::FunctionType(_) => true,
            _ if func == BUILTIN_FUNCTION => true,
            _ => false,
        }
    }

    pub fn match_signature(&self, func: Type, inputs: &Vec<Type>) -> Option<Type> {
        for arg in inputs {
            if *arg == UNKNOWN_RETURN {
                return Some(UNKNOWN_RETURN);
            }
        }
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

    pub fn add_function_signature(&mut self, func: Type, sig: Signature) -> usize {
        let ft = match &mut self.types[func.index].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to add a signature to a non function"),
        };
        ft.signatures.push(sig);
        ft.signatures.len() - 1
    }

    pub fn patch_signature_return(&mut self, func: Type, handle: usize, return_type: Type) {
        let ft = match &mut self.types[func.index].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to patch signature to a non function"),
        };
        ft.signatures[handle].output = return_type;
    }

    pub fn get_signatures_for_func(&self, typ: Type) -> Vec<Signature> {
        match &self.types[typ.index].te_type {
            TypeEntryType::FunctionType(s) => s.signatures.clone(),
            t => panic!("Trying to look for concrete type but found {:?}", t),
        }
    }

    pub fn format_signature(&self, sig: &Signature) -> String {
        let arg_types = self.format_args(&sig.inputs);
        format!("({}) -> {}", arg_types, self.typename(sig.output))
    }

    pub fn format_args(&self, args: &Vec<Type>) -> String {
        args.iter()
            .map(|t| self.typename(*t).clone())
            .collect::<Vec<String>>()
            .join(", ")
    }
}
