#![allow(dead_code)]
use crate::operation::Operation;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    pub types: Vec<TypeEntry>,
    types_by_name: HashMap<String, Type>,
    operations: HashMap<Operation, GenericOperation>,
    func_type_cache: HashMap<Signature, Type>,
}

#[derive(Debug, Clone)]
pub enum TypeEntryType {
    UnitType,
    UnknownReturnType,
    BasicType,
    ContainerType(Type),
    FunctionType(FunctionType),
    ResolvedFunctionType(ResolvedFunctionType),
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
    resolved_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctionType {
    signature: Signature,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub output: Type,
}

pub type Type = usize;
pub const UNIT: Type = 0;
pub const UNKNOWN_RETURN: Type = 1;
pub const BUILTIN_FUNCTION: Type = 2;
pub const NULL: Type = 3;
pub const PTR: Type = 4;
pub const BOOL: Type = 5;
pub const CHAR: Type = 6;
pub const INT: Type = 7;
pub const FLOAT: Type = 8;
pub const STR: Type = 9;

impl TypeSystem {
    pub fn new() -> Self {
        let mut rv = Self {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            operations: HashMap::new(),
            func_type_cache: HashMap::new(),
        };
        rv.add_default_types();
        rv.add_default_ops();
        rv
    }

    fn add_default_types(&mut self) {
        self.new_type_with_num(String::from("unit"), TypeEntryType::new_unit(), UNIT);
        self.new_type_with_num(
            String::from("UNKNOWN RETURN"),
            TypeEntryType::new_unknown_return(),
            UNKNOWN_RETURN,
        );
        self.new_type_with_num(
            String::from("builtin function"),
            TypeEntryType::new_basic(),
            BUILTIN_FUNCTION,
        );
        self.new_type_with_num(String::from("null"), TypeEntryType::new_basic(), NULL);
        self.new_type_with_num(String::from("ptr"), TypeEntryType::new_basic(), PTR);
        self.new_type_with_num(String::from("bool"), TypeEntryType::new_basic(), BOOL);
        self.new_type_with_num(String::from("char"), TypeEntryType::new_basic(), CHAR);
        self.new_type_with_num(String::from("int"), TypeEntryType::new_basic(), INT);
        self.new_type_with_num(String::from("float"), TypeEntryType::new_basic(), FLOAT);
        self.new_type_with_num(String::from("str"), TypeEntryType::new_basic(), STR);
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
        let rv = self.types.len() - 1;
        self.types_by_name.insert(name, rv);
        rv
    }

    fn new_type_with_num(&mut self, name: String, te_type: TypeEntryType, expected: Type) -> Type {
        let typ = self.new_type(name, te_type);
        assert_eq!(typ, expected);
        typ
    }

    pub fn list_type(&mut self, t: Type) -> Type {
        match self.types[t].list_type {
            Some(t) => return t,
            None => (),
        }
        let new_name = format!("list<{}>", self.typename(t));
        let list_type = self.new_type(new_name, TypeEntryType::ContainerType(t));
        self.types[t].list_type = Some(list_type);

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
        match self.types[t].te_type {
            TypeEntryType::ContainerType(t) => Some(t),
            _ => None,
        }
    }

    pub fn function_type(&mut self, name: String) -> Type {
        let te_type = TypeEntryType::FunctionType(FunctionType {
            resolved_types: Vec::new(),
        });
        self.new_type(name, te_type)
    }

    pub fn add_signature(&mut self, op: Operation, sig: Signature) {
        self.operations.get_mut(&op).unwrap().signatures.push(sig);
    }

    pub fn typename(&self, t: Type) -> String {
        self.types[t].name.clone()
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
        if lhs == rhs && aop == Operation::Equal {
            return Some(lhs);
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
        match &self.types[func].te_type {
            TypeEntryType::UnknownReturnType => true,
            TypeEntryType::FunctionType(_) => true,
            TypeEntryType::ResolvedFunctionType(_) => true,
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
        let ft = match &self.types[func].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to match a signatures with a non function"),
        };
        for typ in ft.resolved_types.iter() {
            if let TypeEntryType::ResolvedFunctionType(rft) = &self.types[*typ].te_type {
                if &rft.signature.inputs == inputs {
                    return Some(rft.signature.output);
                }
            } else {
                unreachable!()
            }
        }
        None
    }

    pub fn add_function_signature(&mut self, func: Type, sig: Signature) -> usize {
        let typ = self.func_type_from_sig(&sig);
        let ft = match &mut self.types[func].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to add a signature to a non function"),
        };
        ft.resolved_types.push(typ);
        ft.resolved_types.len() - 1
    }

    pub fn func_type_from_sig(&mut self, sig: &Signature) -> Type {
        match self.func_type_cache.get(sig) {
            Some(val) => return *val,
            None => (),
        }
        let name = format!("function{}", self.format_signature(&sig));
        let rv = self.new_type(
            name,
            TypeEntryType::ResolvedFunctionType(ResolvedFunctionType {
                signature: sig.clone(),
            }),
        );
        self.func_type_cache.insert(sig.clone(), rv);
        rv
    }

    pub fn function_type_resolve(&mut self, func: Type, handle: usize) -> Type {
        let ft = match &self.types[func].te_type {
            TypeEntryType::FunctionType(ft) => ft,
            _ => panic!("trying to signature resolve a non function"),
        };
        let signature = match &self.types[ft.resolved_types[handle]].te_type {
            TypeEntryType::ResolvedFunctionType(r) => r.signature.clone(),
            _ => unreachable!(),
        };
        let typename = format!(
            "{}({})",
            self.typename(func),
            self.format_args(&signature.inputs)
        );
        self.new_type(
            typename,
            TypeEntryType::ResolvedFunctionType(ResolvedFunctionType { signature }),
        )
    }

    pub fn get_signatures_for_func(&self, typ: Type) -> Vec<Signature> {
        let rtfs = match &self.types[typ].te_type {
            TypeEntryType::FunctionType(s) => s.resolved_types.clone(),
            t => panic!("Trying to look for concrete type but found {:?}", t),
        };
        let mut rv = Vec::new();
        for typ in rtfs {
            rv.push(self.get_resolved_func_sig(typ))
        }
        rv
    }

    pub fn get_resolved_func_sig_can_fail(&self, typ: Type) -> Option<Signature> {
        match &self.types[typ].te_type {
            TypeEntryType::ResolvedFunctionType(r) => Some(r.signature.clone()),
            _ => None,
        }
    }

    pub fn get_resolved_func_sig(&self, typ: Type) -> Signature {
        match &self.types[typ].te_type {
            TypeEntryType::ResolvedFunctionType(r) => r.signature.clone(),
            t => unreachable!("Expected ResolvedFunctionType found {:?}", t),
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

    pub fn mangle(&self, name: &str, sig: &Signature) -> String {
        format!("{}({})", name, self.format_args(&sig.inputs))
    }
}
