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
    Unit,
    UnknownReturn,
    Basic,
    List(Type),
    Option(Type),
    Function(Function),
    Class(Class),
    ResolvedClass(ResolvedClass),
    UnresolvedClass(UnresolvedClass),
    ResolvedFunction(ResolvedFunction),
}

#[derive(Debug, Clone)]
pub struct TypeEntry {
    name: String,
    te_type: TypeEntryType,
    list_type: Option<Type>,
    option_type: Option<Type>,
}

#[derive(Debug, Clone)]
struct GenericOperation {
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct Class {
    // resolved_types: HashSet<Type>,
    // unresolved_types: HashSet<Type>,
    num_fields: usize,
}

#[derive(Debug, Clone)]
pub struct UnresolvedClass {
    fields: Vec<Option<Type>>,
    class: Type,
}

#[derive(Debug, Clone)]
pub struct ResolvedClass {
    fields: Vec<Type>,
    class: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    resolved_types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct ResolvedFunction {
    signature: Signature,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct BoundFunction {
    signature: Signature,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub output: Type,
    pub repeated_inputs: Option<Type>,
}

#[macro_export]
macro_rules! sig {
    ($($ty:expr),*; $repty:expr, ... => $outty:expr) => {
         Signature {
            inputs: vec![$($ty),*],
            output: $outty,
            repeated_inputs: Some($repty),
        }
    };
    ($($ty:expr),* $(,)? => $outty:expr) => {
         Signature {
            inputs: vec![$($ty),*],
            output: $outty,
            repeated_inputs: None,
        }
    };
    ($repty:expr, ... => $outty:expr) => {
         Signature {
            inputs: vec![],
            output: $outty,
            repeated_inputs: Some($repty),
        }
    };
    (=> $outty:expr) => {
         Signature {
            inputs: vec![],
            output: $outty,
            repeated_inputs: None,
        }
    }
}
pub(crate) use sig;

impl Signature {
    pub fn new(inputs: Vec<Type>, repeated_inputs: Option<Type>, output: Type) -> Self {
        Self {
            inputs,
            output,
            repeated_inputs,
        }
    }

    pub fn match_inputs(&self, inputs: &[Type]) -> Option<Type> {
        let inputs_eq = self.inputs.iter().zip(inputs).all(|(a, b)| a.matches(b));
        if !inputs_eq {
            return None;
        }
        if self.inputs.len() == inputs.len() {
            return Some(self.output);
        }
        if let Some(t) = self.repeated_inputs {
            if inputs[self.inputs.len()..].iter().all(|a| a.matches(&t)) {
                return Some(self.output);
            }
        }
        None
    }
}

pub type Type = usize;
pub const UNIT: Type = 0;
pub const UNKNOWN_RETURN: Type = 1;
pub const ANY: Type = 2;
pub const UNREACHABLE: Type = 3;
pub const BUILTIN_FUNCTION: Type = 4;
pub const NULL: Type = 5;
pub const PTR: Type = 6;
pub const BOOL: Type = 7;
pub const CHAR: Type = 8;
pub const INT: Type = 9;
pub const FLOAT: Type = 10;
pub const STR: Type = 11;
pub const OPT_STR: Type = 12;

trait TypeMatch {
    fn matches(&self, other: &Type) -> bool;
}

impl TypeMatch for Type {
    fn matches(&self, other: &Type) -> bool {
        self == other || *self == ANY || *other == ANY
    }
}

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
        // This is somewhat of a hack because getline returns an option string so we need to to be
        // determinable.
        let opt_str = rv.option_type(STR);
        debug_assert_eq!(opt_str, OPT_STR);
        rv
    }

    pub fn print_types(&self) {
        eprintln!("Types:");
        for (i, typ) in self.types.iter().enumerate() {
            eprintln!("  {i:02}: {}", typ.name)
        }
    }

    fn add_default_types(&mut self) {
        self.new_type_with_num(String::from("unit"), TypeEntryType::Unit, UNIT);
        self.new_type_with_num(
            String::from("UNKNOWN RETURN"),
            TypeEntryType::UnknownReturn,
            UNKNOWN_RETURN,
        );
        self.new_type_with_num(String::from("anytype"), TypeEntryType::Unit, ANY);
        self.new_type_with_num(
            String::from("unreachable"),
            TypeEntryType::Unit,
            UNREACHABLE,
        );
        self.new_type_with_num(
            String::from("builtin function"),
            TypeEntryType::Basic,
            BUILTIN_FUNCTION,
        );
        self.new_type_with_num(String::from("null"), TypeEntryType::Basic, NULL);
        self.new_type_with_num(String::from("ptr"), TypeEntryType::Basic, PTR);
        self.new_type_with_num(String::from("bool"), TypeEntryType::Basic, BOOL);
        self.new_type_with_num(String::from("char"), TypeEntryType::Basic, CHAR);
        self.new_type_with_num(String::from("int"), TypeEntryType::Basic, INT);
        self.new_type_with_num(String::from("float"), TypeEntryType::Basic, FLOAT);
        self.new_type_with_num(String::from("str"), TypeEntryType::List(CHAR), STR);
    }

    fn add_default_ops(&mut self) {
        macro_rules! make_op {
            ($($operation: ident),+ | $($( $input: ident),+ => $returntype:ident),* $(,)? ) => {
                let ops = vec![$(Operation::$operation),+];
                for op in ops {
                    self.ensure_op(op);
                    $(self.add_signature(op, sig!($($input),+ => $returntype));)*
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
            CHAR, INT => INT,
            INT, CHAR => INT,
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
            CHAR, INT => CHAR,
            INT, CHAR => INT,
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

        make_op!(UnaryPlus, Negate |
            CHAR => CHAR,
            INT => INT,
            FLOAT => FLOAT,
        );

        self.ensure_op(Operation::Deref);
        self.ensure_op(Operation::Extract);
    }

    pub fn ensure_op(&mut self, operation: Operation) {
        self.operations
            .entry(operation)
            .or_insert_with(|| GenericOperation {
                signatures: Vec::new(),
            });
    }

    fn new_type(&mut self, name: String, te_type: TypeEntryType) -> Type {
        self.types.push(TypeEntry {
            list_type: None,
            option_type: None,
            name: name.clone(),
            te_type,
        });
        let rv = self.types.len() - 1;
        self.types_by_name.insert(name, rv);
        rv
    }

    fn new_type_with_num(&mut self, name: String, te_type: TypeEntryType, expected: Type) -> Type {
        let typ = self.new_type(name, te_type);
        debug_assert_eq!(typ, expected);
        typ
    }

    pub fn list_type(&mut self, t: Type) -> Type {
        if let Some(t) = self.types[t].list_type {
            return t;
        }
        let new_name = format!("list<{}>", self.typename(t));
        let list_type = self.new_type(new_name, TypeEntryType::List(t));
        self.types[t].list_type = Some(list_type);

        self.add_signature(Operation::Equal, sig!(list_type, list_type => list_type));
        self.add_signature(Operation::Plus, sig!(list_type, list_type => list_type));

        list_type
    }

    pub fn option_type(&mut self, t: Type) -> Type {
        if let Some(t) = self.types[t].option_type {
            return t;
        }
        let new_name = format!("option<{}>", self.typename(t));
        let op_type = self.new_type(new_name, TypeEntryType::Option(t));
        self.types[t].option_type = Some(op_type);

        self.add_signature(Operation::Equal, sig!(op_type,op_type => op_type));
        self.add_signature(Operation::Equal, sig!(op_type, NULL => op_type));
        self.add_signature(Operation::Equal, sig!(op_type, t => op_type));
        self.add_signature(Operation::CmpEq, sig!(op_type,op_type => BOOL));
        self.add_signature(Operation::CmpNotEq, sig!(op_type,op_type => BOOL));
        self.add_signature(Operation::Deref, sig!(op_type => t));

        self.add_signature(Operation::Extract, sig!(t, op_type => BOOL));
        self.add_signature(Operation::BoolOr, sig!(op_type, op_type => op_type));
        self.add_signature(Operation::BoolOr, sig!(op_type, t => t));
        self.add_signature(Operation::BoolAnd, sig!(op_type, op_type => op_type));
        self.add_signature(Operation::BoolXor, sig!(op_type, op_type => op_type));

        op_type
    }

    pub fn underlying_type(&self, t: Type) -> Option<Type> {
        match self.types[t].te_type {
            TypeEntryType::List(t) => Some(t),
            TypeEntryType::Option(t) => Some(t),
            _ => None,
        }
    }

    pub fn function_type(&mut self, name: String) -> Type {
        let te_type = TypeEntryType::Function(Function {
            resolved_types: Vec::new(),
        });
        self.new_type(name, te_type)
    }

    pub fn class_type(&mut self, name: String, num_fields: usize) -> Type {
        let te_type = TypeEntryType::Class(Class {
            // resolved_types: HashSet::new(),
            // unresolved_types: HashSet::new(),
            num_fields,
        });
        self.new_type(name, te_type)
    }

    pub fn class_query_field(&self, cls: Type, idx: usize) -> Option<Type> {
        match &self.types[cls].te_type {
            TypeEntryType::ResolvedClass(r) => Some(r.fields[idx]),
            TypeEntryType::UnresolvedClass(u) => u.fields[idx],
            _ => panic!("querying {} as (Un)ResolvedClassType", self.typename(cls)),
        }
    }

    pub fn set_field_type(&mut self, cls: Type, idx: usize, typ: Type) {
        match &mut self.types[cls].te_type {
            TypeEntryType::UnresolvedClass(u) => u.fields[idx] = Some(typ),
            _ => panic!("querying {} as UnresolvedClassType", self.typename(cls)),
        }
    }

    pub fn class_new_unresolved(&mut self, cls: Type) -> Type {
        let name = self.types[cls].name.clone();
        let num_fields = match &mut self.types[cls].te_type {
            TypeEntryType::Class(c) => c.num_fields,
            _ => unreachable!(),
        };

        self.new_type(
            name,
            TypeEntryType::UnresolvedClass(UnresolvedClass {
                fields: vec![None; num_fields],
                class: cls,
            }),
        )
    }

    #[allow(dead_code)]
    pub fn resolve_class(&mut self, cls: Type) {
        self.types[cls].te_type = match &self.types[cls].te_type {
            TypeEntryType::UnresolvedClass(c) => TypeEntryType::ResolvedClass(ResolvedClass {
                fields: c.fields.iter().map(|x| x.unwrap_or(UNIT)).collect(),
                class: c.class,
            }),

            _ => unreachable!(),
        };
    }

    pub fn class_underlying(&self, cls: Type) -> Type {
        match &self.types[cls].te_type {
            TypeEntryType::UnresolvedClass(c) => c.class,
            TypeEntryType::ResolvedClass(c) => c.class,
            _ => panic!("querying {} as UnresolvedClassType", self.typename(cls)),
        }
    }

    pub fn add_signature(&mut self, op: Operation, sig: Signature) {
        self.operations.get_mut(&op).unwrap().signatures.push(sig);
    }

    pub fn typename(&self, t: Type) -> String {
        self.types[t].name.clone()
    }

    pub fn lookup_binop(&self, binop: Operation, lhs: Type, rhs: Type) -> Option<Type> {
        debug_assert!(binop.is_binop());
        if lhs == UNKNOWN_RETURN || rhs == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }
        for sig in self.operations.get(&binop).unwrap().signatures.iter() {
            if sig.inputs == [lhs, rhs] {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_assign(&self, aop: Operation, lhs: Type, rhs: Type) -> Option<Type> {
        debug_assert!(aop.is_assignop());
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
        debug_assert!(puop.is_preunop());
        if t == UNKNOWN_RETURN {
            return Some(UNKNOWN_RETURN);
        }

        for sig in self.operations.get(&puop).unwrap().signatures.iter() {
            if sig.inputs == [t] {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn lookup_postunop(&self, puop: Operation, t: Type) -> Option<Type> {
        debug_assert!(puop.is_postunop());
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
        if func == BUILTIN_FUNCTION {
            return true;
        }
        matches!(
            &self.types[func].te_type,
            TypeEntryType::UnknownReturn
                | TypeEntryType::Function(_)
                | TypeEntryType::ResolvedFunction(_)
        )
    }

    pub fn is_class(&self, func: Type) -> bool {
        matches!(&self.types[func].te_type, TypeEntryType::Class(_))
    }

    pub fn is_option(&self, opt: Type) -> bool {
        matches!(self.types[opt].te_type, TypeEntryType::Option(_))
    }

    pub fn is_list(&self, list: Type) -> bool {
        matches!(self.types[list].te_type, TypeEntryType::List(_))
    }

    pub fn function_get_resolved(&self, func: Type, args: &[Type]) -> Option<Type> {
        let ft = match &self.types[func].te_type {
            TypeEntryType::Function(ft) => ft,
            _ => panic!("trying to match a signatures with a non function"),
        };
        for typ in ft.resolved_types.iter() {
            if let TypeEntryType::ResolvedFunction(rft) = &self.types[*typ].te_type {
                if rft.signature.inputs == args {
                    return Some(*typ);
                }
            } else {
                unreachable!()
            }
        }
        None
    }

    pub fn match_signature(&self, func: Type, inputs: &[Type]) -> Option<Type> {
        if inputs.iter().any(|&a| a == UNKNOWN_RETURN) {
            return Some(UNKNOWN_RETURN);
        }
        match self.function_get_resolved(func, inputs) {
            None => None,
            Some(t) => {
                if let TypeEntryType::ResolvedFunction(rft) = &self.types[t].te_type {
                    Some(rft.signature.output)
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub fn func_type_from_sig(&mut self, sig: &Signature) -> Type {
        if let Some(val) = self.func_type_cache.get(sig) {
            return *val;
        }
        let name = format!("function{}", self.format_signature(sig));
        let rv = self.new_type(
            name,
            TypeEntryType::ResolvedFunction(ResolvedFunction {
                signature: sig.clone(),
            }),
        );
        self.func_type_cache.insert(sig.clone(), rv);
        rv
    }

    pub fn add_function_signature(&mut self, func: Type, sig: Signature) -> Type {
        let typ = self.func_type_from_sig(&sig);
        match &mut self.types[func].te_type {
            TypeEntryType::Function(ft) => ft.resolved_types.push(typ),
            _ => panic!("trying to signature resolve a non function"),
        };
        let typename = format!("{}({})", self.typename(func), self.format_args(&sig.inputs));
        self.new_type(
            typename,
            TypeEntryType::ResolvedFunction(ResolvedFunction { signature: sig }),
        )
    }

    pub fn get_signatures_for_func(&self, typ: Type) -> Vec<Signature> {
        let rtfs = match &self.types[typ].te_type {
            TypeEntryType::Function(s) => s.resolved_types.clone(),
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
            TypeEntryType::ResolvedFunction(r) => Some(r.signature.clone()),
            _ => None,
        }
    }

    pub fn get_resolved_func_sig(&self, typ: Type) -> Signature {
        match &self.types[typ].te_type {
            TypeEntryType::ResolvedFunction(r) => r.signature.clone(),
            t => unreachable!("Expected ResolvedFunction found {:?}", t),
        }
    }

    pub fn format_signature(&self, sig: &Signature) -> String {
        let arg_types = self.format_args_from_sig(sig);
        format!("({}) -> {}", arg_types, self.typename(sig.output))
    }

    pub fn format_args(&self, args: &[Type]) -> String {
        args.iter()
            .map(|t| self.typename(*t))
            .collect::<Vec<String>>()
            .join(", ")
    }

    pub fn format_args_repeated(&self, args: &[Type], repeated: Type) -> String {
        if args.is_empty() {
            format!("*{}", self.typename(repeated))
        } else {
            format!("{}, *{}", self.format_args(args), self.typename(repeated))
        }
    }

    pub fn format_args_from_sig(&self, sig: &Signature) -> String {
        if let Some(t) = sig.repeated_inputs {
            self.format_args_repeated(&sig.inputs, t)
        } else {
            self.format_args(&sig.inputs)
        }
    }

    pub fn mangle(&self, name: &str, sig: &Signature) -> String {
        format!("{}({})", name, self.format_args_from_sig(sig))
    }
}
