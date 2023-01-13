use crate::operation::Operation;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    pub types: Vec<TypeEntry>,
    types_by_name: HashMap<String, Type>,
    operations: HashMap<Operation, GenericOperation>,
}

#[derive(Debug, Clone)]
pub enum TypeEntryType {
    Unit,
    UnknownReturn,
    Basic,
    List(Type),
    Option(Type),
    Function,
    Class,
    ClassVariant(Type),
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
pub const EMPTYLIST: Type = 13;

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
        };
        rv.add_default_types();
        rv.add_default_ops();
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
        let null = self.option_type_with_name(UNREACHABLE, "null".into());
        debug_assert_eq!(null, NULL);
        self.new_type_with_num("ptr".into(), TypeEntryType::Basic, PTR);
        self.new_type_with_num("bool".into(), TypeEntryType::Basic, BOOL);
        self.new_type_with_num("char".into(), TypeEntryType::Basic, CHAR);
        self.new_type_with_num("int".into(), TypeEntryType::Basic, INT);
        self.new_type_with_num("float".into(), TypeEntryType::Basic, FLOAT);
        self.new_type_with_num("str".into(), TypeEntryType::List(CHAR), STR);
        let opt_str = self.option_type(STR);
        debug_assert_eq!(opt_str, OPT_STR);
        let emptylist = self.list_type_with_name(UNREACHABLE, "[]".into());
        debug_assert_eq!(emptylist, EMPTYLIST);
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

        make_op!(Deref |
            NULL => UNREACHABLE,
        );
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
        // This is mainly for default args
        // But also a little hacky because we have no gaurantee that NULL exists yet
        self.add_signature(Operation::BoolOr, sig!(NULL, rv => rv));
        self.add_signature(Operation::Extract, sig!(rv, NULL => BOOL));
        rv
    }

    fn new_type_with_num(&mut self, name: String, te_type: TypeEntryType, expected: Type) -> Type {
        let typ = self.new_type(name.clone(), te_type);
        debug_assert_eq!(typ, expected, "{name}");
        typ
    }

    pub fn list_type(&mut self, t: Type) -> Type {
        let new_name = format!("[{}]", self.typename(t));
        self.list_type_with_name(t, new_name)
    }
    pub fn list_type_with_name(&mut self, t: Type, new_name: String) -> Type {
        if let Some(t) = self.types[t].list_type {
            return t;
        }
        let list_type = self.new_type(new_name, TypeEntryType::List(t));
        self.types[t].list_type = Some(list_type);

        self.add_signature(Operation::Equal, sig!(list_type, list_type => list_type));
        self.add_signature(Operation::Plus, sig!(list_type, list_type => list_type));
        self.add_signature(Operation::PlusEq, sig!(list_type, list_type => list_type));
        self.add_signature(Operation::Plus, sig!(EMPTYLIST, list_type => list_type));
        self.add_signature(Operation::Plus, sig!(list_type, EMPTYLIST => list_type));

        self.add_signature(Operation::PlusEq, sig!(EMPTYLIST, list_type => list_type));
        self.add_signature(Operation::Equal, sig!(EMPTYLIST, list_type => list_type));
        self.add_signature(Operation::Equal, sig!(list_type, EMPTYLIST => list_type));

        list_type
    }

    pub fn option_type(&mut self, t: Type) -> Type {
        let new_name = format!("{}?", self.typename(t));
        self.option_type_with_name(t, new_name)
    }

    fn option_type_with_name(&mut self, t: Type, new_name: String) -> Type {
        if let Some(t) = self.types[t].option_type {
            return t;
        }
        let op_type = self.new_type(new_name, TypeEntryType::Option(t));
        self.types[t].option_type = Some(op_type);

        self.add_signature(Operation::Equal, sig!(op_type,op_type => op_type));
        self.add_signature(Operation::Equal, sig!(op_type, NULL => op_type));
        self.add_signature(Operation::Equal, sig!(op_type, t => op_type));
        self.add_signature(Operation::CmpEq, sig!(op_type, op_type => BOOL));
        self.add_signature(Operation::CmpNotEq, sig!(op_type,op_type => BOOL));
        self.add_signature(Operation::Deref, sig!(op_type => t));

        self.add_signature(Operation::Extract, sig!(t, op_type => BOOL));
        self.add_signature(Operation::BoolOr, sig!(op_type, op_type => op_type));
        self.add_signature(Operation::BoolOr, sig!(op_type, t => t));
        self.add_signature(Operation::BoolAnd, sig!(op_type, op_type => op_type));
        self.add_signature(Operation::BoolXor, sig!(op_type, op_type => op_type));

        if let Some(BOOL) = self.lookup_binop(Operation::CmpEq, t, t) {
            self.add_signature(Operation::CmpEq, sig!(t, op_type => BOOL));
            self.add_signature(Operation::CmpEq, sig!(op_type, t => BOOL));
        }
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
        let te_type = TypeEntryType::Function;
        self.new_type(name, te_type)
    }

    pub fn class_type(&mut self, name: String) -> Type {
        self.new_type(name, TypeEntryType::Class)
    }

    pub fn class_variant(&mut self, base: Type) -> Type {
        let name = if cfg!(debug_assertions) {
            self.typename(base) + "-variant"
        } else {
            self.typename(base)
        };
        let te_type = TypeEntryType::ClassVariant(base);
        self.new_type(name, te_type)
    }

    pub fn add_signature(&mut self, op: Operation, sig: Signature) {
        self.ensure_op(op);
        self.operations.get_mut(&op).unwrap().signatures.push(sig);
    }

    pub fn typename(&self, t: Type) -> String {
        self.types[t].name.clone()
    }

    pub fn lookup_binop(&self, binop: Operation, lhs: Type, rhs: Type) -> Option<Type> {
        debug_assert!(binop.is_binop());
        let shortcut_types = [UNKNOWN_RETURN, UNREACHABLE];
        for st in shortcut_types {
            if lhs == st || rhs == st {
                return Some(st);
            }
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
        if lhs == UNREACHABLE {
            return if aop == Operation::Extract {
                Some(BOOL)
            } else {
                Some(rhs)
            };
        }
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
            TypeEntryType::UnknownReturn | TypeEntryType::Function
        )
    }

    pub fn is_class(&self, func: Type) -> bool {
        matches!(&self.types[func].te_type, TypeEntryType::Class)
    }

    pub fn is_object(&self, func: Type) -> bool {
        matches!(&self.types[func].te_type, TypeEntryType::ClassVariant(_))
    }

    pub fn is_option(&self, opt: Type) -> bool {
        matches!(self.types[opt].te_type, TypeEntryType::Option(_))
    }

    pub fn is_list(&self, opt: Type) -> bool {
        matches!(self.types[opt].te_type, TypeEntryType::List(_))
    }

    pub fn get_typeof_list_type(&self, list: Type) -> Option<Type> {
        match self.types[list].te_type {
            TypeEntryType::List(t) => Some(t),
            _ => None,
        }
    }

    pub fn get_typeof_option_type(&self, list: Type) -> Option<Type> {
        match self.types[list].te_type {
            TypeEntryType::Option(t) => Some(t),
            _ => None,
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

    pub fn format_method_sig(&self, sig: &Signature, name: String) -> String {
        let inputs = sig.inputs.clone();
        let first = inputs[0];
        let sig = Signature::new(inputs, None, sig.output);
        format!(
            "{}.{name}{}",
            self.typename(first),
            self.format_signature(&sig)
        )
    }
}
