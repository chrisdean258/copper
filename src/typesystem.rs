#![allow(non_upper_case_globals)]
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq)]
pub struct TypeRef {
    pub idx: usize,
}
#[derive(Clone, Debug, Copy, Hash, PartialEq, Eq)]
pub struct OpRef {
    pub idx: usize,
}

pub const Undefined: TypeRef = TypeRef { idx: 0 };
pub const Null: TypeRef = TypeRef { idx: 1 };
pub const Float: TypeRef = TypeRef { idx: 2 };
pub const Int: TypeRef = TypeRef { idx: 3 };
pub const Char: TypeRef = TypeRef { idx: 4 };
pub const Bool: TypeRef = TypeRef { idx: 5 };
pub const EmptyList: TypeRef = TypeRef { idx: 6 };
pub const Str: TypeRef = TypeRef { idx: 7 };

pub const CmpEq: OpRef = OpRef { idx: 0 };
pub const CmpNotEq: OpRef = OpRef { idx: 1 };
pub const Plus: OpRef = OpRef { idx: 2 };
pub const Minus: OpRef = OpRef { idx: 3 };
pub const Times: OpRef = OpRef { idx: 4 };
pub const Div: OpRef = OpRef { idx: 5 };
pub const CmpGE: OpRef = OpRef { idx: 6 };
pub const CmpLE: OpRef = OpRef { idx: 7 };
pub const CmpGT: OpRef = OpRef { idx: 8 };
pub const CmpLT: OpRef = OpRef { idx: 9 };
pub const Mod: OpRef = OpRef { idx: 10 };
pub const BitOr: OpRef = OpRef { idx: 11 };
pub const BitAnd: OpRef = OpRef { idx: 12 };
pub const BitXor: OpRef = OpRef { idx: 13 };
pub const BitShiftLeft: OpRef = OpRef { idx: 14 };
pub const BitShiftRight: OpRef = OpRef { idx: 15 };
pub const Inc: OpRef = OpRef { idx: 16 };
pub const Dec: OpRef = OpRef { idx: 17 };
pub const BoolNot: OpRef = OpRef { idx: 18 };
pub const BitNot: OpRef = OpRef { idx: 19 };
pub const BoolOr: OpRef = OpRef { idx: 20 };
pub const BoolAnd: OpRef = OpRef { idx: 21 };
pub const BoolXor: OpRef = OpRef { idx: 22 };
pub const PlusEq: OpRef = OpRef { idx: 23 };
pub const MinusEq: OpRef = OpRef { idx: 24 };
pub const TimesEq: OpRef = OpRef { idx: 25 };
pub const DivEq: OpRef = OpRef { idx: 26 };
pub const ModEq: OpRef = OpRef { idx: 27 };
pub const OrEq: OpRef = OpRef { idx: 28 };
pub const XorEq: OpRef = OpRef { idx: 29 };
pub const AndEq: OpRef = OpRef { idx: 30 };
pub const BitShiftRightEq: OpRef = OpRef { idx: 31 };
pub const BitShiftLeftEq: OpRef = OpRef { idx: 32 };

#[derive(Debug, Clone)]
pub struct TypeSystem<T: Clone + Debug, U: Clone + Debug> {
    pub types: Vec<TypeEntry<T, U>>,
    pub types_by_name: HashMap<String, TypeRef>,
    pub conversions: Vec<Vec<TypeRef>>,
    pub op_table: Vec<Operation>,
    pub operations: Vec<Operation>,
    pub operations_by_name: HashMap<String, OpRef>,
}

#[derive(Debug, Clone)]
pub struct TypeEntry<T: Clone + Debug, U: Clone + Debug> {
    pub name: String,
    pub typ: TypeEntryType<T, U>,
    pub fields: HashMap<String, TypeRef>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TypeEntryType<T: Clone + Debug, U: Clone + Debug> {
    BasicType,
    BuiltinFunction(String, TypeRef), // Cannot typecheck yet
    Callable(usize, T),               // Lambda or function, usize is num of args
    UnionType(HashSet<TypeRef>),      // One of the underlying types. Use for type inference
    Class(U),                         // A "type" object if you will
    Object(TypeRef),                  // TypeRef refers to class
    List(TypeRef),                    // Lists so far
    Container(TypeRef),               // Options;
    UninitializedLocation(usize),     // Use solely for typechecking __init__ functions
    PlaceHolder,                      // Used for typechecking functions
}

#[derive(Debug, Clone)]
pub struct Operation {
    name: String,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone)]
pub struct GenericSignature {
    pub inputs: Vec<TypeRef>,
    pub output: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub inputs: Vec<TypeRef>,
    pub output: TypeRef,
}
/*
#[derive(Debug, Clone)]
pub struct OperationConstraint {
    op: OpRef,
    args: Vec<TypeRef>,
}
*/

impl Signature {
    #[allow(dead_code)]
    pub fn match_inputs(&self, other: &Vec<TypeRef>) -> bool {
        if self.inputs.len() != other.len() {
            return false;
        }
        for it in self.inputs.iter().zip(other.iter()) {
            let (t1, t2) = it;
            if t1 != t2 {
                return false;
            }
        }
        true
    }
}

macro_rules! make_ops {
    ($ts: ident, [$($name: expr),+ $(,)?], $( ($($inp: expr),*$(,)? => $output:expr )),* $(,)? ) => {
        let mut opcodes = Vec::new();
        $(
            opcodes.push($ts.add_op($name));
         )+
            for opcode in opcodes {
                $(
                    $ts.operations[opcode.idx].signatures.push(Signature {
                        inputs: vec![$($inp,)*],
                        output: $output,
                    });
                 )*
            }
    };
}

impl<T: Clone + Debug, U: Clone + Debug> TypeSystem<T, U> {
    pub fn new() -> TypeSystem<T, U> {
        let mut rv = TypeSystem {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            conversions: Vec::new(),
            op_table: Vec::new(),
            operations: Vec::new(),
            operations_by_name: HashMap::new(),
        };
        rv.init();
        rv
    }

    fn init(&mut self) {
        self.basic("uninitialized");
        let null = self.basic("null");
        let float = self.basic("float");
        let int = self.basic("int");
        let chr = self.basic("char");
        let bol = self.basic("bool");
        self.add_conversion(int, float);
        self.add_conversion(chr, int);
        self.add_conversion(bol, int);

        make_ops! {self, ["+", "-", "*", "/"],
            (bol, bol => int),
            (chr, chr => chr),
            (int, int => int),
            (float, float => float),
        };
        make_ops! {self, ["==", "!="],
            (null, null => bol),
            (bol, bol => bol),
            (chr, chr => bol),
            (int, int => bol),
            (float, float => bol),
        }
        make_ops! {self, [">=", "<=", ">", "<"],
            (bol, bol => bol),
            (chr, chr => bol),
            (int, int => bol),
            (float, float => bol),
        }
        make_ops! {self, ["%"],
            (chr, int => chr),
            (int, int => int),
        }
        make_ops! {self, ["|", "&", "^"],
            (bol, bol => bol),
            (chr, int => chr),
            (int, int => int),
        }
        make_ops! {self, ["<<", ">>"],
            (chr, int => chr),
            (int, int => int),
        }
        make_ops! {self, ["+", "-", "++", "--"],
            (chr => chr),
            (int => int),
            (float => float),
        };
        make_ops! {self, ["!"],
            (bol => bol),
        };
        make_ops! {self, ["~"],
            (chr => chr),
            (int => int),
        };
        make_ops! {self, ["||", "&&", "^^"],
            (bol, bol => bol),
        };
        make_ops! {self, ["+=", "-=", "*=", "/=", "%="],
            (chr, int => chr),
            (int, int => int),
            (float, float => float),
        };
        make_ops! {self, ["|=", "^=", "&="],
            (bol, bol => bol),
            (chr, int => chr),
            (int, int => int),
        };
        make_ops! {self, [">>=", "<<="],
            (chr, int => chr),
            (int, int => int),
        };
        self.basic("list");
        self.list_name("string", chr);
        self.catch_errors();
    }

    pub fn catch_errors(&self) {
        assert_eq!(self.operations[Plus.idx].name, "+");
        assert_eq!(self.operations[Minus.idx].name, "-");
        assert_eq!(self.operations[Times.idx].name, "*");
        assert_eq!(self.operations[Div.idx].name, "/");
        assert_eq!(self.operations[CmpEq.idx].name, "==");
        assert_eq!(self.operations[CmpNotEq.idx].name, "!=");
        assert_eq!(self.operations[CmpGE.idx].name, ">=");
        assert_eq!(self.operations[CmpLE.idx].name, "<=");
        assert_eq!(self.operations[CmpGT.idx].name, ">");
        assert_eq!(self.operations[CmpLT.idx].name, "<");
        assert_eq!(self.operations[Mod.idx].name, "%");
        assert_eq!(self.operations[BitOr.idx].name, "|");
        assert_eq!(self.operations[BitAnd.idx].name, "&");
        assert_eq!(self.operations[BitXor.idx].name, "^");
        assert_eq!(self.operations[BitShiftLeft.idx].name, "<<");
        assert_eq!(self.operations[BitShiftRight.idx].name, ">>");
        assert_eq!(self.operations[Inc.idx].name, "++");
        assert_eq!(self.operations[Dec.idx].name, "--");
        assert_eq!(self.operations[BoolNot.idx].name, "!");
        assert_eq!(self.operations[BitNot.idx].name, "~");
        assert_eq!(self.operations[BoolOr.idx].name, "||");
        assert_eq!(self.operations[BoolAnd.idx].name, "&&");
        assert_eq!(self.operations[BoolXor.idx].name, "^^");
        assert_eq!(self.operations[PlusEq.idx].name, "+=");
        assert_eq!(self.operations[MinusEq.idx].name, "-=");
        assert_eq!(self.operations[TimesEq.idx].name, "*=");
        assert_eq!(self.operations[DivEq.idx].name, "/=");
        assert_eq!(self.operations[ModEq.idx].name, "%=");
        assert_eq!(self.operations[OrEq.idx].name, "|=");
        assert_eq!(self.operations[XorEq.idx].name, "^=");
        assert_eq!(self.operations[AndEq.idx].name, "&=");
        assert_eq!(self.operations[BitShiftRightEq.idx].name, ">>=");
        assert_eq!(self.operations[BitShiftLeftEq.idx].name, "<<=");
    }

    #[allow(dead_code)]
    pub fn typename(&self, typ: TypeRef) -> String {
        self.types[typ.idx].name.clone()
    }

    #[allow(dead_code)]
    pub fn opname(&self, op: OpRef) -> String {
        self.operations[op.idx].name.clone()
    }

    #[allow(dead_code)]
    pub fn print_ops(&self) {
        let mut i = 0;
        for op in self.operations.iter() {
            print!("{}: `{}`: [", i, op.name);
            i += 1;
            for sig in op.signatures.iter() {
                print!("\n    ");
                self.print_sig(sig);
            }
            println!("\n]");
        }
    }

    #[allow(dead_code)]
    pub fn print_op(&self, op: OpRef) {
        let op = &self.operations[op.idx];
        print!("`{}`: [", op.name);
        for sig in op.signatures.iter() {
            print!("\n    ");
            self.print_sig(sig);
        }
        println!("\n]");
    }

    #[allow(dead_code)]
    pub fn print_types(&self) {
        let mut i = 0;
        for op in self.types.iter() {
            println!("{}: {}", i, op.name);
            i += 1;
        }
    }

    pub fn print_sig(&self, sig: &Signature) {
        let mut first = true;
        print!("(");
        for typ in sig.inputs.iter() {
            if !first {
                print!(", ");
            }
            self.print_type(*typ);
            first = false;
        }
        print!(" => ");
        self.print_type(sig.output);
        print!(")");
    }

    pub fn print_type(&self, typ: TypeRef) {
        print!("{}", self.typename(typ))
    }

    pub fn entry(&mut self, name: &str, typ: TypeEntryType<T, U>) -> TypeRef {
        self.types_by_name
            .get(name)
            .map(|x| *x)
            .unwrap_or_else(|| self.new_entry(name, typ))
    }

    pub fn new_entry(&mut self, name: &str, typ: TypeEntryType<T, U>) -> TypeRef {
        let val = TypeEntry {
            name: name.into(),
            typ,
            fields: HashMap::new(),
        };

        let rv = TypeRef {
            idx: self.types.len(),
        };
        self.types_by_name.insert(val.name.clone(), rv);
        self.types.push(val);
        self.conversions.push(Vec::new());
        make_ops! {self, ["==","!="],
            (rv, Null => Bool),
        }
        rv
    }

    #[allow(dead_code)]
    pub fn replace(&mut self, idx: TypeRef, val: TypeEntry<T, U>) {
        self.types[idx.idx] = val;
    }

    pub fn basic(&mut self, name: &str) -> TypeRef {
        self.new_entry(name, TypeEntryType::BasicType)
    }

    #[allow(dead_code)]
    pub fn placeholder(&mut self, name: &str) -> TypeRef {
        self.new_entry(name, TypeEntryType::PlaceHolder)
    }

    pub fn builtinfunction(&mut self, name: &str, rt: TypeRef) -> TypeRef {
        let nname = format!("builtin_function<'{}'>", name);
        self.new_entry(&nname, TypeEntryType::BuiltinFunction(name.into(), rt))
    }

    pub fn list(&mut self, typ: TypeRef) -> TypeRef {
        self.list_name(&format!("list<{}>", self.types[typ.idx].name), typ)
    }

    pub fn list_name(&mut self, name: &str, typ: TypeRef) -> TypeRef {
        let rv = self.entry(name.into(), TypeEntryType::List(typ));
        make_ops! {self, ["+", "+="],
            (rv, rv => rv),
            (EmptyList, rv => rv),
            (rv, EmptyList => rv),
        }
        if self.apply_operation(CmpEq, vec![typ, typ]).is_some() {
            make_ops! {self, ["==", "!="], (rv, rv => Bool), }
            if self.apply_operation(CmpGT, vec![typ, typ]).is_some() {
                make_ops! {self, [">=", "<=", ">", "<"], (rv, rv => Bool) }
            }
        }
        let bifname = format!("{}.__index__", name);
        let bif = self.builtinfunction(&bifname, typ);
        self.types[rv.idx].fields.insert("__index__".into(), bif);
        let bifname = format!("{}.__iter__", name);
        let bif = self.builtinfunction(&bifname, typ);
        self.types[rv.idx].fields.insert("__iter__".into(), bif);
        self.add_conversion(EmptyList, rv);
        rv
    }

    pub fn function(&mut self, name: &str, num_args: usize, extra: T) -> TypeRef {
        self.new_entry(name, TypeEntryType::Callable(num_args, extra))
    }

    pub fn optional(&mut self, typ: TypeRef) -> TypeRef {
        self.optional_name(&format!("optional<{}>", self.types[typ.idx].name), typ)
    }

    pub fn optional_name(&mut self, name: &str, typ: TypeRef) -> TypeRef {
        let rv = self.new_entry(name.into(), TypeEntryType::Container(typ));
        make_ops! {self, ["*"], (rv => typ), }
        make_ops! {self, ["==", "!="],
            (rv, Null => Bool),
        }
        rv
    }

    pub fn class(
        &mut self,
        name: &str,
        fields: HashSet<String>,
        methods: HashMap<String, TypeRef>,
        extra: U,
    ) -> TypeRef {
        // we Just add the methods and fields to the name so we can extend funcitonality of predefinded
        let entry = self.entry(name, TypeEntryType::Class(extra));
        for (name, val) in methods {
            self.types[entry.idx].fields.insert(name, val);
        }
        for name in fields {
            self.types[entry.idx].fields.insert(name, Undefined);
        }
        entry
    }

    pub fn union(&mut self, types: HashSet<TypeRef>) -> TypeRef {
        let mut names = Vec::new();
        if types.len() == 1 {
            for typ in types.iter() {
                return *typ;
            }
        }
        if types.len() == 1 && types.contains(&Null) {
            for typ in types.iter() {
                if *typ != Null {
                    return self.optional(*typ);
                }
            }
        }
        for typ in types.iter() {
            names.push(self.types[typ.idx].name.clone());
        }
        let name = format!("union<{}>", names.join("|"));
        self.entry(&name, TypeEntryType::UnionType(types))
    }

    pub fn class_instance(&mut self, class: TypeRef) -> TypeRef {
        let name = format!("<class `{}`>", self.types[class.idx].name);
        let rv = self.new_entry(&name, TypeEntryType::Object(class));
        self.types[rv.idx].fields = self.types[class.idx].fields.clone();
        rv
    }

    pub fn add_conversion(&mut self, from: TypeRef, to: TypeRef) {
        self.conversions[from.idx].push(to);
    }

    pub fn get_conversions(&self, val: &TypeRef) -> HashSet<TypeRef> {
        let mut rv = HashSet::new();
        self.get_conversions_int(val, &mut rv);
        rv
    }

    pub fn get_conversions_int(&self, val: &TypeRef, set: &mut HashSet<TypeRef>) {
        if !set.insert(*val) {
            return;
        }
        for u in self.conversions[val.idx].iter() {
            self.get_conversions_int(u, set);
        }
    }

    pub fn convertable_to(&self, from: &TypeRef, to: &TypeRef) -> bool {
        match &self.types[from.idx].typ {
            TypeEntryType::PlaceHolder => return true,
            TypeEntryType::UnionType(types) => {
                for typ in types.iter() {
                    if self.convertable_to(typ, to) {
                        return true;
                    }
                }
                return false;
            }
            _ => (),
        }
        self.get_conversions(from).contains(&to)
    }

    pub fn is_assignable(&mut self, lhs: &TypeRef, rhs: &TypeRef) -> bool {
        use TypeEntryType::*;
        if rhs == lhs {
            return true;
        }
        if self.convertable_to(rhs, lhs) {
            return true;
        }
        match (
            self.types[lhs.idx].typ.clone(),
            &mut self.types[rhs.idx].typ,
        ) {
            (Container(t), Container(l)) => {
                let l = *l;
                self.is_assignable(&t, &l)
            }
            _ => false,
        }
    }

    pub fn add_op(&mut self, name: &str) -> OpRef {
        if let Some(val) = self.operations_by_name.get(name) {
            return *val;
        }
        let rv = OpRef {
            idx: self.operations.len(),
        };
        self.operations.push(Operation {
            name: name.into(),
            signatures: Vec::new(),
        });
        self.operations_by_name.insert(name.into(), rv);
        rv
    }

    pub fn apply_operation(&mut self, op: OpRef, inputs: Vec<TypeRef>) -> Option<TypeRef> {
        for input in inputs.iter() {
            match self.types[input.idx].typ {
                TypeEntryType::PlaceHolder => return self.apply_operation_place(op, inputs),
                TypeEntryType::UnionType(_) => return self.apply_operation_place(op, inputs),
                _ => (),
            }
        }
        self.apply_operation_noplace(op, inputs)
    }

    pub fn apply_operation_place(&mut self, op: OpRef, inputs: Vec<TypeRef>) -> Option<TypeRef> {
        let operation = &self.operations[op.idx];
        let mut types: HashSet<TypeRef> = HashSet::new();
        for sig in operation.signatures.iter() {
            if inputs.len() != sig.inputs.len() {
                continue;
            }
            let mut matches = true;
            for (input, siginput) in inputs.iter().zip(sig.inputs.iter()) {
                matches = true;
                if !self.convertable_to(input, siginput) {
                    matches = false;
                }
                match &self.types[input.idx].typ {
                    TypeEntryType::PlaceHolder => {
                        matches = true;
                    }
                    TypeEntryType::UnionType(types) => {
                        for typ in types.iter() {
                            if self.convertable_to(typ, input) {
                                matches = true;
                                break;
                            }
                        }
                    }
                    _ => (),
                }
            }
            if matches {
                types.insert(sig.output);
                break;
            }
        }
        Some(self.union(types))
    }

    pub fn apply_operation_noplace(&mut self, op: OpRef, inputs: Vec<TypeRef>) -> Option<TypeRef> {
        let operation = &self.operations[op.idx];

        for sig in operation.signatures.iter() {
            if inputs.len() != sig.inputs.len() {
                continue;
            }
            let mut matches = true;
            for ipts in inputs.iter().zip(sig.inputs.iter()) {
                let (ip, sg) = ipts;
                if *ip != *sg && !self.convertable_to(ip, sg) {
                    matches = false;
                    break;
                }
            }
            if matches {
                return Some(sig.output);
            }
        }

        let all_list: Option<Vec<TypeRef>> = inputs
            .iter()
            .map(|s| match self.types[s.idx].typ {
                TypeEntryType::List(t) => Some(t),
                TypeEntryType::Container(t) => Some(t),
                _ => None,
            })
            .collect();
        match all_list {
            Some(_) => None, //self.apply_operation(op, v),
            None => None,
        }
    }
}
