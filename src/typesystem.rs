#![allow(dead_code)]
#![allow(non_upper_case_globals)]
use std::collections::HashMap;
use std::collections::HashSet;

pub type TypeRef = usize;
pub type OpRef = usize;

pub const Uninitialized: TypeRef = 0;
pub const Null: TypeRef = 1;
pub const Float: TypeRef = 2;
pub const Int: TypeRef = 3;
pub const Char: TypeRef = 4;
pub const Bool: TypeRef = 5;
pub const Str: TypeRef = 6;

pub const Plus: OpRef = 0;
pub const Minus: OpRef = 1;
pub const Times: OpRef = 2;
pub const Div: OpRef = 3;
pub const CmpEq: OpRef = 4;
pub const CmpNotEq: OpRef = 5;
pub const CmpGE: OpRef = 6;
pub const CmpLE: OpRef = 7;
pub const CmpGT: OpRef = 8;
pub const CmpLT: OpRef = 9;
pub const Mod: OpRef = 10;
pub const BitOr: OpRef = 11;
pub const BitAnd: OpRef = 12;
pub const BitXor: OpRef = 13;
pub const BitShiftLeft: OpRef = 14;
pub const BitShiftRight: OpRef = 15;
pub const Inc: OpRef = 16;
pub const Dec: OpRef = 17;
pub const BoolNot: OpRef = 18;
pub const BitNot: OpRef = 19;
pub const BoolOr: OpRef = 20;
pub const BoolAnd: OpRef = 21;
pub const BoolXor: OpRef = 22;
pub const OrEq: OpRef = 23;
pub const XorEq: OpRef = 24;
pub const AndEq: OpRef = 25;
pub const PlusEq: OpRef = 26;
pub const MinusEq: OpRef = 27;
pub const TimesEq: OpRef = 28;
pub const DivEq: OpRef = 29;
pub const ModEq: OpRef = 30;
pub const BitShiftRightEq: OpRef = 31;
pub const BitShiftLeftEq: OpRef = 32;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    pub types: Vec<TypeEntry>,
    pub types_by_name: HashMap<String, TypeRef>,
    pub conversions: Vec<Vec<TypeRef>>,
    pub op_table: Vec<Operation>,
    pub operations: Vec<Operation>,
    pub operations_by_name: HashMap<String, OpRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEntry {
    pub name: String,
    pub typ: TypeEntryType,
    pub fields: HashMap<String, TypeRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeEntryType {
    BasicType,
    BuiltinFunction(String, TypeRef),    // Cannot typecheck yet
    CallableType(Signature, usize),      // Lambda or function, usize is num of args
    UnionType(Vec<TypeRef>),             // One of the underlying types
    ClassType(HashMap<String, TypeRef>), // Class or tuple
    Iterable(TypeRef),                   // Lists
    Optional(TypeRef),                   // Option types
    PlaceHolderType,                     // Used for typechecking functions
    GenericType(Vec<Constraint>),        // Used for typechecking functions
}

#[derive(Debug, Clone)]
pub struct Operation {
    name: String,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericSignature {
    pub inputs: Vec<TypeRef>,
    pub output: TypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub inputs: Vec<TypeRef>,
    pub output: TypeRef,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Operation(OperationConstraint),
    Type(TypeRef),
    Arg(usize),
    Callable(usize, TypeRef),
    Indexable(usize, TypeRef),
}

#[derive(Debug, Clone, PartialEq)]
pub struct OperationConstraint {
    op: OpRef,
    args: Vec<TypeRef>,
}

impl Signature {
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
                    $ts.operations[opcode].signatures.push(Signature {
                        inputs: vec![$($inp,)*],
                        output: $output,
                    });
                 )*
            }
    };
}

impl TypeSystem {
    pub fn new() -> TypeSystem {
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
        self.basic("null");
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
        make_ops! {self, ["==", "!=", ">=", "<=", ">", "<"],
            (bol, bol => bol),
            (chr, chr => bol),
            (int, int => bol),
            (float, float => bol),
        }
        make_ops! {self, ["%", "|", "&", "^", "<<", ">>"],
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
        make_ops! {self, ["+=", "-=", "/=", "%="],
            (chr, int => chr),
            (int, int => int),
            (float, float => float),
        };
        make_ops! {self, ["|=", "^=", "&=", ">>=", "<<="],
            (chr, int => chr),
            (int, int => int),
        };
        self.list_name("string", chr);
    }

    pub fn typename(&self, typ: TypeRef) -> String {
        self.types[typ].name.clone()
    }

    pub fn opname(&self, op: OpRef) -> String {
        self.operations[op].name.clone()
    }

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

    pub fn print_op(&self, op: OpRef) {
        let op = &self.operations[op];
        print!("`{}`: [", op.name);
        for sig in op.signatures.iter() {
            print!("\n    ");
            self.print_sig(sig);
        }
        println!("\n]");
    }

    pub fn print_types(&self) {
        let mut i = 0;
        for op in self.types.iter() {
            println!("{}: {:#?}", i, op);
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

    pub fn entry(&mut self, name: &str, typ: TypeEntryType) -> Result<TypeRef, String> {
        if let Some(val) = self.types_by_name.get(name) {
            if self.types[*val].typ == typ {
                return Ok(*val);
            } else {
                return Err(format!("Cannot redefine type `{}`", name));
            }
        }
        Ok(self.new_entry(name, typ))
    }

    pub fn new_entry(&mut self, name: &str, typ: TypeEntryType) -> TypeRef {
        let val = TypeEntry {
            name: name.into(),
            typ,
            fields: HashMap::new(),
        };

        let rv = self.types.len();
        self.types_by_name.insert(val.name.clone(), rv);
        self.types.push(val);
        self.conversions.push(Vec::new());
        rv
    }

    pub fn replace(&mut self, idx: TypeRef, val: TypeEntry) {
        self.types[idx] = val;
    }

    pub fn basic(&mut self, name: &str) -> TypeRef {
        self.new_entry(name, TypeEntryType::BasicType)
    }

    pub fn placeholder(&mut self, name: &str) -> TypeRef {
        self.new_entry(name, TypeEntryType::PlaceHolderType)
    }

    pub fn generic(&mut self, name: &str) -> TypeRef {
        let name = format!("generic<{}>", name);
        self.generic_con(&name, Vec::new())
    }

    pub fn generic_con(&mut self, name: &str, con: Vec<Constraint>) -> TypeRef {
        self.new_entry(name, TypeEntryType::GenericType(con))
    }

    pub fn builtinfunction(&mut self, name: &str, rt: TypeRef) -> TypeRef {
        let nname = format!("builtin_function<'{}'>", name);
        self.new_entry(&nname, TypeEntryType::BuiltinFunction(name.into(), rt))
    }

    pub fn list(&mut self, typ: TypeRef) -> TypeRef {
        self.list_name(&format!("iterable<{}>", self.types[typ].name), typ)
    }

    pub fn list_name(&mut self, name: &str, typ: TypeRef) -> TypeRef {
        let rv = self
            .entry(name.into(), TypeEntryType::Iterable(typ))
            .unwrap();
        make_ops! {self, ["+", "+="], (rv, rv => rv), }
        if self.apply_operation_no_gen(CmpEq, vec![typ, typ]).is_some() {
            make_ops! {self, ["==", "!="], (rv, rv => Bool), }
            if self.apply_operation_no_gen(CmpGT, vec![typ, typ]).is_some() {
                make_ops! {self, [">=", "<=", ">", "<"], (rv, rv => Bool) }
            }
        }
        let bifname = format!("{}.__index__", name);
        let bif = self.builtinfunction(&bifname, typ);
        self.types[rv].fields.insert("__index__".into(), bif);
        rv
    }

    pub fn optional(&mut self, typ: TypeRef) -> TypeRef {
        self.optional_name(&format!("optional<{}>", self.types[typ].name), typ)
    }

    pub fn optional_name(&mut self, name: &str, typ: TypeRef) -> TypeRef {
        let rv = self.new_entry(name.into(), TypeEntryType::Optional(typ));
        make_ops! {self, ["*"], (rv => typ), }
        make_ops! {self, ["==", "!="],
            (rv, Null => Bool),
        }
        rv
    }

    pub fn class(&mut self, name: &str, fields: HashMap<String, TypeRef>) -> TypeRef {
        self.new_entry(&name, TypeEntryType::ClassType(fields))
    }

    pub fn add_conversion(&mut self, from: TypeRef, to: TypeRef) {
        self.conversions[from].push(to);
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
        for u in self.conversions[*val].iter() {
            self.get_conversions_int(u, set);
        }
    }

    pub fn convertable_to(&self, from: &TypeRef, to: &TypeRef) -> bool {
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
        match (self.types[*lhs].typ.clone(), &mut self.types[*rhs].typ) {
            (Iterable(t), Iterable(l)) => {
                let l = *l;
                self.is_assignable(&t, &l)
            }
            (_, GenericType(cons)) => {
                cons.push(Constraint::Type(*lhs));
                true
            }
            _ => false,
        }
    }

    pub fn add_op(&mut self, name: &str) -> OpRef {
        if let Some(val) = self.operations_by_name.get(name) {
            return *val;
        }
        let rv = self.operations.len();
        self.operations.push(Operation {
            name: name.into(),
            signatures: Vec::new(),
        });
        self.operations_by_name.insert(name.into(), rv);
        rv
    }

    pub fn constrain_operation(&mut self, op: OpRef, inputs: Vec<TypeRef>) -> TypeRef {
        let typenames: Vec<String> = inputs.iter().map(|x| self.types[*x].name.clone()).collect();
        self.generic_con(
            &format!(
                "<constrained `{}` ({})>",
                self.operations[op].name,
                typenames.join(", ")
            ),
            vec![Constraint::Operation(OperationConstraint {
                op,
                args: inputs.clone(),
            })],
        )
    }

    pub fn constrain_callable(&mut self, typ: TypeRef, num_args: usize) -> TypeRef {
        let output = self.generic("<callable output>");
        match &mut self.types[typ].typ {
            TypeEntryType::GenericType(c) => c.push(Constraint::Callable(num_args, output)),
            _ => (),
        }
        output
    }

    pub fn constrain_idx(&mut self, typ: TypeRef, num_args: usize) -> TypeRef {
        let output = self.generic("<index output>");
        match &mut self.types[typ].typ {
            TypeEntryType::GenericType(c) => c.push(Constraint::Indexable(num_args, output)),
            _ => (),
        }
        output
    }

    pub fn apply_operation(&mut self, op: OpRef, inputs: Vec<TypeRef>) -> Option<TypeRef> {
        for ip in inputs.iter() {
            match self.types[*ip].typ {
                TypeEntryType::GenericType(_) => return Some(self.constrain_operation(op, inputs)),
                _ => (),
            }
        }

        self.apply_operation_no_gen(op, inputs)
    }

    pub fn apply_operation_no_gen(&self, op: OpRef, inputs: Vec<TypeRef>) -> Option<TypeRef> {
        let operation = self.operations.get(op).unwrap();
        for sig in operation.signatures.iter() {
            if inputs.len() != sig.inputs.len() {
                continue;
            }
            let mut matches = true;
            for ipts in inputs.iter().zip(sig.inputs.iter()) {
                let (ip, sg) = ipts;
                if !self.convertable_to(ip, sg) {
                    matches = false;
                    break;
                }
            }
            if matches {
                return Some(sig.output);
            }
        }
        None
    }

    pub fn check_constraints(
        &self,
        typ: &TypeRef,
        function_args: &Vec<TypeRef>,
    ) -> Option<TypeRef> {
        let (typ, cons) = match &self.types[*typ].typ {
            TypeEntryType::GenericType(c) => (*typ, c),
            _ => return Some(*typ),
        };

        let mut pros_typ = Uninitialized;
        for con in cons.iter() {
            let new_type = match con {
                Constraint::Operation(c) => {
                    let mut args = Vec::new();
                    for arg in c.args.iter() {
                        let t = self.check_constraints(arg, function_args)?;
                        args.push(t)
                    }
                    self.apply_operation_no_gen(c.op, args)
                }
                Constraint::Type(t) => {
                    if !self.convertable_to(&typ, t) {
                        return None;
                    }
                    Some(typ)
                }
                Constraint::Arg(idx) => Some(function_args[*idx]),
                Constraint::Callable(num_args, output) => {
                    if *num_args != function_args.len() {
                        return None;
                    }
                    Some(*output)
                }
                Constraint::Indexable(num_args, output) => {
                    if *num_args != function_args.len() {
                        return None;
                    }
                    Some(*output)
                }
            };
            let new_type = new_type?;
            if pros_typ == Uninitialized || pros_typ == new_type {
                pros_typ = new_type;
                continue;
            } else if self.convertable_to(&pros_typ, &new_type) {
                pros_typ = new_type;
                continue;
            } else if self.convertable_to(&new_type, &pros_typ) {
                continue;
            }
        }
        Some(pros_typ)
    }
}
