#![allow(dead_code)]
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    pub types: Vec<TypeEntry>,
    pub types_by_name: HashMap<String, usize>,
    pub conversions: Vec<Vec<usize>>,
    pub op_table: Vec<Operation>,
    pub operations: Vec<Operation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEntry {
    name: String,
    typ: TypeEntryType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeEntryType {
    BasicType,
    CallableType(Vec<usize>, usize), // Lambda or function
    UnionType(Vec<usize>),           // One of the underlying types
    StructType(Vec<usize>),          // Struct or tuple
    ListType(usize),                 // Lists
}

#[derive(Debug, Clone)]
pub struct Operation {
    name: String,
    signatures: Vec<Signature>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    inputs: Vec<usize>,
    output: usize,
}

macro_rules! make_operation {
    ($ts: ident, $name: expr,  $(($($inp: expr,)* => $output:expr ))* ) => {
        let opcode = $ts.add_op($name);
        $(
            $ts.operations[opcode].signatures.push(Signature {
               inputs: vec![$(*$ts.types_by_name.get($inp).unwrap(),)*],
               output: *$ts.types_by_name.get($output).unwrap(),
            });
        )*
    };
}

impl TypeSystem {
    pub fn new() -> TypeSystem {
        TypeSystem {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            conversions: Vec::new(),
            op_table: Vec::new(),
            operations: Vec::new(),
        }
    }

    pub fn new_entry(&mut self, val: TypeEntry) -> usize {
        if let Some(val) = self.types_by_name.get(&val.name) {
            return *val;
        }
        let rv = self.types.len();
        self.types_by_name.insert(val.name.clone(), rv);
        self.types.push(val);
        self.conversions.push(Vec::new());
        rv
    }

    pub fn basic(&mut self, name: &str) -> usize {
        self.new_entry(TypeEntry {
            name: name.into(),
            typ: TypeEntryType::BasicType,
        })
    }

    pub fn list(&mut self, typ: usize) -> usize {
        let name = format!("list<{}>", self.types[typ].name);
        self.new_entry(TypeEntry {
            name,
            typ: TypeEntryType::ListType(typ),
        })
    }

    pub fn add_conversion(&mut self, from: usize, to: usize) {
        self.conversions[from].push(to);
    }

    pub fn get_conversions(&self, val: &usize) -> HashSet<usize> {
        let mut visited: Vec<bool> = self.types.iter().map(|_| false).collect();
        self.get_conversions_int(val, &mut visited)
    }

    pub fn get_conversions_int(&self, val: &usize, visited: &mut Vec<bool>) -> HashSet<usize> {
        if visited[*val] {
            return HashSet::new();
        }
        visited[*val] = true;
        let mut conversions = HashSet::new();
        for u in self.conversions[*val].iter() {
            let hs: HashSet<usize> = self.get_conversions_int(u, visited);
            conversions = conversions.union(&hs).map(|x| *x).collect();
        }
        conversions
    }

    pub fn convertable_to(&self, from: &usize, to: &usize) -> bool {
        self.get_conversions(from).contains(&to)
    }

    pub fn add_op(&mut self, name: &str) -> usize {
        let rv = self.operations.len();
        self.operations.push(Operation {
            name: name.into(),
            signatures: Vec::new(),
        });
        rv
    }

    pub fn apply_operation(&self, op: usize, inputs: Vec<usize>) -> Option<usize> {
        let operation = self.operations.get(op).unwrap();
        for sig in operation.signatures.iter() {
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

    pub fn test_make_op(&mut self) {
        make_operation! {self, "+",
            ("int", "int", => "int")
            ("float", "float", => "int")
        };
        make_operation! {self, "-",
            ("int", "int", => "int")
            ("float", "float", => "int")
        };
        make_operation! {self, "*",
            ("int", "int", => "int")
            ("float", "float", => "int")
        };
    }
}
