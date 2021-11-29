#![allow(dead_code)]
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug, Clone)]
pub struct TypeSystem {
    types: Vec<TypeEntry>,
    types_by_name: HashMap<String, usize>,
    conversions: Vec<Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEntry {
    name: String,
    typ: TypeEntryType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeEntryType {
    BasicType,
    UnionType(Vec<usize>),  // One of the underlying types
    StructType(Vec<usize>), // Struct or tuple
    ListType(usize),        // Lists
}

impl TypeSystem {
    pub fn new() -> TypeSystem {
        let mut rv = TypeSystem {
            types: Vec::new(),
            types_by_name: HashMap::new(),
            conversions: Vec::new(),
        };
        let flt = rv.basic("float".into());
        let int = rv.basic("int".into());
        let chr = rv.basic("char".into());
        let bol = rv.basic("bool".into());
        rv.add_conversion(int, flt);
        rv.add_conversion(chr, int);
        rv.add_conversion(bol, chr);
        rv
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

    pub fn basic(&mut self, name: String) -> usize {
        self.new_entry(TypeEntry {
            name,
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

    pub fn get_conversions(&self, val: usize) -> HashSet<usize> {
        let mut visited: Vec<bool> = self.types.iter().map(|_| false).collect();
        self.get_conversions_int(val, &mut visited)
    }

    pub fn get_conversions_int(&self, val: usize, visited: &mut Vec<bool>) -> HashSet<usize> {
        if visited[val] {
            return HashSet::new();
        }
        visited[val] = true;
        let mut conversions = HashSet::new();
        for u in self.conversions[val].iter() {
            let hs: HashSet<usize> = self.get_conversions_int(*u, visited);
            conversions = conversions.union(&hs).map(|x| *x).collect();
        }
        conversions
    }
}
