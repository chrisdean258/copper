use crate::builtins::*;
use crate::parser::*;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Display, Formatter};
use std::mem::swap;
use std::rc::{Rc, Weak};

type ScopeTable = Vec<Rc<RefCell<HashMap<String, usize>>>>;

#[derive(Clone, Debug)]
pub struct Object {
    pub value: Value,
    pub fields: HashMap<String, usize>,
}

#[allow(dead_code)]
#[derive(Clone)]
pub enum Value {
    BuiltinFunc(&'static str, fn(&mut Evaluator, Vec<Object>) -> Object),
    Reference(usize),
    Str(String),
    List(Vec<Object>),
    Int(i64),
    Float(f64),
    Char(char),
    Bool(u8),
    Function(Function, Vec<Object>, ScopeTable),
    Lambda(Lambda, ScopeTable),
    Class(Rc<ClassDecl>, ScopeTable),
    Object(Rc<ClassDecl>),
    Undefined,
    Null,
}

#[derive(Clone, Debug)]
pub struct Evaluator {
    scopes: ScopeTable,
    pub memory: Vec<Object>,
    all_scopes: Vec<Weak<RefCell<HashMap<String, usize>>>>,
    free: BTreeSet<usize>,
    alloc_count: usize,
    safe: usize,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        f.write_str("Value::")?;
        match self {
            BuiltinFunc(name, _) => f.write_fmt(format_args!("BuiltinFunc({})", name)),
            Str(s) => f.write_fmt(format_args!("Str({:?})", s)),
            List(l) => {
                f.write_str("List([")?;
                for expr in l.iter() {
                    f.write_fmt(format_args!("{:?}, ", expr))?;
                }
                f.write_str("])")
            }
            Int(i) => f.write_fmt(format_args!("Int({:?})", i)),
            Float(d) => f.write_fmt(format_args!("Float({:?})", d)),
            Char(c) => f.write_fmt(format_args!("Char({:?})", c)),
            Reference(u) => f.write_fmt(format_args!("Reference({:x})", u)),
            Bool(b) => f.write_fmt(format_args!("Bool({})", b)),
            Function(func, _, _) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", "),))
            }
            Lambda(lambda, _) => f.write_fmt(format_args!("lambda(args={})", lambda.num_args)),
            Class(cd, _) => f.write_fmt(format_args!("<type `class {}`>", cd.name)),
            Object(cls) => f.write_fmt(format_args!("<class {}>", cls.name)),
            Null => f.write_str("Null"),
            Undefined => f.write_str("Undefined"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        match self {
            BuiltinFunc(name, _) => f.write_fmt(format_args!("BuiltinFunc({})", name)),
            Str(s) => f.write_fmt(format_args!("{}", s)),
            Int(i) => f.write_fmt(format_args!("{}", i)),
            Float(d) => f.write_fmt(format_args!("{}", d)),
            Char(c) => f.write_fmt(format_args!("{}", c)),
            Reference(u) => f.write_fmt(format_args!("Reference(0x{:x})", u)),
            Bool(b) => f.write_fmt(format_args!("{}", if *b != 0 { "true" } else { "false" })),
            List(l) => {
                f.write_str("[")?;
                for expr in l.iter() {
                    f.write_fmt(format_args!("{}, ", expr.value))?;
                }
                f.write_str("]")
            }
            Function(func, _, _) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", ")))
            }
            Lambda(lambda, _) => f.write_fmt(format_args!("lambda(args={})", lambda.num_args)),
            Class(cd, _) => f.write_fmt(format_args!("<type `class {}`>", cd.name)),
            Object(cls) => f.write_fmt(format_args!("<class {}>", cls.name)),
            Null => f.write_str("null"),
            Undefined => f.write_str("undefined"),
        }
    }
}

impl Evaluator {
    pub fn new() -> Evaluator {
        let mut eval = Evaluator {
            scopes: Vec::new(),
            memory: Vec::new(),
            all_scopes: Vec::new(),
            free: BTreeSet::new(),
            alloc_count: 0,
            safe: 0,
        };

        let mut builtins = HashMap::new();

        let idx = eval.alloc_builtin_function("print", copper_print);
        builtins.insert(String::from("print"), idx);
        let idx = eval.alloc_builtin_function("prints", copper_print_no_newline);
        builtins.insert(String::from("prints"), idx);
        let idx = eval.alloc_builtin_function("getline", copper_getline);
        builtins.insert(String::from("getline"), idx);
        let idx = eval.alloc_builtin_function("len", copper_len);
        builtins.insert(String::from("len"), idx);

        let scopes = Rc::new(RefCell::new(builtins));
        eval.all_scopes.push(Rc::downgrade(&scopes));

        eval.scopes.push(scopes);

        eval
    }

    pub fn object_fields(&self, value: Value, fields: HashMap<String, usize>) -> Object {
        Object { value, fields }
    }

    pub fn object(&self, value: Value) -> Object {
        self.object_fields(value, HashMap::new())
    }

    pub fn reference(&self, idx: usize) -> Object {
        self.object(Value::Reference(idx))
    }

    pub fn undefined(&self) -> Object {
        self.object(Value::Undefined)
    }

    pub fn null(&self) -> Object {
        self.object(Value::Null)
    }

    pub fn alloc_builtin_function(
        &mut self,
        name: &'static str,
        func: fn(&mut Evaluator, Vec<Object>) -> Object,
    ) -> usize {
        let rv = self.object(Value::BuiltinFunc(name, func));
        let idx = self.alloc(rv);
        self.memory[idx].fields.insert("__call__".into(), idx);
        idx
    }

    pub fn class(&self, cd: &ClassDecl, scope: ScopeTable) -> Object {
        self.object(Value::Class(Rc::new(cd.clone()), scope))
    }

    pub fn string(&self, s: String) -> Object {
        self.object(Value::Str(s))
    }

    pub fn list(&self, l: Vec<Object>) -> Object {
        self.object(Value::List(l))
    }

    pub fn int(&self, i: i64) -> Object {
        self.object(Value::Int(i))
    }

    pub fn float(&self, f: f64) -> Object {
        self.object(Value::Float(f))
    }

    pub fn bool(&self, b: u8) -> Object {
        self.object(Value::Bool(b))
    }

    pub fn char(&self, c: char) -> Object {
        self.object(Value::Char(c))
    }

    pub fn alloc_function(&mut self, f: &Function) -> usize {
        self.alloc_function_args(f, Vec::new(), self.scopes.clone())
    }

    pub fn alloc_function_args(
        &mut self,
        f: &Function,
        args: Vec<Object>,
        scopes: ScopeTable,
    ) -> usize {
        let val = Value::Function(f.clone(), args, scopes);
        let idx = self.alloc(self.object(val));
        self.memory[idx].fields.insert("__call__".into(), idx);
        idx
    }

    pub fn alloc_lambda(&mut self, l: &Lambda) -> usize {
        let val = Value::Lambda(l.clone(), self.scopes.clone());
        let idx = self.alloc(self.object(val));
        self.memory[idx].fields.insert("__call__".into(), idx);
        idx
    }

    pub fn class_instance(&self, cls: &Rc<ClassDecl>) -> Object {
        self.object(Value::Object(cls.clone()))
    }

    pub fn alloc(&mut self, val: Object) -> usize {
        if self.alloc_count == 100000 {
            self.clean_memory();
            self.alloc_count = 0;
        }
        self.alloc_count += 1;
        if self.free.len() > 1 {
            //this is a hack because pop_first is not available
            for idx in self.free.iter() {
                let idx = *idx;
                self.memory[idx] = val;
                self.free.remove(&idx);
                return idx;
            }
            unreachable!()
        } else {
            self.memory.push(val);
            self.memory.len() - 1
        }
    }

    fn clean_memory(&mut self) {
        let mut free = BTreeSet::new();
        for i in 0..self.memory.len() {
            free.insert(i);
        }
        self.save_memory(self.safe, &mut free);
        for posscope in self.all_scopes.clone().iter() {
            if let Some(scope) = posscope.upgrade() {
                for val in scope.borrow().values() {
                    self.save_memory(*val, &mut free);
                }
            }
        }
        self.free = free;
    }

    fn save_all<TT: Iterator<Item = Object>>(&self, items: TT, to_save: &mut BTreeSet<usize>) {
        for item in items {
            if let Value::Reference(u) = item.value {
                to_save.insert(u);
            }
        }
    }

    fn save_memory(&mut self, idx: usize, free: &mut BTreeSet<usize>) -> bool {
        let mut to_save: BTreeSet<usize> = BTreeSet::new();
        to_save.insert(idx);
        while to_save.len() > 0 {
            let mut idx = *to_save.iter().next().unwrap();
            to_save.remove(&mut idx);
            // this prevents double processing
            if !free.remove(&idx) {
                continue;
            }
            match self.memory[idx].value.clone() {
                Value::List(l) => {
                    self.save_all(l.into_iter(), &mut to_save);
                }
                Value::Reference(u) => {
                    to_save.insert(u);
                }
                // left here for posterity
                // Value::Function(_, objs, scopes) => {
                // self.save_all(objs.into_iter(), &mut to_save);
                // for scope in scopes {
                // for u in scope.borrow().values() {
                // to_save.insert(*u);
                // }
                // }
                // }
                // Value::Lambda(_, scopes) => {
                // for scope in scopes {
                // for u in scope.borrow().values() {
                // to_save.insert(*u);
                // }
                // }
                // }
                // Value::Class(_, scopes) => {
                // for scope in scopes {
                // for u in scope.borrow().values() {
                // to_save.insert(*u);
                // }
                // }
                // }
                // Object(Rc<ClassDecl>),
                _ => (),
            }
        }
        true
    }

    fn openscope(&mut self) {
        let new_scopes = Rc::new(RefCell::new(HashMap::new()));
        self.all_scopes.push(Rc::downgrade(&new_scopes));
        self.scopes.push(new_scopes);
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_scope(&mut self, key: &str) -> Option<usize> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.borrow().get(key) {
                return Some(*val);
            }
        }
        None
    }

    fn lookup_scope_local(&self, key: &str) -> Option<usize> {
        self.scopes
            .last()
            .unwrap()
            .borrow()
            .get(key)
            .and_then(|u| Some(*u))
    }

    fn setdefault_scope_local(&mut self, key: &str, default: Object) -> usize {
        if let Some(_) = self.lookup_scope_local(key) {
        } else {
            let idx = self.alloc(default.clone());
            self.insert_scope_local(key, idx);
        }
        self.lookup_scope_local(key).unwrap()
    }

    fn insert_scope_local(&mut self, key: &str, idx: usize) {
        self.scopes
            .last_mut()
            .unwrap()
            .borrow_mut()
            .insert(key.to_string(), idx);
    }

    pub fn deref(&mut self, val: Object) -> Object {
        let idx = match val.value {
            Value::Reference(u) => u,
            _ => return val,
        };
        let mut curidx = idx;
        loop {
            curidx = match self.memory[curidx].value {
                Value::Reference(u) => u,
                _ => return self.memory[curidx].clone(),
            };
            if curidx == idx {
                panic!("reference loop")
            }
        }
    }

    pub fn deref_idx(&mut self, idx: usize) -> usize {
        let mut curidx = idx;
        loop {
            curidx = match self.memory[curidx].value {
                Value::Reference(u) => u,
                _ => return curidx,
            };
            if curidx == idx {
                panic!("reference loop");
            }
        }
    }

    pub fn eval(&mut self, tree: &mut ParseTree) -> Result<Object, String> {
        let mut rv = self.undefined();
        for statement in tree.statements.iter_mut() {
            rv = self.eval_statement(statement)?;
        }
        Ok(self.deref(rv))
    }

    fn eval_statement(&mut self, statement: &mut Statement) -> Result<Object, String> {
        use crate::parser::Statement::*;
        Ok(match statement {
            Expr(expr) => self.eval_expr(expr)?,
            GlobalDecl(gd) => self.eval_global_decl(gd)?,
            ClassDecl(cd) => self.eval_class_decl(cd)?,
            Import(_) => todo!(),
            FromImport(_) => todo!(),
            Continue(_) => todo!(),
            Break(_) => todo!(),
            Return(_) => todo!(),
        })
    }

    fn eval_class_decl(&mut self, cd: &ClassDecl) -> Result<Object, String> {
        let val = self.class(cd, self.scopes.clone());
        let idx = self.alloc(val);
        self.insert_scope_local(&cd.name, idx);
        Ok(self.undefined())
    }

    fn eval_global_decl(&mut self, gd: &GlobalDecl) -> Result<Object, String> {
        use crate::lex::TokenType;
        let id = match &gd.token.token_type {
            TokenType::Identifier(s) => s,
            _ => unreachable!(),
        };

        let idx = match self.lookup_scope(id) {
            Some(i) => i,
            None => unreachable!(),
        };
        self.insert_scope_local(id, idx);
        Ok(self.undefined())
    }

    fn eval_if(&mut self, i: &mut If) -> Result<Object, String> {
        let mut ran_first = self.eval_if_internal(i)?;

        for ai in i.and_bodies.iter_mut() {
            if let Some(rv) = self.eval_if_internal(ai)? {
                ran_first = Some(rv);
            }
        }

        if ran_first.is_none() && i.else_body.is_some() {
            ran_first = Some(self.eval_expr(i.else_body.as_mut().unwrap())?);
        }

        match ran_first {
            Some(a) => Ok(a),
            None => Ok(self.undefined()),
        }
    }

    fn eval_if_internal(&mut self, i: &mut If) -> Result<Option<Object>, String> {
        let cond = self.eval_expr(&mut i.condition)?;
        let derefed = self.deref(cond);
        Ok(match derefed.value {
            Value::Bool(1) => Some(self.eval_expr(i.body.as_mut())?),
            Value::Bool(0) => None,
            _ => {
                return Err(format!(
                    "If expressions must have boolean conditions not {:?}",
                    derefed
                ))
            }
        })
    }

    fn eval_for(&mut self, f: &mut For) -> Result<Object, String> {
        let iterable = self.eval_expr(f.items.as_mut())?;
        let mut iterable = self.deref(iterable);
        match iterable.value {
            Value::List(l) => {
                for val in l {
                    let item = self.eval_assignable(f.reference.as_mut(), true)?;
                    self.memory[item] = val;
                    self.eval_expr(f.body.as_mut())?;
                }
                return Ok(self.undefined());
            }
            Value::Str(s) => {
                for chr in s.chars() {
                    let item = self.eval_assignable(f.reference.as_mut(), true)?;
                    self.memory[item] = self.char(chr);
                    self.eval_expr(f.body.as_mut())?;
                }
                return Ok(self.undefined());
            }
            Value::Function(_, _, _) => (),
            Value::Lambda(_, _) => (),
            Value::BuiltinFunc(_, _) => (),
            _ => unreachable!("{}", iterable.value),
        };
        loop {
            let item = self.eval_callable(&mut iterable, Vec::new())?.clone();
            match &item.value {
                Value::Undefined => break,
                _ => (),
            }
            let memloc = self.eval_assignable(f.reference.as_mut(), true)?;
            self.memory[memloc] = item;
            self.eval_expr(f.body.as_mut())?;
        }
        Ok(self.undefined())
    }

    fn eval_while(&mut self, w: &mut While) -> Result<Object, String> {
        loop {
            let cond = self.eval_expr(w.condition.as_mut())?;
            let derefed = self.deref(cond);
            match derefed.value {
                Value::Bool(1) => (),
                Value::Bool(0) => break,
                _ => {
                    return Err(format!(
                        "While loops must have boolean conditions not {:?}",
                        derefed
                    ))
                }
            }
            self.eval_expr(&mut w.body)?;
        }
        Ok(self.undefined())
    }

    fn eval_block(&mut self, b: &mut BlockExpr) -> Result<Object, String> {
        let mut rv = self.undefined();
        for statement in b.statements.iter_mut() {
            rv = self.eval_statement(statement)?;
        }
        Ok(rv)
    }

    fn eval_binop(&mut self, binop: &mut BinOp) -> Result<Object, String> {
        use crate::lex::TokenType::*;

        let mut lhs = self.eval_expr(binop.lhs.as_mut())?;
        match binop.op.token_type {
            BoolOr => match lhs.value {
                Value::Bool(1) => return Ok(lhs),
                _ => (),
            },
            BoolAnd => match lhs.value {
                Value::Bool(0) => return Ok(lhs),
                _ => (),
            },
            _ => (),
        }
        let mut rhs = self.eval_expr(binop.rhs.as_mut())?;
        lhs = self.deref(lhs);
        rhs = self.deref(rhs);

        Ok(match binop.op.token_type {
            Plus => match (lhs.value, rhs.value) {
                (Value::Str(a), Value::Str(b)) => self.string(format!("{}{}", a, b)),
                (Value::Int(a), Value::Int(b)) => self.int(a + b),
                (Value::Float(a), Value::Float(b)) => self.float(a + b),
                (Value::Float(a), Value::Int(b)) => self.float(a + b as f64),
                (Value::Int(a), Value::Float(b)) => self.float(a as f64 + b),
                (Value::Int(a), Value::Bool(b)) => self.int(a + b as i64),
                (Value::Bool(a), Value::Int(b)) => self.int(a as i64 + b),
                (Value::Char(a), Value::Char(b)) => self.char((a as u8 + b as u8) as char),
                (Value::Char(a), Value::Int(b)) => self.int(a as i64 + b),
                (Value::Int(a), Value::Char(b)) => self.int(a + b as i64),
                (Value::List(a), Value::List(b)) => {
                    let mut rv = a.clone();
                    rv.append(&mut b.clone());
                    self.list(rv)
                }
                (a, b) => {
                    return Err(format!(
                        "{}: cannot add two together. Not supported ({:?} + {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Minus => match (lhs.value, rhs.value) {
                (Value::Int(a), Value::Int(b)) => self.int(a - b),
                (Value::Float(a), Value::Float(b)) => self.float(a - b),
                (Value::Float(a), Value::Int(b)) => self.float(a - b as f64),
                (Value::Int(a), Value::Float(b)) => self.float(a as f64 - b),
                (Value::Int(a), Value::Bool(b)) => self.int(a - b as i64),
                (Value::Bool(a), Value::Int(b)) => self.int(a as i64 - b),
                (Value::Char(a), Value::Char(b)) => self.char((a as u8 - b as u8) as char),
                (Value::Char(a), Value::Int(b)) => self.int(a as i64 - b),
                (Value::Int(a), Value::Char(b)) => self.int(a - b as i64),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot subtract these two. Not supported ({:?} - {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Times => match (lhs.value, rhs.value) {
                (Value::Char(a), Value::Int(b)) => self.int(a as i64 * b),
                (Value::Int(a), Value::Char(b)) => self.int(a * b as i64),
                (Value::Int(a), Value::Int(b)) => self.int(a * b),
                (Value::Float(a), Value::Float(b)) => self.float(a * b),
                (Value::Float(a), Value::Int(b)) => self.float(a * b as f64),
                (Value::Int(a), Value::Float(b)) => self.float(a as f64 * b),
                (Value::Int(a), Value::Bool(b)) => self.int(a * b as i64),
                (Value::Bool(a), Value::Int(b)) => self.int(a as i64 * b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot multiply these two. Not supported ({:?} * {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Div => match (lhs.value, rhs.value) {
                (Value::Int(a), Value::Int(b)) => self.int(a / b),
                (Value::Float(a), Value::Float(b)) => self.float(a / b),
                (Value::Float(a), Value::Int(b)) => self.float(a / b as f64),
                (Value::Int(a), Value::Float(b)) => self.float(a as f64 / b),
                (Value::Bool(a), Value::Int(b)) => self.int(a as i64 / b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot divide these two. Not supported ({:?} / {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Mod => match (lhs.value, rhs.value) {
                (Value::Int(a), Value::Int(b)) => self.int(a % b),
                (Value::Char(a), Value::Int(b)) => self.int(a as i64 % b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot divide these two. Not supported ({:?} % {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            CmpEq => match (lhs.value, rhs.value) {
                (Value::Str(a), Value::Str(b)) => self.bool((a == b) as u8),
                (Value::Int(a), Value::Int(b)) => self.bool((a == b) as u8),
                (Value::Bool(a), Value::Bool(b)) => self.bool((a == b) as u8),
                (Value::Char(a), Value::Char(b)) => self.bool((a == b) as u8),
                (Value::Char(a), Value::Int(b)) => self.bool((a as i64 == b) as u8),
                (Value::Int(a), Value::Float(b)) => self.bool((a as f64 == b) as u8),
                (Value::Float(a), Value::Int(b)) => self.bool((a == b as f64) as u8),
                (Value::Float(a), Value::Float(b)) => self.bool((a == b) as u8),
                (Value::Null, Value::Null) => self.bool(1),
                (Value::Null, _) => self.bool(0),
                (_, Value::Null) => self.bool(0),
                (a, b) => return Err(format!(
                    "{}: cannot compare equality between these two. Not supported ({:?} == {:?})",
                    binop.op.location, a, b
                )),
            },
            CmpNotEq => match (lhs.value, rhs.value) {
                (Value::Str(a), Value::Str(b)) => self.bool((a != b) as u8),
                (Value::Int(a), Value::Int(b)) => self.bool((a != b) as u8),
                (Value::Bool(a), Value::Bool(b)) => self.bool((a != b) as u8),
                (Value::Char(a), Value::Char(b)) => self.bool((a != b) as u8),
                (Value::Char(a), Value::Int(b)) => self.bool((a as i64 != b) as u8),
                (Value::Int(a), Value::Char(b)) => self.bool((a != b as i64) as u8),
                (Value::Int(a), Value::Float(b)) => self.bool((a as f64 != b) as u8),
                (Value::Float(a), Value::Int(b)) => self.bool((a != b as f64) as u8),
                (Value::Float(a), Value::Float(b)) => self.bool((a != b) as u8),
                (Value::Null, Value::Null) => self.bool(0),
                (Value::Null, _) => self.bool(1),
                (_, Value::Null) => self.bool(1),
                (a, b) => return Err(format!(
                    "{}: cannot compare equality between these two. Not supported ({:?} != {:?})",
                    binop.op.location, a, b
                )),
            },
            CmpGE => {
                match (lhs.value, rhs.value) {
                    (Value::Float(a), Value::Float(b)) => self.bool((a >= b) as u8),
                    (Value::Float(a), Value::Int(b)) => self.bool((a >= b as f64) as u8),
                    (Value::Int(a), Value::Float(b)) => self.bool((a as f64 >= b) as u8),
                    (Value::Str(a), Value::Str(b)) => self.bool((a >= b) as u8),
                    (Value::Int(a), Value::Int(b)) => self.bool((a >= b) as u8),
                    (Value::Char(a), Value::Char(b)) => self.bool((a >= b) as u8),
                    (Value::Char(a), Value::Int(b)) => self.bool((a as i64 >= b) as u8),
                    (Value::Int(a), Value::Char(b)) => self.bool((a >= b as i64) as u8),
                    (a, b) => return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} >= {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            CmpGT => {
                match (lhs.value, rhs.value) {
                    (Value::Float(a), Value::Float(b)) => self.bool((a > b) as u8),
                    (Value::Float(a), Value::Int(b)) => self.bool((a > b as f64) as u8),
                    (Value::Int(a), Value::Float(b)) => self.bool((a as f64 > b) as u8),
                    (Value::Str(a), Value::Str(b)) => self.bool((a > b) as u8),
                    (Value::Int(a), Value::Int(b)) => self.bool((a > b) as u8),
                    (Value::Char(a), Value::Char(b)) => self.bool((a > b) as u8),
                    (Value::Char(a), Value::Int(b)) => self.bool((a as i64 > b) as u8),
                    (Value::Int(a), Value::Char(b)) => self.bool((a > b as i64) as u8),
                    (a, b) => return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} > {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            CmpLE => {
                match (lhs.value, rhs.value) {
                    (Value::Float(a), Value::Float(b)) => self.bool((a <= b) as u8),
                    (Value::Float(a), Value::Int(b)) => self.bool((a <= b as f64) as u8),
                    (Value::Int(a), Value::Float(b)) => self.bool((a as f64 <= b) as u8),
                    (Value::Str(a), Value::Str(b)) => self.bool((a <= b) as u8),
                    (Value::Int(a), Value::Int(b)) => self.bool((a <= b) as u8),
                    (Value::Char(a), Value::Char(b)) => self.bool((a <= b) as u8),
                    (Value::Char(a), Value::Int(b)) => self.bool((a as i64 <= b) as u8),
                    (Value::Int(a), Value::Char(b)) => self.bool((a <= b as i64) as u8),
                    (a, b) => return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} <= {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            CmpLT => {
                match (lhs.value, rhs.value) {
                    (Value::Float(a), Value::Float(b)) => self.bool((a < b) as u8),
                    (Value::Float(a), Value::Int(b)) => self.bool((a < b as f64) as u8),
                    (Value::Int(a), Value::Float(b)) => self.bool(((a as f64) < b) as u8),
                    (Value::Str(a), Value::Str(b)) => self.bool((a < b) as u8),
                    (Value::Int(a), Value::Int(b)) => self.bool((a < b) as u8),
                    (Value::Char(a), Value::Char(b)) => self.bool((a < b) as u8),
                    (Value::Char(a), Value::Int(b)) => self.bool(((a as i64) < b) as u8),
                    (Value::Int(a), Value::Char(b)) => self.bool((a < b as i64) as u8),
                    (a, b) => return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} < {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BitOr => match (lhs.value, rhs.value) {
                (Value::Bool(a), Value::Bool(b)) => self.bool(a | b),
                (Value::Bool(a), Value::Int(b)) => self.int(a as i64 | b),
                (Value::Int(a), Value::Bool(b)) => self.int(a | b as i64),
                (Value::Int(a), Value::Int(b)) => self.int(a | b),
                (Value::Char(a), Value::Char(b)) => self.char((a as u8 | b as u8) as char),
                (Value::Char(a), Value::Int(b)) => self.int((a as i64) | b),
                (Value::Int(a), Value::Char(b)) => self.int(a | b as i64),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compute or between these two. Not supported ({:?} | {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BitAnd => {
                match (lhs.value, rhs.value) {
                    (Value::Bool(a), Value::Bool(b)) => self.bool(a & b),
                    (Value::Bool(a), Value::Int(b)) => self.int(a as i64 & b),
                    (Value::Int(a), Value::Bool(b)) => self.int(a & b as i64),
                    (Value::Int(a), Value::Int(b)) => self.int(a & b),
                    (Value::Char(a), Value::Char(b)) => self.char((a as u8 & b as u8) as char),
                    (Value::Char(a), Value::Int(b)) => self.int((a as i64) & b),
                    (Value::Int(a), Value::Char(b)) => self.int(a & b as i64),
                    (a, b) => return Err(format!(
                        "{}: cannot computer and between these two. Not supported ({:?} & {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BitXor => {
                match (lhs.value, rhs.value) {
                    (Value::Bool(a), Value::Bool(b)) => self.bool(a ^ b),
                    (Value::Bool(a), Value::Int(b)) => self.int(a as i64 ^ b),
                    (Value::Int(a), Value::Bool(b)) => self.int(a ^ b as i64),
                    (Value::Int(a), Value::Int(b)) => self.int(a ^ b),
                    (Value::Char(a), Value::Char(b)) => self.char((a as u8 ^ b as u8) as char),
                    (Value::Char(a), Value::Int(b)) => self.int((a as i64) ^ b),
                    (Value::Int(a), Value::Char(b)) => self.int(a ^ b as i64),
                    (a, b) => return Err(format!(
                        "{}: cannot computer xor between these two. Not supported ({:?} ^ {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BoolXor => {
                match (lhs.value, rhs.value) {
                    (Value::Bool(a), Value::Bool(b)) => self.bool(a ^ b),
                    (a, b) => return Err(format!(
                        "{}: cannot computer boolean xor between these two. Not supported ({:?} ^^ {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BoolAnd => {
                match (lhs.value, rhs.value) {
                    (Value::Bool(a), Value::Bool(b)) => self.bool(a & b),
                    (a, b) => return Err(format!(
                        "{}: cannot computer boolean and between these two. Not supported ({:?} && {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BoolOr => {
                match (lhs.value, rhs.value) {
                    (Value::Bool(a), Value::Bool(b)) => self.bool(a | b),
                    (a, b) => return Err(format!(
                        "{}: cannot computer boolean or between these two. Not supported ({:?} || {:?})",
                        binop.op.location, a, b
                    )),
                }
            }
            BitShiftRight => match (lhs.value, rhs.value) {
                (Value::Int(a), Value::Char(b)) => self.int(a >> (b as u8)),
                (Value::Int(a), Value::Int(b)) => self.int(a >> b),
                (Value::Char(a), Value::Int(b)) => self.int((a as i64) >> b),
                (a, b) => return Err(format!(
                    "{}: cannot compute bitshift between these two. Not supported ({:?} >> {:?})",
                    binop.op.location, a, b
                )),
            },
            BitShiftLeft => match (lhs.value, rhs.value) {
                (Value::Int(a), Value::Char(b)) => self.int(a << (b as u8)),
                (Value::Int(a), Value::Int(b)) => self.int(a << b),
                (Value::Char(a), Value::Int(b)) => self.int((a as i64) << b),
                (a, b) => return Err(format!(
                    "{}: cannot compute bitshift between these two. Not supported ({:?} << {:?})",
                    binop.op.location, a, b
                )),
            },
            _ => {
                return Err(format!(
                    "{}: {:?}, unimplemented",
                    binop.op.location, binop.op.token_type
                ))
            }
        })
    }

    fn eval_expr(&mut self, expr: &mut Expression) -> Result<Object, String> {
        use crate::parser::Expression::*;
        match expr {
            CallExpr(callexpr) => self.eval_call_expr(callexpr),
            RefExpr(refexpr) => self.eval_ref_expr(refexpr, false),
            Immediate(immediate) => self.eval_immediate(immediate),
            BlockExpr(blockexpr) => self.eval_block(blockexpr),
            BinOp(binop) => self.eval_binop(binop),
            AssignExpr(assignexpr) => self.eval_assign(assignexpr),
            While(w) => self.eval_while(w),
            For(f) => self.eval_for(f),
            If(i) => self.eval_if(i),
            Function(f) => self.eval_function_def(f),
            Lambda(lambda) => self.eval_lambda_def(lambda),
            PreUnOp(u) => self.eval_unop_pre(u),
            PostUnOp(u) => self.eval_unop_post(u),
            List(l) => self.eval_list(l),
            IndexExpr(i) => self.eval_index_expr(i),
            DottedLookup(d) => self.eval_dotted_lookup(d),
        }
    }

    fn eval_dotted_lookup(&mut self, d: &mut DottedLookup) -> Result<Object, String> {
        let obj = self.eval_expr(d.lhs.as_mut())?;
        let obj = self.deref(obj);
        Ok(self.reference(*obj.fields.get(&d.rhs).ok_or(format!(
            "{}: `{}` has no field `.{}`",
            d.location, obj.value, d.rhs
        ))?))
    }

    fn eval_index_expr(&mut self, i: &mut IndexExpr) -> Result<Object, String> {
        let obj = self.eval_expr(i.obj.as_mut())?;
        let obj = self.deref(obj);
        let mut idxs = Vec::new();
        for idx in i.args.iter_mut() {
            let out = self.eval_expr(idx)?;
            idxs.push(self.deref(out));
        }
        let idx = if i.args.len() != 1 {
            return Err(format!(
                "{}: List types must be indexed with exactly one argument",
                i.location
            ));
        } else {
            match &idxs[0].value {
                Value::Int(i) => *i,
                Value::Char(c) => *c as i64,
                Value::Bool(b) => *b as i64,
                _ => {
                    return Err(format!(
                        "{}: List types must be indexed integers",
                        i.args[0].location()
                    ))
                }
            }
        };
        match obj.value {
            Value::List(l) => {
                let new_idx = if idx < 0 { idx + l.len() as i64 } else { idx };
                if new_idx < 0 || new_idx >= l.len() as i64 {
                    Err(format!("{}: Index out of range", i.args[0].location()))
                } else {
                    Ok(l[new_idx as usize].clone())
                }
            }
            Value::Str(s) => {
                let new_idx = if idx < 0 { idx + s.len() as i64 } else { idx };
                if new_idx < 0 || new_idx >= s.len() as i64 {
                    Err(format!("{}: Index out of range", i.args[0].location()))
                } else {
                    let chars: Vec<char> = s.chars().collect();
                    Ok(self.char(chars[new_idx as usize].clone()))
                }
            }
            _ => Err(format!("{}: Cannot index non list types", i.location)),
        }
    }

    fn eval_list(&mut self, l: &mut List) -> Result<Object, String> {
        let mut rv = Vec::new();
        for expr in l.exprs.iter_mut() {
            let val = self.eval_expr(expr)?;
            let val = self.deref(val);
            let idx = self.alloc(val);
            rv.push(self.reference(idx));
        }
        Ok(self.list(rv))
    }

    fn eval_unop_pre(&mut self, u: &mut PreUnOp) -> Result<Object, String> {
        use crate::lex::TokenType;
        let rhs = self.eval_expr(u.rhs.as_mut())?;
        let ref_idx = match rhs.value {
            Value::Reference(u) => Some(u),
            _ => None,
        };
        let rhs = self.deref(rhs);

        Ok(match u.op.token_type {
            TokenType::BoolNot => match rhs.value {
                Value::Bool(b) => self.bool(if b == 0 { 1 } else { 0 }),
                Value::Int(i) => self.bool(if i == 0 { 1 } else { 0 }),
                Value::Char(c) => self.bool(if c == '\0' { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::BitNot => match rhs.value {
                Value::Int(i) => self.int(!i),
                Value::Char(c) => self.char(!(c as u8) as char),
                _ => unreachable!(),
            },
            TokenType::Minus => match rhs.value {
                Value::Int(i) => self.int(-i),
                _ => unreachable!(),
            },
            TokenType::Plus => match rhs.value {
                Value::Int(i) => self.int(i),
                Value::Char(c) => self.char(c),
                _ => unreachable!(),
            },
            TokenType::Times => match rhs.value {
                Value::Null => return Err(format!("{}: Null value encountered", u.op.location)),
                _ => rhs,
            },
            TokenType::Inc => match rhs.value {
                Value::Int(i) => {
                    match &mut self.memory[ref_idx.unwrap()].value {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    self.int(i + 1)
                }
                Value::Char(c) => {
                    match &mut self.memory[ref_idx.unwrap()].value {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    self.char(((c as u8) + 1) as char)
                }
                _ => unreachable!(),
            },
            TokenType::Dec => match rhs.value {
                Value::Int(i) => {
                    match &mut self.memory[ref_idx.unwrap()].value {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    self.int(i - 1)
                }
                Value::Char(c) => {
                    match &mut self.memory[ref_idx.unwrap()].value {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    self.char(((c as u8) - 1) as char)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
    }

    fn eval_unop_post(&mut self, u: &PostUnOp) -> Result<Object, String> {
        use crate::lex::TokenType;

        let lhsidx = self.eval_ref_expr_int(&*u.lhs, false)?;
        let derefedidx = self.deref_idx(lhsidx);
        let lhs = self.memory[derefedidx].clone();
        Ok(match u.op.token_type {
            TokenType::Inc => match lhs.value {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx].value {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    self.int(i)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx].value {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    self.char(c)
                }
                _ => unreachable!(),
            },
            TokenType::Dec => match lhs.value {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx].value {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    self.int(i)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx].value {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    self.char(c)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
    }

    fn eval_function_def(&mut self, f: &Function) -> Result<Object, String> {
        let idx = self.alloc_function(f);
        let rv = self.reference(idx);
        if f.name.is_some() {
            self.insert_scope_local(f.name.as_ref().unwrap(), idx);
        }
        Ok(rv)
    }

    fn eval_lambda_def(&mut self, l: &Lambda) -> Result<Object, String> {
        let idx = self.alloc_lambda(l);
        Ok(self.reference(idx))
    }

    fn eval_assign(&mut self, expr: &mut AssignExpr) -> Result<Object, String> {
        use crate::lex::TokenType;
        let lhsidx = self.eval_assignable(expr.lhs.as_mut(), expr.allow_decl)?;
        let rhs = self.eval_expr(expr.rhs.as_mut())?;
        let rhs = self.deref(rhs);
        match &expr.op.token_type {
            TokenType::Equal => {
                self.memory[lhsidx] = rhs;
            }
            TokenType::AndEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a &= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 & *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a &= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a &= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 & *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::OrEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Bool(a), Value::Bool(b)) => *a |= *b,
                (Value::Int(a), Value::Int(b)) => *a |= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 | *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a |= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a |= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 | *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::XorEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a ^= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 ^ *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a ^= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a ^= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 ^ *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::PlusEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a += *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a += *b as i64,
                (Value::Int(a), Value::Char(b)) => *a += *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a += *b,
                (Value::Float(a), Value::Bool(b)) => *a += *b as f64,
                (Value::Float(a), Value::Char(b)) => *a += (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a + *b as f64,
                (Value::List(a), Value::List(b)) => a.append(&mut b.clone()),
                _ => unreachable!("{} += {}", self.memory[lhsidx].value, rhs.value),
            },
            TokenType::MinusEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a -= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a -= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a -= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a -= *b,
                (Value::Float(a), Value::Bool(b)) => *a -= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a -= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a - *b as f64,
                _ => unreachable!(),
            },
            TokenType::TimesEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a *= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a *= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a *= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a *= *b,
                (Value::Float(a), Value::Bool(b)) => *a *= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a *= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::DivEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a /= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a /= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a /= *b,
                (Value::Float(a), Value::Char(b)) => *a /= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::ModEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a %= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a %= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Float(a), Value::Char(b)) => *a %= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::BitShiftRightEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a >>= *b,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 >> *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::BitShiftLeftEq => match (&mut self.memory[lhsidx].value, &rhs.value) {
                (Value::Int(a), Value::Int(b)) => *a <<= *b,
                (Value::Char(a), Value::Int(b)) => *a = ((*a as u8) << *b as u8) as char,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        Ok(self.memory[lhsidx].clone())
    }

    fn eval_call_expr(&mut self, expr: &mut CallExpr) -> Result<Object, String> {
        let func = self.eval_expr(expr.function.as_mut())?;
        let mut func = self.deref(func);
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.eval_expr(arg)?);
        }
        let rv = self.eval_callable(&mut func, args)?;
        if let Value::Reference(u) = rv.value {
            self.safe = u;
        }
        Ok(rv)
    }

    fn eval_callable(
        &mut self,
        func: &mut Object,
        mut args: Vec<Object>,
    ) -> Result<Object, String> {
        self.openscope();
        let func = self.deref(func.clone());
        let rv = Ok(match func.value {
            Value::BuiltinFunc(_, f) => f(self, args),
            Value::Function(mut f, preset_args, scopes) => {
                let mut tmp = scopes.clone();
                swap(&mut self.scopes, &mut tmp);
                self.openscope();
                let mut fullargs = preset_args.clone();
                fullargs.append(&mut args);
                if fullargs.len() < f.argnames.len() {
                    let num_needed = f.argnames.len() - fullargs.len();
                    let start = f.default_args.len() - num_needed;
                    for i in start..f.default_args.len() {
                        fullargs.push(self.eval_expr(&mut f.default_args[i])?)
                    }
                }
                for (name, arg) in f.argnames.iter().zip(fullargs.iter()) {
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(name, idx);
                }
                let rv = self.eval_expr(&mut f.body)?;
                self.closescope();
                self.scopes = tmp;
                rv
            }
            Value::Lambda(mut l, scopes) => {
                assert_eq!(args.len(), l.num_args);
                let mut tmp = scopes.clone();
                swap(&mut self.scopes, &mut tmp);
                self.openscope();
                for (arg_num, arg) in args.iter().enumerate() {
                    let label = format!("\\{}", arg_num);
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(&label, idx);
                }
                let rv = self.eval_expr(&mut l.body)?;
                self.closescope();
                self.scopes = tmp;
                rv
            }
            Value::Class(cd, scopes) => {
                let mut obj = self.class_instance(&cd);
                let val = self.alloc(self.undefined());
                let mut init = 0;
                for (name, func) in cd.methods.iter() {
                    let memref =
                        self.alloc_function_args(func, vec![self.reference(val)], scopes.clone());
                    if name == "__init__" {
                        init = memref;
                    }
                    obj.fields.insert(name.clone(), memref);
                }
                for name in cd.fields.iter() {
                    obj.fields
                        .insert(name.clone(), self.alloc(self.undefined()));
                }
                self.memory[val] = obj;
                self.eval_callable(&mut self.memory[init].clone(), args)?;
                self.reference(val)
            }
            t => unreachable!("{}", t),
        });
        self.closescope();
        rv
    }

    fn eval_assignable(
        &mut self,
        expr: &mut Expression,
        allow_insert: bool,
    ) -> Result<usize, String> {
        match expr {
            Expression::RefExpr(re) => self.eval_ref_expr_int(re, allow_insert),
            Expression::IndexExpr(i) => match self.eval_index_expr(i)?.value {
                Value::Reference(u) => Ok(u),
                _ => unreachable!(),
            },
            Expression::DottedLookup(i) => match self.eval_dotted_lookup(i)?.value {
                Value::Reference(u) => Ok(u),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn eval_ref_expr(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<Object, String> {
        let idx = self.eval_ref_expr_int(expr, allow_insert)?;
        Ok(self.reference(idx))
    }

    fn eval_ref_expr_int(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => {
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, self.undefined()))
                } else if let Some(idx) = self.lookup_scope(s) {
                    Ok(idx)
                } else {
                    Err(format!("{}: No such name in scope...", s))
                }
            }
            LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, self.undefined()))
                } else if let Some(idx) = self.lookup_scope(&s) {
                    Ok(idx)
                } else {
                    Err(format!("{}: No such name in scope...", s))
                }
            }
            _ => Err("Unexpected...".into()),
        }
    }

    fn eval_immediate(&mut self, immediate: &Immediate) -> Result<Object, String> {
        use crate::lex::TokenType::*;
        Ok(match &immediate.value.token_type {
            Str(s) => self.string(s.clone()),
            Int(i) => self.int(i.clone()),
            Float(f) => self.float(f.clone()),
            Char(c) => self.char(c.clone()),
            Bool(b) => self.bool(*b),
            Null => self.null(),
            _ => return Err(format!("Unexpected immediate value {:?}", immediate.value)),
        })
    }
}
