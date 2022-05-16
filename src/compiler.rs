use crate::builtins::BuiltinFunction;
use crate::code_builder::{CodeBuilder, Instruction};
use crate::operation::Operation;
use crate::parser::*;
use crate::typesystem::{Signature, Type, TypeSystem, BOOL, UNIT};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem::swap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy)]
enum MemoryLocation {
    BuiltinFunction(usize),
    // CodeLocation(usize),
    GlobalVariable(usize),
    LocalVariable(usize),
}

#[derive(Debug, Clone)]
pub struct Compiler {
    code: CodeBuilder,
    need_ref: bool,
    types: Option<TypeSystem>,
    scopes: Vec<HashMap<String, MemoryLocation>>,
    num_args: usize,
    arg_types: Option<Vec<Type>>,
    strings: HashMap<String, usize>,
    num_locals: Vec<usize>,
    // lambda_arg_types: Option<Vec<Type>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(),
            need_ref: false,
            scopes: vec![
                {
                    // Builtins
                    let mut b = HashMap::new();
                    for (i, func) in BuiltinFunction::get_table().iter().enumerate() {
                        b.insert(func.name.clone(), MemoryLocation::BuiltinFunction(i));
                    }
                    b
                },
                HashMap::new(), //globals
            ],
            types: None,
            num_args: 0,
            arg_types: None,
            strings: HashMap::new(),
            num_locals: vec![0, 0],
        }
    }

    pub fn compile(
        &mut self,
        name: String,
        p: &ParseTree,
        types: &TypeSystem,
    ) -> (Vec<Instruction>, Vec<String>, usize) {
        self.types = Some(types.clone());
        self.code.open_function(name);
        if p.globals.unwrap() > 0 {
            self.code.reserve(p.globals.unwrap() - self.scopes[1].len());
        }
        for statement in p.statements.iter() {
            self.statement(statement, true);
        }
        let entry = self.code.close_function();
        self.types = None;
        let mut strings = vec![String::new(); self.strings.len()];
        for (s, p) in self.strings.iter() {
            strings[*p] = s.clone();
        }

        (self.code.code(), strings, entry)
    }

    fn open_scope(&mut self) {
        self.num_locals.push(0);
        self.scopes.push(HashMap::new());
    }

    fn close_scope(&mut self) {
        assert!(self.scopes.len() >= 1);
        self.num_locals.pop();
        self.scopes.pop();
    }

    fn insert_scope(&mut self, name: String, what: MemoryLocation) {
        assert!(self.scopes.len() >= 2);
        let scope = self.scopes.last_mut().unwrap();
        assert!(scope.insert(name.clone(), what).is_none());
    }

    fn next_local(&mut self) -> MemoryLocation {
        // Actually local == global so we will write out a global
        *self.num_locals.last_mut().unwrap() += 1;
        if self.scopes.len() == 2 {
            MemoryLocation::GlobalVariable(*self.num_locals.last().unwrap() - 1)
        } else {
            MemoryLocation::LocalVariable(*self.num_locals.last().unwrap() - 1)
        }
    }

    fn lookup_scope_local_or_global(&mut self, name: &str) -> MemoryLocation {
        assert!(self.scopes.len() >= 2);
        let scope = self.scopes.last().unwrap();
        if let Some(u) = scope.get(name) {
            return *u;
        }
        if let Some(u) = self.scopes[1].get(name) {
            return *u;
        }
        if let Some(u) = self.scopes[0].get(name) {
            return *u;
        }
        unreachable!(
            // self.arg_types.is_some(),
            "Looking up `{}` failed",
            name
        );
        // let types = self
        // .types
        // .as_ref()
        // .unwrap()
        // .format_args(self.arg_types.as_ref().unwrap());
        // let funcname = format!("{}({})", name, types);
        // if let Some(u) = scope.get(&funcname) {
        // return *u;
        // }
        // if let Some(u) = self.scopes[1].get(&funcname) {
        // return *u;
        // }
        // if let Some(u) = self.scopes[0].get(&funcname) {
        // return *u;
        // }
        // unreachable!(
        // "Tried to lookup `{}` and `{}` which were not present. {:?}",
        // name, funcname, self.scopes
        // );
    }

    fn entomb_string(&mut self, string: String) -> usize {
        let len = self.strings.len();
        *self.strings.entry(string).or_insert(len)
    }

    fn statement(&mut self, s: &Statement, pop: bool) {
        match s {
            Statement::Expr(e) => {
                self.expr(e);
                if pop {
                    self.code.emit(Operation::Pop, vec![UNIT], None);
                }
            }
            Statement::ClassDecl(c) => todo!("{:?}", c),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(c) => todo!("{:?}", c),
            Statement::Break(b) => todo!("{:?}", b),
            Statement::Return(r) => todo!("{:?}", r),
        }
    }

    fn expr(&mut self, e: &Expression) {
        match &e.etype {
            ExpressionType::While(w) => self.whileexpr(w),
            ExpressionType::For(f) => self.forexpr(f),
            ExpressionType::If(i) => self.ifexpr(i),
            ExpressionType::CallExpr(c) => self.call(c),
            ExpressionType::RefExpr(r) => self.refexpr(r),
            ExpressionType::Immediate(i) => self.immediate(i),
            ExpressionType::BlockExpr(b) => self.block(b),
            ExpressionType::BinOp(b) => self.binop(b),
            ExpressionType::PreUnOp(p) => self.preunop(p, e.derived_type.unwrap()),
            ExpressionType::PostUnOp(p) => self.postunop(p, e.derived_type.unwrap()),
            ExpressionType::AssignExpr(a) => self.assignment(a),
            ExpressionType::Function(f) => self.function(
                f.clone(),
                self.types
                    .as_ref()
                    .unwrap()
                    .get_signatures_for_func(e.derived_type.unwrap()),
            ),
            ExpressionType::Lambda(l) => self.lambda(
                l.clone(),
                self.types
                    .as_ref()
                    .unwrap()
                    .get_signatures_for_func(e.derived_type.unwrap()),
            ),
            ExpressionType::List(l) => self.list(l),
            ExpressionType::IndexExpr(i) => self.index(i),
            ExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
            ExpressionType::Str(s) => self.string(s),
        }
    }

    fn binop(&mut self, b: &BinOp) {
        self.expr(b.lhs.as_ref());
        self.expr(b.rhs.as_ref());
        self.code.emit(b.op, vec![b.lhs.typ(), b.rhs.typ()], None);
    }

    fn preunop(&mut self, p: &PreUnOp, typ: Type) {
        match p.op {
            Operation::PreInc => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Plus, vec![typ], None);
                self.code.store();
            }
            Operation::PreDec => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Minus, vec![typ], None);
                self.code.store();
            }
            t if t.is_preunop() => {
                self.expr(p.rhs.as_ref());
                self.code.emit(p.op, vec![p.rhs.typ()], None);
            }
            _ => unreachable!(),
        }
    }

    fn postunop(&mut self, p: &PostUnOp, typ: Type) {
        self.get_ref(p.lhs.as_ref());
        self.code.dup();
        self.code.load();
        self.code.dup();
        self.code.rotate(3);
        self.code.push(Value::Int(1));
        self.code.emit(
            match p.op {
                Operation::PostInc => Operation::Plus,
                Operation::PostDec => Operation::Minus,
                _ => unreachable!(),
            },
            vec![typ],
            None,
        );
        self.code.store();
        self.code.pop();
    }

    fn get_ref(&mut self, e: &Expression) {
        self.need_ref = true;
        self.expr(e);
        self.need_ref = false;
    }

    fn refexpr(&mut self, r: &RefExpr) {
        let mem: MemoryLocation = if r.is_decl {
            let new_var = self.next_local();
            self.insert_scope(r.name.clone(), new_var);
            new_var
        } else {
            self.lookup_scope_local_or_global(&r.name)
        };
        match mem {
            MemoryLocation::BuiltinFunction(u) if !self.need_ref => {
                self.code.builtin_ref(u);
                return;
            }
            MemoryLocation::BuiltinFunction(u) => panic!("Cannot write to builtin function {}", u),
            // MemoryLocation::CodeLocation(u) if !self.need_ref => self.code.code_ref(u),
            // MemoryLocation::CodeLocation(u) => panic!("Cannot write to builtin function {}", u),
            MemoryLocation::GlobalVariable(u) => self.code.global_ref(u),
            MemoryLocation::LocalVariable(u) => self.code.local_ref(u as isize),
        };
        if !self.need_ref {
            self.code.load();
        }
    }

    fn assignment(&mut self, a: &AssignExpr) {
        self.get_ref(a.lhs.as_ref());
        if a.op != Operation::Equal {
            self.code.dup();
            self.code.load();
            self.expr(a.rhs.as_ref());
            self.code.emit(
                a.op.underlying_binop(),
                vec![a.lhs.typ(), a.rhs.typ()],
                None,
            );
        } else {
            self.expr(a.rhs.as_ref());
        }
        self.code.store();
    }

    fn immediate(&mut self, i: &Immediate) {
        self.code.push(i.value);
    }

    fn block(&mut self, b: &BlockExpr) {
        let mut first = true;
        for statement in b.statements.iter() {
            if !first {
                first = false;
                self.code.pop();
            }
            self.statement(statement, false);
        }
    }

    fn whileexpr(&mut self, w: &While) {
        let start = self.code.jump_relative(0);
        let body = self.code.next_function_relative_addr();
        self.expr(w.body.as_ref());
        let stop = self.code.next_function_relative_addr();
        self.code.backpatch_jump_rel(start, stop as isize);
        self.expr(w.condition.as_ref());
        self.code.jump_relative_if(body as isize);
        self.code.push(Value::Uninitialized);
    }

    fn forexpr(&mut self, _f: &For) {}

    fn ifexpr(&mut self, i: &If) {
        if i.and_bodies.len() > 0 {
            self.code.push(Value::Uninitialized); // If return value
            self.expr(i.condition.as_ref());
            self.code.dup();
            self.code.emit(Operation::BoolNot, vec![BOOL], None);
            let mut next_branch = self.code.jump_relative_if(0);
            self.expr(i.body.as_ref());
            self.code.rotate(3);
            self.code.swap();
            self.code.pop();

            for (body, _) in i.and_bodies.iter() {
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
                self.expr(body.condition.as_ref());
                self.code.dup();
                self.code.rotate(3);
                self.code.emit(Operation::BoolOr, vec![BOOL, BOOL], None);
                self.code.swap();
                self.code.emit(Operation::BoolNot, vec![BOOL], None);
                next_branch = self.code.jump_relative_if(0);
                self.expr(body.body.as_ref());
                self.code.rotate(3);
                self.code.swap();
                self.code.pop();
            }
            let loc = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(next_branch, loc as isize);
            if let Some(else_body) = &i.else_body {
                next_branch = self.code.jump_relative_if(0);
                self.expr(else_body);
                self.code.swap();
                self.code.pop();
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
            }
        } else {
            self.expr(i.condition.as_ref());
            if let Some(else_body) = &i.else_body {
                let true_branch = self.code.jump_relative_if(0);
                self.expr(else_body);
                let jump_to_end = self.code.jump_relative(0);
                let true_loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(true_branch, true_loc as isize);
                self.expr(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            } else {
                self.code.emit(Operation::BoolNot, vec![BOOL], None);
                let jump_to_end = self.code.jump_relative_if(0);
                self.expr(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            }
        }
    }

    fn string(&mut self, s: &Str) {
        let str_idx = self.entomb_string(s.string.clone());
        self.code.push(Value::Str(str_idx));
    }

    fn list(&mut self, _l: &List) {
        todo!()
    }

    fn index(&mut self, _i: &IndexExpr) {
        todo!()
    }

    fn function(&mut self, f: Rc<RefCell<Function>>, sigs: Vec<Signature>) {
        if let Some(name) = &f.borrow().name {
            for sig in sigs.iter() {
                // let funcname = format!(
                // "{}({})",
                // name,
                // self.types.as_ref().unwrap().format_args(&sig.inputs)
                // );
                let varloc = self.next_local();
                self.insert_scope(name.clone(), varloc);
                let addr = self.single_function(&f.borrow(), sig);
                match varloc {
                    MemoryLocation::LocalVariable(u) => self.code.local_ref(u as isize),
                    MemoryLocation::GlobalVariable(u) => self.code.local_ref(u as isize),
                    _ => unreachable!(),
                };
                self.code.push(Value::Ptr(addr));
                self.code.store();
                break;
            }
        } else {
            for sig in sigs.iter() {
                let addr = self.single_function(&f.borrow(), sig);
                self.code.push(Value::Ptr(addr));
            }
        }
    }

    fn single_function(&mut self, f: &Function, sig: &Signature) -> usize {
        assert!(
            sig.inputs.len() == f.argnames.len(),
            "{:#?}\n-------------------\n{:#?}",
            f,
            sig
        );
        let name = match &f.name {
            Some(n) => format!(
                "{}{}",
                n,
                self.types.as_ref().unwrap().format_signature(sig)
            ),
            None => format!(
                "<anonymous>{}",
                self.types.as_ref().unwrap().format_signature(sig)
            ),
        };
        self.open_scope();
        self.code.open_function(name);
        for argname in f.argnames.iter() {
            let var = self.next_local();
            self.insert_scope(argname.clone(), var);
        }
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if f.locals.unwrap() > f.argnames.len() {
            self.code.reserve(f.locals.unwrap() - f.argnames.len());
        }
        self.expr(f.body.as_ref());
        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        self.code.close_function()
    }

    fn lambda(&mut self, l: Rc<RefCell<Lambda>>, sigs: Vec<Signature>) {
        for sig in sigs.iter() {
            let addr = self.single_lambda(&l.borrow(), sig);
            self.code.push(Value::Ptr(addr));
            break;
        }
    }

    fn single_lambda(&mut self, l: &Lambda, sig: &Signature) -> usize {
        let name = format!(
            "<lambda>{}",
            self.types.as_ref().unwrap().format_signature(sig)
        );
        self.open_scope();
        self.code.open_function(name);
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if l.locals.unwrap() > sig.inputs.len() {
            self.code.reserve(l.locals.unwrap() - sig.inputs.len());
        }
        self.expr(l.body.as_ref());
        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        self.code.close_function()
    }

    fn lambdaarg(&mut self, l: &LambdaArg) {
        self.code.local_ref(l.number as isize);
        self.code.load();
    }

    fn call(&mut self, c: &CallExpr) {
        self.code.push(Value::Uninitialized); // ip
        self.code.push(Value::Uninitialized); // bp
        for arg in c.args.iter() {
            self.expr(arg);
        }
        self.code.push(Value::Count(c.args.len()));

        let mut arg_types = Some(c.args.iter().map(|a| a.derived_type.unwrap()).collect());
        swap(&mut arg_types, &mut self.arg_types);
        self.expr(c.function.as_ref());
        self.arg_types = arg_types;

        self.code.call();
    }

    fn dotted_lookup(&mut self, _d: &DottedLookup) {
        todo!()
    }
}
