#![allow(dead_code)]
use crate::code_emitter::{CodeBuilder, Instruction};
use crate::operation::Operation;
use crate::parser::*;
use crate::typesystem::{Signature, TypeSystem, BOOL, UNIT};
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Compiler {
    code: CodeBuilder,
    need_ref: bool,
    types: Option<TypeSystem>,
    scopes: Vec<HashMap<String, usize>>,
    num_args: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(),
            need_ref: false,
            scopes: vec![HashMap::new()],
            types: None,
            num_args: 0,
        }
    }

    pub fn compile(
        &mut self,
        name: String,
        p: &ParseTree,
        types: &TypeSystem,
    ) -> (Vec<Instruction>, usize) {
        self.types = Some(types.clone());
        self.code.open_function(name);
        self.code.reserve(p.globals.unwrap());
        for statement in p.statements.iter() {
            self.statement(statement);
        }
        self.code.crash();
        let entry = self.code.close_function();
        self.types = None;
        (self.code.code(), entry)
    }

    fn open_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn close_scope(&mut self) {
        assert!(self.scopes.len() >= 1);
        self.scopes.pop();
    }

    fn insert_scope(&mut self, name: String) -> usize {
        assert!(self.scopes.len() >= 1);
        let scope = self.scopes.last_mut().unwrap();
        let val = scope.len();
        assert!(scope.insert(name, val).is_none());
        val
    }

    fn lookup_scope_local(&mut self, name: &str) -> usize {
        assert!(self.scopes.len() >= 1);
        let scope = self.scopes.last().unwrap();
        match scope.get(name) {
            Some(s) => *s,
            None => unreachable!(
                "Bug in scope lookup. tried to look up `{}` which had not been declared\n{:#?}",
                name, self.scopes
            ),
        }
    }

    fn statement(&mut self, s: &Statement) {
        match s {
            Statement::Expr(e) => {
                self.expr(e);
                self.code.emit(Operation::Pop, vec![UNIT], vec![]);
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
            ExpressionType::PreUnOp(p) => self.preunop(p),
            ExpressionType::PostUnOp(p) => self.postunop(p),
            ExpressionType::AssignExpr(a) => self.assignment(a),
            ExpressionType::Function(f) => self.function(
                f.clone(),
                self.types
                    .as_ref()
                    .unwrap()
                    .get_signatures_for_func(e.derived_type.unwrap()),
            ),
            ExpressionType::Lambda(l) => self.lambda(l),
            ExpressionType::List(l) => self.list(l),
            ExpressionType::IndexExpr(i) => self.index(i),
            ExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
        }
    }

    fn binop(&mut self, b: &BinOp) {
        self.expr(b.lhs.as_ref());
        self.expr(b.rhs.as_ref());
        self.code.emit(b.op, vec![b.lhs.typ(), b.rhs.typ()], vec![]);
    }

    fn preunop(&mut self, p: &PreUnOp) {
        match p.op {
            Operation::PreInc => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Plus, vec![], vec![]);
                self.code.store();
            }
            Operation::PreDec => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Minus, vec![], vec![]);
                self.code.store();
            }
            t if t.is_preunop() => {
                self.expr(p.rhs.as_ref());
                self.code.emit(p.op, vec![p.rhs.typ()], vec![]);
            }
            _ => unreachable!(),
        }
    }

    fn postunop(&mut self, p: &PostUnOp) {
        match p.op {
            Operation::PostInc => {
                self.get_ref(p.lhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.dup();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Plus, vec![], vec![]);
                self.code.swap();
                self.code.rotate(3);
                self.code.store();
                self.code.pop();
            }
            Operation::PostDec => {
                self.get_ref(p.lhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.dup();
                self.code.push(Value::Int(1));
                self.code.emit(Operation::Minus, vec![], vec![]);
                self.code.swap();
                self.code.rotate(3);
                self.code.store();
                self.code.pop();
            }
            _ => unreachable!(),
        }
    }

    fn get_ref(&mut self, e: &Expression) {
        self.need_ref = true;
        self.expr(e);
        self.need_ref = false;
    }

    fn refexpr(&mut self, r: &RefExpr) {
        let offset = if r.is_decl {
            self.insert_scope(r.name.clone())
        } else {
            self.lookup_scope_local(&r.name)
        };
        self.code.local_ref(offset as isize);
        if !self.need_ref {
            self.code.load();
        }
    }

    fn assignment(&mut self, a: &AssignExpr) {
        self.need_ref = true;
        self.expr(a.lhs.as_ref());
        self.need_ref = false;
        if a.op != Operation::Equal {
            self.code.dup();
            self.code.load();
            self.expr(a.rhs.as_ref());
            self.code.emit(
                a.op.underlying_binop(),
                vec![a.lhs.typ(), a.rhs.typ()],
                vec![],
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
        for statement in b.statements.iter() {
            self.statement(statement);
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
        self.code.push(Value::Uninitialized); // If return value
        self.expr(i.condition.as_ref());
        self.code.dup();
        self.code.emit(Operation::BoolNot, vec![BOOL], vec![]);
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
            self.code.emit(Operation::BoolOr, vec![BOOL, BOOL], vec![]);
            self.code.swap();
            self.code.emit(Operation::BoolNot, vec![BOOL], vec![]);
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
    }

    fn list(&mut self, _l: &List) {
        todo!()
    }

    fn index(&mut self, _i: &IndexExpr) {
        todo!()
    }

    fn function(&mut self, f: Rc<RefCell<Function>>, sigs: Vec<Signature>) {
        assert!(
            sigs.len() == 1,
            "Temporary until we get function templating working"
        );
        if let Some(name) = &f.borrow().name {
            let off = self.insert_scope(name.clone());
            for sig in sigs.iter() {
                let addr = self.single_function(&f.borrow(), sig);
                self.code.local_ref(off as isize);
                self.code.push(Value::Ptr(addr));
                self.code.store();
            }
        } else {
            for sig in sigs.iter() {
                let addr = self.single_function(&f.borrow(), sig);
                self.code.push(Value::Ptr(addr));
            }
        }
    }

    fn single_function(&mut self, f: &Function, sig: &Signature) -> usize {
        assert!(sig.inputs.len() == f.argnames.len());
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
            self.insert_scope(argname.clone());
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

    fn lambda(&mut self, _l: &Lambda) {
        todo!()
    }

    fn lambdaarg(&mut self, _l: &LambdaArg) {
        todo!()
    }

    fn call(&mut self, c: &CallExpr) {
        self.code.push(Value::Uninitialized); // ip
        self.code.push(Value::Uninitialized); // bp
        for arg in c.args.iter() {
            self.expr(arg);
        }
        self.code.push(Value::Count(c.args.len()));
        self.expr(c.function.as_ref());
        self.code.call();
    }

    fn dotted_lookup(&mut self, _d: &DottedLookup) {
        todo!()
    }
}
