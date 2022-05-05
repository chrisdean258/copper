#![allow(dead_code)]
use crate::code_emitter::{CodeBuilder, Instruction};
use crate::operation::Operation;
use crate::parser::*;
use crate::value::Value;
use std::collections::HashMap;

pub struct Compiler {
    code: CodeBuilder,
    scopes: Vec<HashMap<String, usize>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(),
            scopes: vec![HashMap::new()],
        }
    }

    pub fn compile(&mut self, name: String, p: &ParseTree) -> Vec<Instruction> {
        self.code.open_function(name);
        self.code.reserve(p.globals.unwrap());
        for statement in p.statements.iter() {
            self.statement(statement);
        }
        self.code.close_function();
        self.code.code()
    }

    fn open_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn lookup_scope(&self, _name: &str) -> usize {
        todo!("Full scoping is hard. Well' talk about it later");
        // for scope in self.scopes.iter().rev() {
        // match scope.get(name) {
        // Some(t) => return *t,
        // None => (),
        // }
        // }
        // unreachable!()
    }

    fn lookup_scope_local(&self, name: &str) -> usize {
        assert!(self.scopes.len() >= 1);
        *self.scopes.last().unwrap().get(name).unwrap()
    }

    fn close_scope(&mut self) {
        self.scopes.pop();
    }

    fn statement(&mut self, s: &Statement) {
        match s {
            Statement::Expr(e) => {
                self.expr(e);
                self.code.emit(Operation::Pop, vec![e.typ()], vec![]);
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
            ExpressionType::Function(f) => self.function(f),
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
        self.code.emit(p.op, vec![p.rhs.typ()], vec![]);
    }

    fn postunop(&mut self, p: &PostUnOp) {
        self.code.emit(p.op, vec![p.lhs.typ()], vec![]);
    }

    fn refexpr(&mut self, r: &RefExpr) {
        let offset = r.place.unwrap();
        self.code.push(Value::PtrOffset(offset));
        self.code.emit(Operation::RefFrame, vec![], vec![]);
    }

    fn assignment(&mut self, a: &AssignExpr) {
        self.expr(a.lhs.as_ref());
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

    fn whileexpr(&mut self, _w: &While) {}

    fn forexpr(&mut self, __w: &For) {}

    fn ifexpr(&mut self, _i: &If) {}

    fn ifexpr_int(&mut self, _i: &If, _is_and_if: bool) {}

    fn list(&mut self, _l: &List) {}

    fn index(&mut self, _i: &IndexExpr) {}

    fn function(&mut self, _f: &Function) {}

    fn lambda(&mut self, _l: &Lambda) {}

    fn lambdaarg(&mut self, _l: &LambdaArg) {}

    fn call(&mut self, _c: &CallExpr) {}

    fn dotted_lookup(&mut self, _d: &DottedLookup) {}
}
