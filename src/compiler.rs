#![allow(dead_code)]
use crate::code_emitter::{CodeBuilder, Instruction};
use crate::operation::Operation;
use crate::parser::*;
use crate::typesystem::{BOOL, UNIT};
use crate::value::Value;
use std::collections::HashMap;

pub struct Compiler {
    code: CodeBuilder,
    scopes: Vec<HashMap<String, usize>>,
    need_ref: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(),
            scopes: vec![HashMap::new()],
            need_ref: false,
        }
    }

    pub fn compile(&mut self, name: String, p: &ParseTree) -> Vec<Instruction> {
        self.code.open_function(name);
        self.code.reserve(p.globals.unwrap() as isize);
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
        let offset = r.place.unwrap();
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
        let start = self.code.jump_unknown();
        let body = self.code.next_function_relative_addr();
        self.expr(w.body.as_ref());
        let stop = self.code.next_function_relative_addr();
        self.code.backpatch(Operation::JumpRel, start, stop);
        self.expr(w.condition.as_ref());
        self.code.jump_relative_if(body as isize);
        self.code.push(Value::Null);
    }

    fn forexpr(&mut self, _f: &For) {}

    fn ifexpr(&mut self, i: &If) {
        self.code.push(Value::Null); // If return value
        self.code.push(Value::Bool(0));
        self.expr(i.condition.as_ref());
        self.code.emit(Operation::BoolOr, vec![BOOL, BOOL], vec![]);
        self.code.dup();
        self.code.emit(Operation::BoolNot, vec![BOOL], vec![]);
        let mut next_branch = self.code.jump_unknown();
        self.expr(i.body.as_ref());
        self.code.rotate(3);
        self.code.swap();
        self.code.pop();

        for (body, _) in i.and_bodies.iter() {
            let loc = self.code.next_function_relative_addr();
            self.code.backpatch(Operation::JumpRelIf, next_branch, loc);
            self.expr(body.condition.as_ref());
            self.code.emit(Operation::BoolOr, vec![BOOL, BOOL], vec![]);
            self.code.dup();
            self.code.emit(Operation::BoolNot, vec![BOOL], vec![]);
            next_branch = self.code.jump_unknown();
            self.expr(body.body.as_ref());
            self.code.rotate(3);
            self.code.swap();
            self.code.pop();
        }

        let loc = self.code.next_function_relative_addr();
        self.code.backpatch(Operation::JumpRelIf, next_branch, loc);

        if let Some(else_body) = &i.else_body {
            next_branch = self.code.jump_unknown();
            self.expr(else_body);
            self.code.swap();
            let loc = self.code.next_function_relative_addr();
            self.code.backpatch(Operation::JumpRelIf, next_branch, loc);
        }
    }

    fn list(&mut self, _l: &List) {}

    fn index(&mut self, _i: &IndexExpr) {}

    fn function(&mut self, _f: &Function) {}

    fn lambda(&mut self, _l: &Lambda) {}

    fn lambdaarg(&mut self, _l: &LambdaArg) {}

    fn call(&mut self, _c: &CallExpr) {}

    fn dotted_lookup(&mut self, _d: &DottedLookup) {}
}
