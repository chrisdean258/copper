#![allow(dead_code)]
use crate::code_emitter::{CodeBuilder, Instruction};
use crate::operation::Operation;
use crate::parser::*;
use std::collections::HashMap;

pub struct Compiler {
    code: CodeBuilder,
    scopes: Vec<HashMap<String, usize>>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            code: CodeBuilder::new(),
            scopes: Vec::new(),
        }
    }

    pub fn compile(&mut self, name: String, p: &ParseTree) -> Vec<Instruction> {
        self.code.open_function(name);
        for statement in p.statements.iter() {
            self.statement(statement);
        }
        self.code.close_function();
        self.code.code()
    }

    fn open_scope(&mut self) {
        self.scopes.push(HashMap::new());
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

    fn refexpr(&mut self, _r: &RefExpr) {}

    fn assignment(&mut self, _a: &AssignExpr) {}

    fn immediate(&mut self, i: &Immediate) {
        self.code.emit(Operation::Push, vec![], vec![i.value]);
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
