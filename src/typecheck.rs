use crate::parser::ParseTree;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {
    system: TypeSystem,
    scopes: Vec<HashMap<String, Rc<RefCell<Type>>>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            system: TypeSystem::new(),
            scopes: Vec::new(),
        }
    }

    pub fn typecheck(&mut self, p: &mut ParseTree) -> Result<(), String> {
        let mut results = Vec::new();
        for statement in p.statements.iter_mut() {
            match self.statement(statement) {
                Ok(()) => (),
                Err(s) => results.push(s),
            }
        }
        if results.len() > 0 {
            Err(results.join("\n"))
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, s: &mut Statement) -> Result<(), String> {
        match s {
            Statement::Expr(e) => self.expr(e).map(|_| ()),
            Statement::ClassDecl(c) => todo!("{:?}", c),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(_) => Ok(()),
            Statement::Break(_) => Ok(()),
            Statement::Return(r) => todo!("{:?}", r),
        }
    }

    fn expr(&mut self, e: &mut Expression) -> Result<Type, String> {
        match e {
            Expression::While(w) => todo!("{:?}", w),
            Expression::For(f) => todo!("{:?}", f),
            Expression::If(i) => todo!("{:?}", i),
            Expression::CallExpr(c) => todo!("{:?}", c),
            Expression::RefExpr(r) => todo!("{:?}", r),
            Expression::Immediate(i) => self.immediate(i),
            Expression::BlockExpr(b) => todo!("{:?}", b),
            Expression::BinOp(b) => self.binop(b),
            Expression::PreUnOp(p) => todo!("{:?}", p),
            Expression::PostUnOp(p) => todo!("{:?}", p),
            Expression::AssignExpr(a) => todo!("{:?}", a),
            Expression::Function(f) => todo!("{:?}", f),
            Expression::Lambda(l) => todo!("{:?}", l),
            Expression::List(l) => todo!("{:?}", l),
            Expression::IndexExpr(i) => todo!("{:?}", i),
            Expression::DottedLookup(d) => todo!("{:?}", d),
        }
    }

    fn binop(&mut self, b: &mut BinOp) -> Result<Type, String> {
        let ltype = self.expr(b.lhs.as_mut())?;
        let rtype = self.expr(b.rhs.as_mut())?;
        match self.system.lookup_binop(b.op, ltype, rtype) {
            Some(t) => Ok(t),
            None => return Err(format!("{}: cannot apply binary operation '{}' {} '{}'. No operation has been defined between these types", b.location, self.system.typename(ltype), b.op, self.system.typename(rtype)))
        }
    }

    fn immediate(&mut self, i: &mut Immediate) -> Result<Type, String> {
        Ok(match i.value {
            ImmediateValue::Null => typesystem::NULL,
            ImmediateValue::Bool(_) => typesystem::BOOL,
            ImmediateValue::Char(_) => typesystem::CHAR,
            ImmediateValue::Int(_) => typesystem::INT,
            ImmediateValue::Float(_) => typesystem::FLOAT,
            ImmediateValue::Str(_) => typesystem::STR,
        })
    }
}
