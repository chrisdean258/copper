use crate::parser::ParseTree;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {
    pub system: TypeSystem,
    scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,
    allow_insert: Option<Type>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            system: TypeSystem::new(),
            scopes: vec![Rc::new(RefCell::new(HashMap::new()))],
            allow_insert: None,
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

    fn lookup_scope(&mut self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            match scope.borrow().get(name) {
                Some(t) => return Some(*t),
                None => (),
            }
        }
        None
    }

    fn insert_scope(&mut self, name: &str, t: Type) {
        (*self.scopes.last().unwrap())
            .borrow_mut()
            .insert(String::from(name), t);
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
            Expression::RefExpr(r) => self.refexpr(r),
            Expression::Immediate(i) => self.immediate(i),
            Expression::BlockExpr(b) => todo!("{:?}", b),
            Expression::BinOp(b) => self.binop(b),
            Expression::PreUnOp(p) => todo!("{:?}", p),
            Expression::PostUnOp(p) => todo!("{:?}", p),
            Expression::AssignExpr(a) => self.assignment(a),
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
        self.system.lookup_binop(b.op, ltype, rtype).ok_or(
            format!("{}: cannot apply binary operation `{}` {} `{}`. No operation has been defined between these types",
                    b.location, self.system.typename(ltype), b.op, self.system.typename(rtype)))
    }

    fn refexpr(&mut self, r: &mut RefExpr) -> Result<Type, String> {
        match self.lookup_scope(&r.name) {
            Some(t) => Ok(t),
            None if self.allow_insert.is_some() => {
                self.insert_scope(&r.name, self.allow_insert.unwrap());
                Ok(self.allow_insert.unwrap())
            }
            None => Err(format!(
                "{}: `{}` no such variable in scope",
                r.location, r.name
            )),
        }
    }

    fn assignment(&mut self, a: &mut AssignExpr) -> Result<Type, String> {
        if !a.lhs.is_lval() {
            return Err(format!(
                "{}: lhs of assignment is not assignable",
                a.location
            ));
        }
        let rhstype = self.expr(a.rhs.as_mut())?;
        self.allow_insert = Some(rhstype);
        let lhstype = self.expr(a.lhs.as_mut())?;
        self.allow_insert = None;
        self.system
            .lookup_assign(a.op, lhstype, rhstype)
            .ok_or(format!(
                "{}: Cannot assign type `{}` {} `{}`",
                a.location,
                self.system.typename(lhstype),
                a.op,
                self.system.typename(rhstype)
            ))
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
