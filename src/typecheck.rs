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

type TypeError = Vec<String>;

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
                Ok(_) => (),
                Err(mut s) => results.append(&mut s),
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

    fn statement(&mut self, s: &mut Statement) -> Result<Type, TypeError> {
        match s {
            Statement::Expr(e) => self.expr(e),
            Statement::ClassDecl(c) => todo!("{:?}", c),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(_) => Ok(NULL),
            Statement::Break(_) => Ok(NULL),
            Statement::Return(r) => todo!("{:?}", r),
        }
    }

    fn expr(&mut self, e: &mut Expression) -> Result<Type, TypeError> {
        match e {
            Expression::While(w) => self.whileexpr(w),
            Expression::For(f) => self.forexpr(f),
            Expression::If(i) => self.ifexpr(i),
            Expression::CallExpr(c) => todo!("{:?}", c),
            Expression::RefExpr(r) => self.refexpr(r),
            Expression::Immediate(i) => self.immediate(i),
            Expression::BlockExpr(b) => self.block(b),
            Expression::BinOp(b) => self.binop(b),
            Expression::PreUnOp(p) => todo!("{:?}", p),
            Expression::PostUnOp(p) => todo!("{:?}", p),
            Expression::AssignExpr(a) => self.assignment(a),
            Expression::Function(f) => todo!("{:?}", f),
            Expression::Lambda(l) => todo!("{:?}", l),
            Expression::List(l) => self.list(l),
            Expression::IndexExpr(i) => self.index(i),
            Expression::DottedLookup(d) => todo!("{:?}", d),
        }
    }

    fn binop(&mut self, b: &mut BinOp) -> Result<Type, TypeError> {
        let ltype = self.expr(b.lhs.as_mut())?;
        let rtype = self.expr(b.rhs.as_mut())?;
        self.system.lookup_binop(b.op, ltype, rtype).ok_or(
            vec![format!("{}: cannot apply binary operation `{}` {} `{}`. No operation has been defined between these types",
                    b.location, self.system.typename(ltype), b.op, self.system.typename(rtype))])
    }

    fn refexpr(&mut self, r: &mut RefExpr) -> Result<Type, TypeError> {
        match self.lookup_scope(&r.name) {
            Some(t) => Ok(t),
            None if self.allow_insert.is_some() => {
                self.insert_scope(&r.name, self.allow_insert.unwrap());
                Ok(self.allow_insert.unwrap())
            }
            None => Err(vec![format!(
                "{}: `{}` no such variable in scope",
                r.location, r.name
            )]),
        }
    }

    fn assignment(&mut self, a: &mut AssignExpr) -> Result<Type, TypeError> {
        if !a.lhs.is_lval() {
            return Err(vec![format!(
                "{}: lhs of assignment is not assignable",
                a.location
            )]);
        }
        let rhstype = self.expr(a.rhs.as_mut())?;
        if rhstype == UNIT {
            return Err(vec![format!("{}: cannot assign unit value", a.location)]);
        }
        self.allow_insert = Some(rhstype);
        let lhstype = self.expr(a.lhs.as_mut())?;
        self.allow_insert = None;
        self.system
            .lookup_assign(a.op, lhstype, rhstype)
            .ok_or(vec![format!(
                "{}: Cannot assign type `{}` {} `{}`",
                a.location,
                self.system.typename(lhstype),
                a.op,
                self.system.typename(rhstype)
            )])
    }

    fn immediate(&mut self, i: &mut Immediate) -> Result<Type, TypeError> {
        Ok(match i.value {
            ImmediateValue::Null => typesystem::NULL,
            ImmediateValue::Bool(_) => typesystem::BOOL,
            ImmediateValue::Char(_) => typesystem::CHAR,
            ImmediateValue::Int(_) => typesystem::INT,
            ImmediateValue::Float(_) => typesystem::FLOAT,
            ImmediateValue::Str(_) => typesystem::STR,
        })
    }

    fn block(&mut self, b: &mut BlockExpr) -> Result<Type, TypeError> {
        let mut errors = Vec::new();
        let mut return_type = UNIT;
        for statement in b.statements.iter_mut() {
            match self.statement(statement) {
                Ok(t) => return_type = t,
                Err(mut e) => errors.append(&mut e),
            }
        }

        if errors.len() == 0 {
            Ok(return_type)
        } else {
            Err(errors)
        }
    }

    fn whileexpr(&mut self, w: &mut While) -> Result<Type, TypeError> {
        let mut errors = Vec::new();
        match self.expr(w.condition.as_mut()) {
            Ok(t) if t == BOOL => (),
            Ok(t) => errors.push(format!(
                "{}: while loop conditionals must be `bool` not `{}`",
                w.condition.as_ref().location(),
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }
        match self.expr(w.body.as_mut()) {
            Ok(_) => (), // This is currently discarded but would be used with `break`
            Err(mut s) => errors.append(&mut s),
        }

        if errors.len() == 0 {
            Ok(UNIT)
        } else {
            Err(errors)
        }
    }

    fn forexpr(&mut self, _w: &mut For) -> Result<Type, TypeError> {
        todo!("Havent worked out the details of iteration yet")
    }

    fn ifexpr(&mut self, i: &mut If) -> Result<Type, TypeError> {
        self.ifexpr_int(i, false)
    }

    fn ifexpr_int(&mut self, i: &mut If, is_and_if: bool) -> Result<Type, TypeError> {
        let mut errors: TypeError = Vec::new();
        let mut rv = UNIT;
        let mut return_unit = false;
        match self.expr(i.condition.as_mut()) {
            Ok(t) if t == BOOL => (),
            Ok(t) => errors.push(format!(
                "{}: if conditionals must be `bool` not `{}`",
                i.condition.as_ref().location(),
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }

        match self.expr(i.body.as_mut()) {
            Ok(t) => rv = t,
            Err(mut s) => errors.append(&mut s),
        }

        for body in i.and_bodies.iter_mut() {
            match self.ifexpr(body) {
                Ok(t) if t == rv => (),
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            }
        }

        match &mut i.else_body {
            Some(b) => match self.expr(b.as_mut()) {
                Ok(t) if t == rv => (),
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            },
            None => return_unit = !is_and_if,
        }

        if errors.len() != 0 {
            Err(errors)
        } else if return_unit {
            Ok(UNIT)
        } else {
            Ok(rv)
        }
    }

    fn list(&mut self, l: &mut List) -> Result<Type, TypeError> {
        let mut errors: TypeError = Vec::new();
        let mut interior_type = UNIT;
        for expr in l.exprs.iter_mut() {
            match self.expr(expr) {
                Ok(t) if (interior_type == UNIT) ^ (t == interior_type) => {
                    interior_type = t;
                }
                Ok(t) => {
                    errors.push(format!(
                        "{}: Type mismatch in list item. Expected `{}` found `{}`",
                        expr.location(),
                        self.system.typename(interior_type),
                        self.system.typename(t)
                    ));
                }
                Err(mut s) => errors.append(&mut s),
            }
        }

        if errors.len() != 0 {
            Err(errors)
        } else {
            Ok(self.system.list_type(interior_type))
        }
    }

    fn index(&mut self, i: &mut IndexExpr) -> Result<Type, TypeError> {
        let obj_type = self.expr(i.obj.as_mut())?;
        // in the future there may be objects that are indexable so there will be an addition check
        let return_type = match self.system.underlying_type(obj_type) {
            None => {
                return Err(vec![format!(
                    "{}: Cannot index into type `{}`",
                    i.location,
                    self.system.typename(obj_type)
                )])
            }
            Some(t) => t,
        };
        if i.args.len() != 1 {
            return Err(vec![format!(
                "{}: Index expression requires 1 argument of type `int`",
                i.location,
            )]);
        }
        let idx_type = self.expr(&mut i.args[0])?;
        if idx_type != INT {
            return Err(vec![format!(
                "{}: Expected `int` found `{}`",
                i.args[0].location(),
                self.system.typename(idx_type)
            )]);
        }
        Ok(return_type)
    }
}
