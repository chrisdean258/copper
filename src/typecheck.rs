use crate::location::Location;
use crate::parser::ParseTree;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::*;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TypeChecker {
    pub system: TypeSystem,
    scopes: Vec<Rc<RefCell<HashMap<String, (Type, usize)>>>>,
    type_to_func: HashMap<Type, Function>,
    type_to_lambda: HashMap<Type, Lambda>,
    allow_insert: Option<Type>,
    lambda_args: Vec<Vec<Type>>,
    location: Option<Location>,
    globals: usize,
}

type TypeError = Vec<String>;
macro_rules! check_err {
    ( $name:ident ) => {
        if $name.len() > 0 {
            return Err($name);
        }
    };
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut rv = Self {
            system: TypeSystem::new(),
            scopes: Vec::new(),
            type_to_func: HashMap::new(),
            type_to_lambda: HashMap::new(),
            allow_insert: None,
            lambda_args: Vec::new(),
            location: None,
            globals: 0,
        };
        rv.openscope();
        rv
    }

    fn errmsg(&self, msg: String) -> String {
        assert!(self.location.is_some());
        format!("{}: {}", self.location.clone().unwrap(), msg)
    }

    fn error(&self, msg: String) -> TypeError {
        vec![self.errmsg(msg)]
    }

    pub fn typecheck(&mut self, p: &mut ParseTree) -> Result<(), String> {
        let mut results = Vec::new();
        for statement in p.statements.iter_mut() {
            match self.statement(statement) {
                Ok(_) => (),
                Err(mut s) => results.append(&mut s),
            }
        }
        self.globals += self.scopes[0].borrow().len();
        p.globals = Some(self.scopes[0].borrow().len());
        if results.len() > 0 {
            Err(results.join("\n"))
        } else {
            Ok(())
        }
    }

    fn lookup_scope(&mut self, name: &str) -> Option<(Type, usize)> {
        for scope in self.scopes.iter().rev() {
            match scope.borrow().get(name) {
                Some((t, p)) => return Some((*t, *p)),
                None => (),
            }
        }
        None
    }

    fn insert_scope(&mut self, name: &str, t: Type) -> usize {
        let mut scope = self.scopes.last().unwrap().borrow_mut();
        let place = scope.len();
        scope.insert(String::from(name), (t, place));
        place
    }

    fn openscope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
    }

    fn closescope(&mut self) -> usize {
        assert!(self.scopes.len() > 0);
        let rv = self.scopes.last().unwrap().borrow().len();
        self.scopes.pop();
        rv
    }

    fn statement(&mut self, s: &mut Statement) -> Result<Type, TypeError> {
        match s {
            Statement::Expr(e) => self.expr(e),
            Statement::ClassDecl(c) => todo!("{:?}", c),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(_) => Ok(UNIT),
            Statement::Break(_) => Ok(UNIT),
            Statement::Return(r) => todo!("{:?}", r),
        }
    }

    fn expr(&mut self, e: &mut Expression) -> Result<Type, TypeError> {
        self.location = Some(e.location.clone());
        let rv = match &mut e.etype {
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
            ExpressionType::DottedLookup(d) => todo!("{:?}", d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
        }?;
        e.derived_type = Some(rv);
        Ok(rv)
    }

    fn binop(&mut self, b: &mut BinOp) -> Result<Type, TypeError> {
        let ltype = self.expr(b.lhs.as_mut())?;
        let rtype = self.expr(b.rhs.as_mut())?;
        let rv = self.system.lookup_binop(b.op, ltype, rtype).ok_or(
            self.error(format!("Cannot apply binary operation `{}` {} `{}`. No operation has been defined between these types",
                    self.system.typename(ltype), b.op, self.system.typename(rtype))))?;
        Ok(rv)
    }

    fn preunop(&mut self, p: &mut PreUnOp) -> Result<Type, TypeError> {
        let rhstype = self.expr(p.rhs.as_mut())?;
        let rv = self
            .system
            .lookup_preunop(p.op, rhstype)
            .ok_or(self.error(format!(
                "Cannot apply unary operation `{}` to `{}`.",
                p.op,
                self.system.typename(rhstype)
            )))?;
        Ok(rv)
    }

    fn postunop(&mut self, p: &mut PostUnOp) -> Result<Type, TypeError> {
        let lhstype = self.expr(p.lhs.as_mut())?;
        let rv = self
            .system
            .lookup_postunop(p.op, lhstype)
            .ok_or(self.error(format!(
                "Cannot apply unary operation `{}` to `{}`.",
                p.op,
                self.system.typename(lhstype)
            )))?;
        Ok(rv)
    }

    fn refexpr(&mut self, r: &mut RefExpr) -> Result<Type, TypeError> {
        match self.lookup_scope(&r.name) {
            Some((t, p)) => {
                r.place = Some(p);
                Ok(t)
            }
            None if self.allow_insert.is_some() => {
                let typ = self.allow_insert.unwrap();
                let spot = self.insert_scope(&r.name, typ);
                r.place = Some(spot);
                Ok(typ)
            }
            None => Err(self.error(format!("`{}` no such variable in scope", r.name))),
        }
    }

    fn assignment(&mut self, a: &mut AssignExpr) -> Result<Type, TypeError> {
        if !a.lhs.is_lval() {
            return Err(self.error("lhs of assignment is not assignable".to_string()));
        }
        let rhstype = self.expr(a.rhs.as_mut())?;
        if rhstype == UNIT {
            return Err(self.error("Cannot assign unit value".to_string()));
        }
        self.allow_insert = Some(rhstype);
        let lhstype = self.expr(a.lhs.as_mut())?;
        self.allow_insert = None;
        self.system
            .lookup_assign(a.op, lhstype, rhstype)
            .ok_or(self.error(format!(
                "Cannot assign type `{}` {} `{}`",
                self.system.typename(lhstype),
                a.op,
                self.system.typename(rhstype)
            )))
    }

    fn immediate(&mut self, i: &mut Immediate) -> Result<Type, TypeError> {
        Ok(match i.value {
            Value::Null => typesystem::NULL,
            Value::Bool(_) => typesystem::BOOL,
            Value::Char(_) => typesystem::CHAR,
            Value::Int(_) => typesystem::INT,
            Value::Float(_) => typesystem::FLOAT,
            _ => unreachable!(),
            // Value::Str(_) => typesystem::STR,
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
        check_err!(errors);
        Ok(return_type)
    }

    fn whileexpr(&mut self, w: &mut While) -> Result<Type, TypeError> {
        let mut errors = Vec::new();
        match self.expr(w.condition.as_mut()) {
            Ok(t) if t == BOOL => (),
            Ok(t) => errors.push(format!(
                "{}: while loop conditionals must be `bool` not `{}`",
                w.condition.as_mut().location,
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }
        match self.expr(w.body.as_mut()) {
            Ok(_) => (), // This is currently discarded but would be used with `break`
            Err(mut s) => errors.append(&mut s),
        }

        check_err!(errors);
        Ok(UNIT)
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
            Ok(t) if t == UNKNOWN_RETURN => (),
            Ok(t) => errors.push(format!(
                "{}: if conditionals must be `bool` not `{}`",
                i.condition.as_mut().location,
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }

        match self.expr(i.body.as_mut()) {
            Ok(t) => rv = t,
            Err(mut s) => errors.append(&mut s),
        }

        for body in i.and_bodies.iter_mut() {
            match self.ifexpr_int(&mut body.0, true) {
                Ok(t) if t == rv => (),
                Ok(t) if t == UNKNOWN_RETURN => (),
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            }
        }

        match &mut i.else_body {
            Some(b) => match self.expr(b.as_mut()) {
                Ok(t) if t == rv => (),
                Ok(t) if t == UNKNOWN_RETURN => (),
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            },
            None => return_unit = !is_and_if,
        }

        check_err!(errors);
        if return_unit {
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
                        expr.location,
                        self.system.typename(interior_type),
                        self.system.typename(t)
                    ));
                }
                Err(mut s) => errors.append(&mut s),
            }
        }

        check_err!(errors);
        Ok(self.system.list_type(interior_type))
    }

    fn index(&mut self, i: &mut IndexExpr) -> Result<Type, TypeError> {
        let obj_type = self.expr(i.obj.as_mut())?;
        // in the future there may be objects that are indexable so there will be an addition check
        let return_type = match self.system.underlying_type(obj_type) {
            None => {
                return Err(self.error(format!(
                    "Cannot index into type `{}`",
                    self.system.typename(obj_type)
                )))
            }
            Some(t) => t,
        };
        if i.args.len() != 1 {
            return Err(self.error(format!(
                "Index expression requires 1 argument of type `int`",
            )));
        }
        let idx_type = self.expr(&mut i.args[0])?;
        if idx_type != INT {
            return Err(vec![format!(
                "{}: Expected `int` found `{}`",
                i.args[0].location,
                self.system.typename(idx_type)
            )]);
        }
        Ok(return_type)
    }

    fn function(&mut self, f: &mut Function) -> Result<Type, TypeError> {
        let typ = match f.name.clone() {
            Some(s) => {
                let rv = self.system.function_type(s.clone());
                self.insert_scope(&s, rv);
                rv
            }
            None => self
                .system
                .function_type("<anonymous function>".to_string()),
        };
        self.type_to_func.insert(typ, f.clone());
        Ok(typ)
    }

    fn lambda(&mut self, l: &mut Lambda) -> Result<Type, TypeError> {
        let typ = self.system.function_type("<lambda>".to_string());
        self.type_to_lambda.insert(typ, l.clone());
        Ok(typ)
    }

    fn lambdaarg(&mut self, l: &mut LambdaArg) -> Result<Type, TypeError> {
        if self.lambda_args.len() == 0 {
            panic!("Trying to derive type of lambda arg in non lambda. This is a bug");
        }
        Ok(self.lambda_args.last().unwrap()[l.number])
    }

    fn call(&mut self, c: &mut CallExpr) -> Result<Type, TypeError> {
        // TODO: maybe used RC Refcell to store functions in map to prevent copying
        // Although this will be tricky to pull off with recursion and ownership
        let functype = self.expr(c.function.as_mut())?;
        let funcloc = c.function.location.clone();
        if !self.system.is_function(functype) {
            return Err(self.error("Trying to call value that is not callable".to_string()));
        }
        let mut errs: TypeError = Vec::new();
        let mut args = Vec::new();
        for arg in c.args.iter_mut() {
            match self.expr(arg) {
                Ok(t) => args.push(t),
                Err(mut s) => errs.append(&mut s),
            }
        }
        check_err!(errs);

        if let Some(t) = self.system.match_signature(functype, &args) {
            return Ok(t);
        }

        let mut function: Function = self.type_to_func[&functype].clone();
        let sig_handle = self.system.add_function_signature(
            functype,
            Signature {
                inputs: args.clone(),
                output: UNKNOWN_RETURN,
            },
        );

        if args.len() != function.argnames.len() {
            return Err(self.error(format!(
                "Trying to call function with {} args. Expected {}",
                args.len(),
                function.argnames.len()
            )));
        }

        self.openscope();
        for (typ, name) in args.iter().zip(function.argnames.iter()) {
            self.insert_scope(name, *typ);
        }

        let rv = match self.expr(function.body.as_mut()) {
            Ok(t) if t == UNKNOWN_RETURN => {
                return Err(vec![
                    format!("{}: Could not determine return type. This is probably an infinite recursion bug", funcloc),
                    self.errmsg("Originating with this function call".to_string())
                ]);
            }
            Ok(t) => t,
            Err(mut s) => {
                s.push(self.errmsg("Originating with this function call".to_string()));
                return Err(s);
            }
        };
        self.system.patch_signature_return(functype, sig_handle, rv);
        self.closescope();
        self.openscope();
        for (typ, name) in args.iter().zip(function.argnames.iter()) {
            self.insert_scope(name, *typ);
        }
        let rv = match self.expr(function.body.as_mut()) {
            Ok(t) if t == rv => t,
            Ok(t) => {
                return Err(vec![
                    format!(
                        "{}: Could not determine return type. Derived both `{}` and `{}` as return types.",
                        funcloc, self.system.typename(t), self.system.typename(rv)
                    ),
                    self.errmsg("Originating with this function call".to_string())
                ]);
            }
            Err(mut s) => {
                s.push(self.errmsg("Originating with this function call".to_string()));
                return Err(s);
            }
        };
        function.locals = Some(self.closescope());
        // c.function.derived_type = Some(self.system.func_type(Signature {
        // inputs: args,
        // output: rv,
        // }));
        Ok(rv)
    }
}
