use crate::code_emitter::CodeBuilder;
use crate::operation::Operation;
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
    pub code: CodeBuilder,
    scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,
    type_to_func: HashMap<Type, Function>,
    type_to_lambda: HashMap<Type, Lambda>,
    allow_insert: Option<Type>,
    lambda_args: Vec<Vec<Type>>,
}

#[allow(dead_code)]
pub fn typecheck(p: &ParseTree) -> Result<TypeChecker, String> {
    let mut tc = TypeChecker::new();
    tc.typecheck(p).map(|_| tc)
}

type TypeError = Vec<String>;

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            system: TypeSystem::new(),
            scopes: vec![Rc::new(RefCell::new(HashMap::new()))],
            type_to_func: HashMap::new(),
            type_to_lambda: HashMap::new(),
            allow_insert: None,
            lambda_args: Vec::new(),
            code: CodeBuilder::new("temp".to_string()),
        }
    }

    pub fn typecheck(&mut self, p: &ParseTree) -> Result<(), String> {
        let mut results = Vec::new();
        for statement in p.statements.iter() {
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

    fn openscope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
    }

    fn closescope(&mut self) {
        assert!(self.scopes.len() > 0);
        self.scopes.pop();
    }

    fn statement(&mut self, s: &Statement) -> Result<Type, TypeError> {
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

    fn expr(&mut self, e: &Expression) -> Result<Type, TypeError> {
        match e {
            Expression::While(w) => self.whileexpr(w),
            Expression::For(f) => self.forexpr(f),
            Expression::If(i) => self.ifexpr(i),
            Expression::CallExpr(c) => self.call(c),
            Expression::RefExpr(r) => self.refexpr(r),
            Expression::Immediate(i) => self.immediate(i),
            Expression::BlockExpr(b) => self.block(b),
            Expression::BinOp(b) => self.binop(b),
            Expression::PreUnOp(p) => self.preunop(p),
            Expression::PostUnOp(p) => self.postunop(p),
            Expression::AssignExpr(a) => self.assignment(a),
            Expression::Function(f) => self.function(f),
            Expression::Lambda(l) => self.lambda(l),
            Expression::List(l) => self.list(l),
            Expression::IndexExpr(i) => self.index(i),
            Expression::DottedLookup(d) => todo!("{:?}", d),
            Expression::LambdaArg(l) => self.lambdaarg(l),
        }
    }

    fn binop(&mut self, b: &BinOp) -> Result<Type, TypeError> {
        let ltype = self.expr(b.lhs.as_ref())?;
        let rtype = self.expr(b.rhs.as_ref())?;
        let rv = self.system.lookup_binop(b.op, ltype, rtype).ok_or(
            vec![format!("{}: cannot apply binary operation `{}` {} `{}`. No operation has been defined between these types",
                    b.location, self.system.typename(ltype), b.op, self.system.typename(rtype))])?;
        self.code.emit(b.op, vec![ltype, rtype], vec![]);
        Ok(rv)
    }

    fn preunop(&mut self, p: &PreUnOp) -> Result<Type, TypeError> {
        let rhstype = self.expr(p.rhs.as_ref())?;
        let rv = self
            .system
            .lookup_preunop(p.op, rhstype)
            .ok_or(vec![format!(
                "{}: cannot apply unary operation `{}` to `{}`.",
                p.location,
                p.op,
                self.system.typename(rhstype)
            )])?;
        self.code.emit(p.op, vec![rhstype], vec![]);
        Ok(rv)
    }

    fn postunop(&mut self, p: &PostUnOp) -> Result<Type, TypeError> {
        let lhstype = self.expr(p.lhs.as_ref())?;
        let rv = self
            .system
            .lookup_postunop(p.op, lhstype)
            .ok_or(vec![format!(
                "{}: cannot apply unary operation `{}` to `{}`.",
                p.location,
                p.op,
                self.system.typename(lhstype)
            )])?;
        self.code.emit(p.op, vec![lhstype], vec![]);
        Ok(rv)
    }

    fn refexpr(&mut self, r: &RefExpr) -> Result<Type, TypeError> {
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

    fn assignment(&mut self, a: &AssignExpr) -> Result<Type, TypeError> {
        if !a.lhs.is_lval() {
            return Err(vec![format!(
                "{}: lhs of assignment is not assignable",
                a.location
            )]);
        }
        let rhstype = self.expr(a.rhs.as_ref())?;
        if rhstype == UNIT {
            return Err(vec![format!("{}: cannot assign unit value", a.location)]);
        }
        self.allow_insert = Some(rhstype);
        let lhstype = self.expr(a.lhs.as_ref())?;
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

    fn immediate(&mut self, i: &Immediate) -> Result<Type, TypeError> {
        let rv = match i.value {
            Value::Null => typesystem::NULL,
            Value::Bool(_) => typesystem::BOOL,
            Value::Char(_) => typesystem::CHAR,
            Value::Int(_) => typesystem::INT,
            Value::Float(_) => typesystem::FLOAT,
            // Value::Str(_) => typesystem::STR,
        };
        self.code
            .emit(Operation::Push, vec![rv], vec![i.value.clone()]);
        Ok(rv)
    }

    fn block(&mut self, b: &BlockExpr) -> Result<Type, TypeError> {
        let mut errors = Vec::new();
        let mut return_type = UNIT;
        for statement in b.statements.iter() {
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

    fn whileexpr(&mut self, w: &While) -> Result<Type, TypeError> {
        let mut errors = Vec::new();
        match self.expr(w.condition.as_ref()) {
            Ok(t) if t == BOOL => (),
            Ok(t) => errors.push(format!(
                "{}: while loop conditionals must be `bool` not `{}`",
                w.condition.as_ref().location(),
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }
        match self.expr(w.body.as_ref()) {
            Ok(_) => (), // This is currently discarded but would be used with `break`
            Err(mut s) => errors.append(&mut s),
        }

        if errors.len() == 0 {
            Ok(UNIT)
        } else {
            Err(errors)
        }
    }

    fn forexpr(&mut self, _w: &For) -> Result<Type, TypeError> {
        todo!("Havent worked out the details of iteration yet")
    }

    fn ifexpr(&mut self, i: &If) -> Result<Type, TypeError> {
        self.ifexpr_int(i, false)
    }

    fn ifexpr_int(&mut self, i: &If, is_and_if: bool) -> Result<Type, TypeError> {
        let mut errors: TypeError = Vec::new();
        let mut rv = UNIT;
        let mut return_unit = false;
        match self.expr(i.condition.as_ref()) {
            Ok(t) if t == BOOL => (),
            Ok(t) if t == UNKNOWN_RETURN => (),
            Ok(t) => errors.push(format!(
                "{}: if conditionals must be `bool` not `{}`",
                i.condition.as_ref().location(),
                self.system.typename(t)
            )),
            Err(mut s) => errors.append(&mut s),
        }

        match self.expr(i.body.as_ref()) {
            Ok(t) => rv = t,
            Err(mut s) => errors.append(&mut s),
        }

        for body in i.and_bodies.iter() {
            match self.ifexpr(body) {
                Ok(t) if t == rv => (),
                Ok(t) if t == UNKNOWN_RETURN => (),
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            }
        }

        match &i.else_body {
            Some(b) => match self.expr(b.as_ref()) {
                Ok(t) if t == rv => (),
                Ok(t) if t == UNKNOWN_RETURN => (),
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
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

    fn list(&mut self, l: &List) -> Result<Type, TypeError> {
        let mut errors: TypeError = Vec::new();
        let mut interior_type = UNIT;
        for expr in l.exprs.iter() {
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

    fn index(&mut self, i: &IndexExpr) -> Result<Type, TypeError> {
        let obj_type = self.expr(i.obj.as_ref())?;
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
        let idx_type = self.expr(&i.args[0])?;
        if idx_type != INT {
            return Err(vec![format!(
                "{}: Expected `int` found `{}`",
                i.args[0].location(),
                self.system.typename(idx_type)
            )]);
        }
        Ok(return_type)
    }

    fn function(&mut self, f: &Function) -> Result<Type, TypeError> {
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

    fn lambda(&mut self, l: &Lambda) -> Result<Type, TypeError> {
        let typ = self.system.function_type("<lambda>".to_string());
        self.type_to_lambda.insert(typ, l.clone());
        Ok(typ)
    }

    fn lambdaarg(&mut self, l: &LambdaArg) -> Result<Type, TypeError> {
        if self.lambda_args.len() == 0 {
            panic!("Trying to derive type of lambda arg in non lambda. This is a bug");
        }
        Ok(self.lambda_args.last().unwrap()[l.number])
    }

    fn call(&mut self, c: &CallExpr) -> Result<Type, TypeError> {
        // TODO: maybe used RC Refcell to store functions in map to prevent copying
        // Although this will be tricky to pull off with recursion and ownership
        let functype = self.expr(c.function.as_ref())?;
        if !self.system.is_function(functype) {
            return Err(vec![format!(
                "{}: trying to call value that is not callable",
                c.location
            )]);
        }
        let mut errs: TypeError = Vec::new();
        let mut args = Vec::new();
        for arg in c.args.iter() {
            match self.expr(arg) {
                Ok(t) => args.push(t),
                Err(mut s) => errs.append(&mut s),
            }
        }

        if errs.len() > 0 {
            return Err(errs);
        }

        if let Some(t) = self.system.match_signature(functype, &args) {
            return Ok(t);
        }

        let function: Function = self.type_to_func[&functype].clone();
        let sig_handle = self.system.add_function_signature(
            functype,
            Signature {
                inputs: args.clone(),
                output: UNKNOWN_RETURN,
            },
        );

        self.openscope();
        if args.len() != function.argnames.len() {
            return Err(vec![format!(
                "{}: trying to call function with {} args. Expected {}",
                c.location,
                args.len(),
                function.argnames.len()
            )]);
        }

        for (typ, name) in args.iter().zip(function.argnames.iter()) {
            self.insert_scope(name, *typ);
        }

        let rv = match self.expr(function.body.as_ref()) {
            Ok(t) if t == UNKNOWN_RETURN => {
                return Err(vec![
                    format!("{}: Could not determine return type. This is probably an infinite recursion bug", function.location),
                    format!("{}: Originating with this function call", c.location),
                ]);
            }
            Ok(t) => t,
            Err(mut s) => {
                s.push(format!(
                    "{}: Originating with this function call",
                    c.location
                ));
                return Err(s);
            }
        };
        self.system.patch_signature_return(functype, sig_handle, rv);
        let rv = match self.expr(function.body.as_ref()) {
            Ok(t) if t == UNKNOWN_RETURN => {
                return Err(vec![
                    format!("{}: Could not determine return type. This is probably an infinite recursion bug", function.location),
                    format!("{}: Originating with this function call", c.location),
                ]);
            }
            Ok(t) if t == rv => t,
            Ok(t) => {
                return Err(vec![
                    format!(
                        "{}: Could not determine return type. Derived both `{}` and `{}` as return types.",
                        function.location, self.system.typename(t), self.system.typename(rv)
                    ),
                    format!("{}: Originating with this function call", c.location),
                ]);
            }
            Err(mut s) => {
                s.push(format!(
                    "{}: Originating with this function call",
                    c.location
                ));
                return Err(s);
            }
        };
        self.closescope();
        Ok(rv)
    }
}
