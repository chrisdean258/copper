use crate::{
    builtins::BuiltinFunction, location::Location, parser, parser::ParseTree, parser::*,
    typesystem, typesystem::*, value::Value,
};
use std::{cell::RefCell, collections::HashMap, mem::swap, rc::Rc};

pub struct TypeChecker {
    pub system: TypeSystem,
    scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,
    func_scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,
    class_scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,

    type_to_func: HashMap<Type, Rc<RefCell<parser::Function>>>,
    type_to_lambda: HashMap<Type, Rc<RefCell<Lambda>>>,
    type_to_class: HashMap<Type, Rc<RefCell<ClassDecl>>>,

    allow_insert: Option<Type>,
    lambda_args: Vec<Type>,
    location: Option<Location>,
    globals: usize,
    allow_raw_func: bool,
    func_returns: Vec<Option<Type>>,
    need_recheck: bool,
    repeated_arg: Option<(String, Type)>,
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
            func_scopes: Vec::new(),
            class_scopes: Vec::new(),
            type_to_func: HashMap::new(),
            type_to_lambda: HashMap::new(),
            type_to_class: HashMap::new(),
            allow_insert: None,
            lambda_args: Vec::new(),
            location: None,
            globals: 0,
            allow_raw_func: false,
            func_returns: Vec::new(),
            need_recheck: false,
            repeated_arg: None,
        };
        rv.openscope();
        for f in BuiltinFunction::get_table(&mut rv.system) {
            rv.insert_scope(&f.name, typesystem::BUILTIN_FUNCTION);
        }
        rv.openscope();
        rv
    }

    fn errmsg(&self, msg: String) -> String {
        debug_assert!(self.location.is_some());
        format!("{}: Type Error: {}", self.location.clone().unwrap(), msg)
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
        self.globals += self.scopes[1].borrow().len();
        p.globals = Some(self.scopes[1].borrow().len());
        if results.is_empty() {
            Ok(())
        } else {
            Err(results.join("\n"))
        }
    }

    fn scope_lookup_general(
        &self,
        name: &str,
        scopes: &[Rc<RefCell<HashMap<String, Type>>>],
    ) -> Option<Type> {
        for scope in scopes.iter().rev() {
            if let Some(t) = scope.borrow().get(name) {
                return Some(*t);
            }
        }
        None
    }

    fn scope_lookup(&self, name: &str) -> Option<Type> {
        self.scope_lookup_general(name, &self.scopes)
    }

    fn scope_lookup_func(&self, name: &str) -> Option<Type> {
        self.scope_lookup_general(name, &self.func_scopes)
    }

    fn scope_lookup_class(&self, name: &str) -> Option<Type> {
        self.scope_lookup_general(name, &self.class_scopes)
    }

    fn insert_scope(&mut self, name: &str, t: Type) -> usize {
        let mut scope = self.scopes.last().unwrap().borrow_mut();
        let place = scope.len();
        scope.insert(String::from(name), t);
        place
    }

    fn insert_func_scope(&mut self, name: &str, t: Type) -> usize {
        let mut scope = self.func_scopes.last().unwrap().borrow_mut();
        let place = scope.len();
        scope.insert(String::from(name), t);
        place
    }

    fn insert_class_scope(&mut self, name: &str, t: Type) -> usize {
        let mut scope = self.class_scopes.last().unwrap().borrow_mut();
        let place = scope.len();
        scope.insert(String::from(name), t);
        place
    }

    fn func_mangle_name_scope_insert(&mut self, name: &str, sig: Signature) {
        let mut idx = None;
        for (i, scope) in self.func_scopes.iter().rev().enumerate() {
            if scope.borrow().get(name).is_some() {
                idx = Some(self.scopes.len() - i - 1);
                break;
            }
        }
        let idx = idx.expect("If not this is bug");
        let typ = self.system.func_type_from_sig(&sig);
        self.scopes[idx]
            .borrow_mut()
            .insert(self.system.mangle(name, &sig), typ);
    }

    fn openscope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
        self.func_scopes.push(Rc::new(RefCell::new(HashMap::new())));
        self.class_scopes
            .push(Rc::new(RefCell::new(HashMap::new())));
    }

    fn closescope(&mut self) -> usize {
        self.func_scopes.pop();
        self.class_scopes.pop();
        debug_assert!(!self.scopes.is_empty());
        let rv = self.scopes.last().unwrap().borrow().len();
        self.scopes.pop();
        rv
    }

    fn statement(&mut self, s: &mut Statement) -> Result<Type, TypeError> {
        match s {
            Statement::Expr(e) => self.expr(e),
            Statement::ClassDecl(c) => self.classdecl(c.clone()),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(_) => Ok(UNIT),
            Statement::Break(_) => Ok(UNIT),
            Statement::Return(r) => self.return_(r),
        }
    }

    fn return_(&mut self, r: &mut Return) -> Result<Type, TypeError> {
        let rv = match &mut r.body {
            Some(e) => self.expr(e.as_mut())?,
            None => UNIT,
        };

        // This is an Option<Option<Type>>
        // if outer option is none then we are at global scope
        // if inner is None we havent derived a type yet
        match self.func_returns.last() {
            None if rv == UNIT || rv == INT => (),
            None => {
                return Err(self.error(format!(
                    "Cannot return type {} from non function",
                    self.system.typename(rv)
                )))
            }
            Some(None) => {
                let last_idx = self.func_returns.len() - 1;
                self.func_returns[last_idx] = Some(rv);
            }
            Some(Some(v)) if *v == rv => (),
            Some(Some(v)) => {
                return Err(self.error(format!(
                    "Cannot return type {} from function. Return type already encountered is {}",
                    self.system.typename(rv),
                    self.system.typename(*v),
                )))
            }
        }

        Ok(rv)
    }

    fn classdecl(&mut self, c: Rc<RefCell<ClassDecl>>) -> Result<Type, TypeError> {
        let cc = c.borrow();
        let typ = self
            .system
            .class_type(cc.name.clone(), c.borrow().fields.len());
        self.insert_class_scope(&cc.name, typ);
        drop(cc);
        self.type_to_class.insert(typ, c);
        Ok(typ)
    }

    fn expr(&mut self, e: &mut Expression) -> Result<Type, TypeError> {
        self.location = Some(e.location.clone());
        if let Some(typ) = e.derived_type {
            if typ != UNKNOWN_RETURN {
                return Ok(typ);
            }
        }
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
            ExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
            ExpressionType::Str(s) => self.string(s),
            ExpressionType::FuncRefExpr(r) => self.funcrefexpr(r),
            ExpressionType::RepeatedArg => self.repeated_arg(),
            ExpressionType::Null => self.null(),
        }?;
        e.derived_type = Some(rv);
        if rv == UNKNOWN_RETURN {
            self.need_recheck = true;
        }
        Ok(rv)
    }

    fn null(&mut self) -> Result<Type, TypeError> {
        Ok(NULL)
    }

    fn repeated_arg(&mut self) -> Result<Type, TypeError> {
        Ok(self.repeated_arg.clone().unwrap().1)
    }

    fn binop(&mut self, b: &mut BinOp) -> Result<Type, TypeError> {
        let mut ltype = self.expr(b.lhs.as_mut())?;
        let mut rtype = self.expr(b.rhs.as_mut())?;
        if self.system.is_option(ltype) && rtype == NULL {
            b.rhs.as_mut().derived_type = Some(ltype);
            rtype = ltype;
        }
        if self.system.is_option(rtype) && ltype == NULL {
            b.lhs.as_mut().derived_type = Some(rtype);
            ltype = rtype;
        }
        let rv = self.system.lookup_binop(b.op, ltype, rtype).ok_or_else(||
            self.error(format!(
                    "Cannot apply binary operation `{}` {} `{}`. No operation has been defined between these types",
                    self.system.typename(ltype),
                    b.op,
                    self.system.typename(rtype)
               ))
        )?;

        Ok(rv)
    }

    fn preunop(&mut self, p: &mut PreUnOp) -> Result<Type, TypeError> {
        let rhstype = self.expr(p.rhs.as_mut())?;
        let rv = self.system.lookup_preunop(p.op, rhstype).ok_or_else(|| {
            self.error(format!(
                "Cannot apply unary operation `{}` to `{}`.",
                p.op,
                self.system.typename(rhstype)
            ))
        })?;
        Ok(rv)
    }

    fn postunop(&mut self, p: &mut PostUnOp) -> Result<Type, TypeError> {
        let lhstype = self.expr(p.lhs.as_mut())?;
        let rv = self.system.lookup_postunop(p.op, lhstype).ok_or_else(|| {
            self.error(format!(
                "Cannot apply unary operation `{}` to `{}`.",
                p.op,
                self.system.typename(lhstype)
            ))
        })?;
        Ok(rv)
    }

    fn refexpr(&mut self, r: &mut RefExpr) -> Result<Type, TypeError> {
        match self.scope_lookup(&r.name) {
            Some(t) => return Ok(t),
            None if self.allow_insert.is_some() => {
                let typ = self.allow_insert.unwrap();
                r.is_decl = true;
                self.insert_scope(&r.name, typ);
                return Ok(typ);
            }
            None => (),
        }
        if let Some(t) = self.scope_lookup_class(&r.name) {
            return Ok(t);
        }
        match self.scope_lookup_func(&r.name) {
            Some(t) if self.allow_raw_func => Ok(t),
            Some(_) => Err(self.error(format!(
                "`{}` is a function whose types cannot be determined. Try wrapping it in a lambda",
                r.name
            ))),
            None => Err(self.error(format!("`{}` no such variable in scope", r.name))),
        }
    }

    fn funcrefexpr(&mut self, r: &mut FuncRefExpr) -> Result<Type, TypeError> {
        let name = self.system.mangle(&r.name, &r.sig);
        match self.scope_lookup(&name) {
            Some(t) => Ok(t),
            None => unreachable!("Could not find {:?} in scope", r),
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
            .ok_or_else(|| {
                self.error(format!(
                    "Cannot assign type `{}` {} `{}`",
                    self.system.typename(lhstype),
                    a.op,
                    self.system.typename(rhstype)
                ))
            })
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
        let mut make_option = false;
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
                Ok(NULL) => make_option = true,
                Ok(t) if rv == NULL => {
                    rv = t;
                    make_option = true;
                }
                Ok(t) if t == UNKNOWN_RETURN => (),
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            }
        }

        match &mut i.else_body {
            Some(b) => match self.expr(b.as_mut()) {
                Ok(t) if t == rv => (),
                Ok(UNKNOWN_RETURN) => (),
                Ok(NULL) => make_option = true,
                Ok(t) if rv == NULL => {
                    rv = t;
                    make_option = true;
                }
                Ok(t) if rv == UNKNOWN_RETURN => rv = t,
                Ok(_) => return_unit = true,
                Err(mut s) => errors.append(&mut s),
            },
            None => return_unit = !is_and_if,
        }

        check_err!(errors);
        if return_unit {
            return Ok(UNIT);
        }
        if make_option {
            rv = self.system.option_type(rv);
            i.makes_option = Some(rv);
        }
        Ok(rv)
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
            return Err(
                self.error("Index expression requires 1 argument of type `int`".to_string())
            );
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

    fn function(&mut self, f: &mut Rc<RefCell<parser::Function>>) -> Result<Type, TypeError> {
        let typ = match f.borrow().name.clone() {
            Some(s) => {
                let rv = self.system.function_type(s.clone());
                self.insert_func_scope(&s, rv);
                rv
            }
            None => self
                .system
                .function_type("<anonymous function>".to_string()),
        };
        self.type_to_func.insert(typ, f.clone());
        Ok(typ)
    }

    fn lambda(&mut self, l: &mut Rc<RefCell<Lambda>>) -> Result<Type, TypeError> {
        let typ = self.system.function_type("<lambda>".to_string());
        self.type_to_lambda.insert(typ, l.clone());
        Ok(typ)
    }

    fn lambdaarg(&mut self, l: &mut LambdaArg) -> Result<Type, TypeError> {
        debug_assert!(
            !self.lambda_args.is_empty(),
            "Trying to derive type of lambda arg in non lambda. This is a bug"
        );
        Ok(self.lambda_args[l.number])
    }

    fn string(&mut self, _s: &mut Str) -> Result<Type, TypeError> {
        Ok(STR)
    }

    fn call(&mut self, c: &mut CallExpr) -> Result<Type, TypeError> {
        macro_rules! mangle {
            ($c:ident, $args:ident, $out:expr) => {
                match &$c.function.as_ref().etype {
                    ExpressionType::RefExpr(r) => {
                        if self.scope_lookup_func(&r.name).is_some() {
                            $c.function.as_mut().etype = ExpressionType::FuncRefExpr(FuncRefExpr {
                                name: r.name.clone(),
                                sig: Signature::new($args.clone(), None, $out),
                            })
                        }
                    }
                    _ => (),
                }
            };
        }
        let save = self.allow_raw_func;
        self.allow_raw_func = true;
        let mut functype = self.expr(c.function.as_mut())?;
        self.allow_raw_func = save;
        let mut funcloc = c.function.location.clone();
        let mut args = Vec::new();

        let mut override_return = None;
        if self.system.is_class(functype) {
            let classdecl = self.type_to_class.get_mut(&functype).unwrap().clone();
            c.is_init = Some(classdecl.borrow().fields.len());
            match classdecl.borrow_mut().methods.get_mut("__init__") {
                Some(init) => {
                    match &mut init.etype {
                        ExpressionType::Function(f) => f.borrow_mut().is_init = c.is_init,
                        _ => unreachable!(),
                    }
                    funcloc = init.location.clone();
                    let unresolved = self.system.class_new_unresolved(functype);
                    args.push(unresolved);
                    functype = self.expr(init)?;
                    override_return = Some(unresolved);
                }
                _ => {
                    return Err(vec![funcloc.errfmt(format_args!(
                        "class {} contains no __init__ method",
                        self.system.typename(functype)
                    ))]);
                }
            };
        }

        if !self.system.is_function(functype) {
            return Err(self.error("Trying to call value that is not callable".to_string()));
        }

        let mut errs: TypeError = Vec::new();
        for arg in c.args.iter_mut() {
            match self.expr(arg) {
                Ok(t) => args.push(t),
                Err(mut s) => errs.append(&mut s),
            }
        }
        check_err!(errs);

        if functype == BUILTIN_FUNCTION {
            let funcname = match &c.function.as_ref().etype {
                ExpressionType::RefExpr(r) => r.name.clone(),
                _ => {
                    return Err(
                        self.error("Cannot indirectly call call Builtin functions yet".to_string())
                    )
                }
            };
            let builtins = BuiltinFunction::get_hashmap(&mut self.system);
            let func = builtins.get(&funcname).unwrap();
            return func
                .signature
                .output_type_if_match(&self.system, &args)
                .map_err(|x| vec![x]);
        }

        if let Some(sig) = self.system.get_resolved_func_sig_can_fail(functype) {
            mangle!(c, args, sig.output);
            return Ok(sig.output);
        }

        let rv = match self.type_to_func.get_mut(&functype) {
            Some(func) => {
                let func = func.clone();
                self.call_function(func, functype, args.clone(), funcloc, c)?
            }
            None => {
                let lambda = self.type_to_lambda.get_mut(&functype).unwrap().clone();
                self.call_lambda(lambda, functype, args.clone(), funcloc, c)?
            }
        };
        mangle!(c, args, rv);
        Ok(override_return.unwrap_or(rv)) // override return is for constructors
    }

    fn call_function(
        &mut self,
        function: Rc<RefCell<parser::Function>>,
        functype: Type,
        args: Vec<Type>,
        funcloc: Location,
        c: &mut CallExpr,
    ) -> Result<Type, TypeError> {
        match function.try_borrow_mut() {
            Ok(_) => (),
            Err(_) => return Ok(UNKNOWN_RETURN),
        }
        let rv = self.call_function_int(function.clone(), functype, args, funcloc, c)?;
        if let Some(name) = &function.borrow().name {
            let func_sig = self
                .system
                .get_resolved_func_sig(c.function.derived_type.unwrap());
            self.func_mangle_name_scope_insert(name, func_sig.clone());
            match &c.function.etype {
                ExpressionType::RefExpr(r) => {
                    c.function.etype = ExpressionType::FuncRefExpr(FuncRefExpr {
                        name: r.name.clone(),
                        sig: func_sig,
                    })
                }
                ExpressionType::DottedLookup(_d) => {
                    todo!()
                }
                _ => (),
            }
        }
        Ok(rv)
    }

    fn call_function_single_check(
        &mut self,
        function: &Rc<RefCell<parser::Function>>,
        args: &[Type],
        funcloc: &Location,
    ) -> Result<(Type, usize), TypeError> {
        self.openscope();
        if let Some(name) = function.borrow().repeated.clone() {
            if args.len() > function.borrow().argnames.len() {
                self.repeated_arg = Some((name, *args.last().unwrap()));
            } else {
                self.repeated_arg = Some((name, 0));
            }
        }
        for (typ, name) in args.iter().zip(function.borrow().argnames.iter()) {
            self.insert_scope(name, *typ);
        }
        let rv = self.call_single_check(function.borrow_mut().body.as_mut(), funcloc)?;
        Ok((rv, self.closescope()))
    }

    fn call_single_check(
        &mut self,
        function: &mut Expression,
        funcloc: &Location,
    ) -> Result<Type, TypeError> {
        self.func_returns.push(None);
        let save_repeated = self.repeated_arg.clone();
        let rv = match self.expr(function) {
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
        self.repeated_arg = save_repeated;
        let derived = self.func_returns.pop().unwrap();
        match derived {
            Some(t) if t != rv => {
                return Err(self.error(format!(
                    "Cannot return type {} from function. Return type already encountered is {}",
                    self.system.typename(rv),
                    self.system.typename(t),
                )))
            }
            _ => (),
        }
        Ok(rv)
    }

    fn call_function_int(
        &mut self,
        function: Rc<RefCell<parser::Function>>,
        functype: Type,
        mut args: Vec<Type>,
        funcloc: Location,
        c: &mut CallExpr,
    ) -> Result<Type, TypeError> {
        let num_default_needed = if function.borrow().repeated.is_some() {
            0
        } else {
            let argnamelen = function.borrow().argnames.len();
            if argnamelen < args.len() {
                return Err(self.error(format!(
                    "Too many arguments. Expected {} found {}",
                    argnamelen,
                    args.len()
                )));
            }
            argnamelen - args.len()
        };

        if num_default_needed > function.borrow().default_args.len() {
            return Err(self.error(format!(
                "Trying to call function with {} args + {} default args. Expected {} total",
                args.len(),
                function.borrow().default_args.len(),
                function.borrow().argnames.len(),
            )));
        }

        let first_default = function.borrow().default_args.len() - num_default_needed;

        for i in 0..num_default_needed {
            let mut expr = function.borrow().default_args[i + first_default].clone();
            let typ = self.expr(&mut expr)?;
            c.args.push(expr);
            args.push(typ);
        }

        //check if we have already memoized a matching signature
        if let Some(t) = self.system.function_get_resolved(functype, &args) {
            c.function.as_mut().derived_type = Some(t);
            return Ok(t);
        }

        let save = self.need_recheck;
        self.need_recheck = false;
        let (rv, num_locals) = self.call_function_single_check(&function, &args, &funcloc)?;
        function.borrow_mut().locals = Some(num_locals);
        let func_sig = if function.borrow().repeated.is_none() {
            Signature::new(args.clone(), None, rv)
        } else {
            let num = function.borrow().argnames.len();
            Signature::new(
                args.iter().take(num).cloned().collect(),
                args.last().cloned(),
                rv,
            )
        };
        let rftyp = self.system.add_function_signature(functype, func_sig);
        c.function.as_mut().derived_type = Some(rftyp);

        if self.need_recheck {
            let (new_rv, num_locals) =
                self.call_function_single_check(&function, &args, &funcloc)?;
            if new_rv != rv {
                return Err(vec![format!(
                            "{}: Could not determine return type. Derived both `{}` and `{}` as return types.",
                            funcloc, self.system.typename(new_rv), self.system.typename(rv)),
                        self.errmsg("Originating with this function call".to_string()) ]);
            }
            debug_assert_eq!(function.borrow_mut().locals.unwrap(), num_locals);
        }
        self.need_recheck = save;
        Ok(rv)
    }

    fn call_lambda(
        &mut self,
        lambda: Rc<RefCell<Lambda>>,
        functype: Type,
        mut args: Vec<Type>,
        funcloc: Location,
        c: &mut CallExpr,
    ) -> Result<Type, TypeError> {
        if let Some(t) = self.system.match_signature(functype, &args) {
            return Ok(t);
        }

        swap(&mut self.lambda_args, &mut args);

        self.openscope();
        self.func_returns.push(None);
        let rv = self.call_single_check(lambda.borrow_mut().body.as_mut(), &funcloc)?;
        lambda.borrow_mut().locals = Some(self.closescope() + args.len());
        swap(&mut self.lambda_args, &mut args);
        let rftyp = self
            .system
            .add_function_signature(functype, Signature::new(args, None, rv));
        c.function.as_mut().derived_type = Some(rftyp);
        Ok(rv)
    }

    fn dotted_lookup(&mut self, d: &mut DottedLookup) -> Result<Type, TypeError> {
        let lhs_typ = self.expr(d.lhs.as_mut())?;
        let classdecltyp = self.system.class_underlying(lhs_typ);
        let classdecl = self.type_to_class.get(&classdecltyp).unwrap();
        let idx = *classdecl.borrow().fields.get(&d.rhs).unwrap();
        d.index = Some(idx);
        let field_type = self.system.class_query_field(lhs_typ, idx);
        match (field_type, self.allow_insert) {
            (Some(ft), _) => Ok(ft),
            (None, Some(ai)) => {
                self.system.set_field_type(lhs_typ, idx, ai);
                Ok(ai)
            }
            (None, None) => {
                Err(self.error("Trying to assign with an uninitialized type".to_string()))
            }
        }
    }
}
