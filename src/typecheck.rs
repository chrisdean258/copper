use crate::{
    builtins::BuiltinFunction, location::Location, operation::Operation, parser, parser::ParseTree,
    parser::*, typesystem, typesystem::*, value::Value,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem::{swap, take},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Error {
    loc: Location,
    err: ErrorType,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    CannotAssignType(String, Operation, String),
    CannotAssignUnit,
    CannotDeriveReturnType,
    CannotIndexType(String),
    CannotReturnTypeFromNonFunction(String),
    ClassHasNoFieldOrMethod(String, String),
    ClassLacksInit(String),
    EmptyBlock,
    FuncTypeUnknown(String),
    IfConditionNotBool(String),
    IndexNotInt,
    ListTypeMismatch(String, String),
    LoopConditionNotBool(String),
    MismatchedReturnTypes(String, String),
    NoDefinedBinOp(String, Operation, String),
    NoDefinedUnOp(String, Operation),
    NoSuchNameInScope(String),
    NotAssignable,
    Originating,
    UninitializedAssignment,
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_fmt(format_args!("{}: {}", self.loc, self.err))
    }
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::CannotAssignType(t1, op, t2) => f.write_fmt(format_args!( "Cannot assign `{t1}` {op} `{t2}`",)),
            Self::CannotAssignUnit => f.write_str("Cannot assign unit value"),
            Self::CannotIndexType(t) => f.write_fmt(format_args!( "Cannot index into type `{t}`")),
            Self::CannotReturnTypeFromNonFunction(t) => f.write_fmt(format_args!( "Cannot return type `{t}` from outside a function")),
            Self::ClassHasNoFieldOrMethod(t, fm) => f.write_fmt(format_args!( "Class `{t}` has no field or method `{fm}`",)),
            Self::ClassLacksInit(t) => f.write_fmt(format_args!( "class `{t}` lacks the `__init__` method and cannot be constructed")),
            Self::EmptyBlock => f.write_str("Empty Block Expressions are not allowed (yet)"),
            Self::FuncTypeUnknown(func) => f.write_fmt(format_args!( "`{func}` is a function whose types cannot be determined. Try wrapping it in a lambda",)),
            Self::IfConditionNotBool(t) => f.write_str("if condition must be bool. This one is `{t}`"),
            Self::IndexNotInt => f.write_str( "Index expression requires 1 argument of type `int`"),
            Self::ListTypeMismatch(t1, t2) => f.write_fmt(format_args!( "List established to contain type `{t1}` but this element was of type `{t2}`",)),
            Self::LoopConditionNotBool(t) => f.write_str("While loop condition must be bool. This one is `{t}`"),
            Self::MismatchedReturnTypes(t1, t2) => f.write_fmt(format_args!( "Cannot return type `{t1}` from function. Return type already encountered is `{t2}`",)),
            Self::NoDefinedBinOp(t1, op, t2) => f.write_fmt(format_args!( "Cannot apply binary operation `{t1}` {op} `{t2}`. No operation has been defined between these types",)),
            Self::NoDefinedUnOp(t, op) => f.write_fmt(format_args!( "Cannot apply binary operation {op} to `{t}`. No operation has been defined",)),
            Self::NoSuchNameInScope(s) => f.write_fmt(format_args!( "`no name `{s}` in scope",)),
            Self::NotAssignable => f.write_str("LHS of assignment is not assignable"),
            Self::Originating => f.write_str("Originating here"),
            Self::UninitializedAssignment => f.write_str("Trying to assign to variable with uninitialized type. This might be a bug"),
        }
    }
}

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
    errors: Vec<Error>,
}

#[derive(Debug, Clone)]
pub struct TypedParseTree {
    pub statements: Vec<TypedStatement>,
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Expr(TypedExpression),
    ClassDecl(Rc<RefCell<ClassDecl>>),
    Import(Import),
    FromImport(FromImport),
    Break,
    Continue,
    Return(TypedReturn),
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub etype: TypedExpressionType,
    pub location: Location,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum TypedExpressionType {
    While(TypedWhile),
    For(For),
    If(TypedIf),
    CallExpr(TypedCallExpr),
    InitCallExpr(TypedInitCallExpr),
    MethodCallExpr(TypedMethodCallExpr),
    PossibleMethodCall(PossibleMethodCall),
    VarRefExpr(TypedVarRefExpr),
    ClassRefExpr(TypedClassRefExpr),
    FuncRefExpr(TypedFuncRefExpr),
    Immediate(TypedImmediate),
    BlockExpr(TypedBlockExpr),
    BinOp(TypedBinOp),
    PreUnOp(TypedPreUnOp),
    PostUnOp(TypedPostUnOp),
    AssignExpr(TypedAssignExpr),
    // Functions/lambdas fundamentally don't have types until they are called
    // So this requires looking in the scope table
    Function,
    Lambda,
    List(TypedList),
    Str(TypedStr),
    IndexExpr(TypedIndexExpr),
    DottedLookup(TypedDottedLookup),
    LambdaArg(TypedLambdaArg),
    // FuncRefExpr(FuncRefExpr),
    RepeatedArg,
    Null,
    UnknownReturn,
}

#[derive(Debug, Clone)]
struct TypedReturn {
    body: Option<TypedExpression>,
}

#[derive(Debug, Clone)]
struct TypedBinOp {
    pub lhs: Box<TypedExpression>,
    pub op: Operation,
    pub rhs: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
struct TypedPreUnOp {
    pub op: Operation,
    pub rhs: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
struct TypedPostUnOp {
    pub lhs: Box<TypedExpression>,
    pub op: Operation,
}

#[derive(Debug, Clone)]
struct TypedVarRefExpr {
    pub name: String,
    pub is_decl: bool,
}

#[derive(Debug, Clone)]
struct TypedClassRefExpr {
    pub name: String,
}

#[derive(Debug, Clone)]
struct TypedFuncRefExpr {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypedAssignExpr {
    pub lhs: Box<TypedExpression>,
    pub op: Operation,
    pub rhs: Box<TypedExpression>,
    pub allow_decl: bool,
}

#[derive(Debug, Clone)]
pub struct TypedImmediate {
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct TypedBlockExpr {
    pub statements: Vec<TypedStatement>,
}

#[derive(Debug, Clone)]
pub struct TypedWhile {
    pub condition: Box<TypedExpression>,
    pub body: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedIf {
    pub condition: Box<TypedExpression>,
    pub body: Box<TypedExpression>,
    pub and_bodies: Vec<((TypedIf, Type), Location)>,
    pub else_body: Option<Box<TypedExpression>>,
    pub makes_option: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct TypedList {
    pub exprs: Vec<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedIndexExpr {
    pub obj: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedLambdaArg {
    pub number: usize,
}

#[derive(Debug, Clone)]
pub struct TypedStr {
    pub string: String,
}

#[derive(Debug, Clone)]
pub struct TypedCallExpr {
    pub function: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedMethodCallExpr {
    pub obj: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
    pub method_name: String,
}

#[derive(Debug, Clone)]
pub struct TypedInitCallExpr {
    pub obj: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
    pub alloc_before_call: usize,
}

#[derive(Debug, Clone)]
pub struct TypedDottedLookup {
    pub lhs: Box<TypedExpression>,
    pub rhs: String,
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
            errors: Vec::new(),
        };
        rv.openscope();
        for f in BuiltinFunction::get_table(&mut rv.system) {
            rv.insert_scope(&f.name, typesystem::BUILTIN_FUNCTION);
        }
        rv.openscope();
        rv
    }

    pub fn typecheck(&mut self, p: ParseTree) -> Result<TypedParseTree, Vec<Error>> {
        let results = match self.vec_of_statement(p.statements) {
            Ok(types) => types,
            Err(()) => return Err(take(&mut self.errors)),
        };
        self.globals += self.scopes[1].borrow().len();
        p.globals = Some(self.scopes[1].borrow().len());

        todo!()
    }

    pub fn vec_of_statement(&mut self, stats: Vec<Statement>) -> Result<Vec<TypedStatement>, ()> {
        stats.into_iter().map(|s| self.statement(s)).collect()
    }

    pub fn vec_of_exprs(&mut self, exprs: Vec<Expression>) -> Result<Vec<TypedExpression>, ()> {
        exprs.into_iter().map(|s| self.expr(s)).collect()
    }

    fn error<T>(&mut self, err: ErrorType) -> Result<T, ()> {
        self.errors.push(Error {
            err,
            loc: self.location.clone().unwrap(),
        });
        Err(())
    }

    fn error_add_origin<T>(&mut self, origin: Location) -> Result<T, ()> {
        self.errors.push(Error {
            err: ErrorType::Originating,
            loc: origin,
        });
        Err(())
    }

    fn error_with_origin<T>(&mut self, err: ErrorType, origin: Location) -> Result<T, ()> {
        self.error(err);
        self.error_add_origin(origin);
        Err(())
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

    fn statement(&mut self, s: Statement) -> Result<TypedStatement, ()> {
        Ok(match s {
            Statement::Expr(e) => TypedStatement::Expr(self.expr(e)?),
            Statement::ClassDecl(c) => todo!(), //self.classdecl(c.clone())?,
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            // These were moved out of parsing as parsing can only report one error right now
            Statement::Continue(c) => todo!("{:?}", c), // make sure the continue is allowed in the context
            Statement::Break(b) => todo!("{:?}", b), // make sure the continue is allowed in the context
            Statement::Return(r) => self.return_(r)?,
        })
    }

    fn return_(&mut self, r: Return) -> Result<TypedStatement, ()> {
        let body = match r.body {
            Some(e) => Some(self.expr(*e)?),
            None => None,
        };
        let typ = body.map(|b| b.typ).unwrap_or(UNIT);

        // This is an Option<Option<Type>>
        // if outer option is none then we are at global scope
        // if inner is None we havent derived a type yet
        match self.func_returns.last() {
            None if typ == UNIT || typ == INT => (),
            None => {
                self.error(ErrorType::CannotReturnTypeFromNonFunction(
                    self.system.typename(typ),
                ))?;
            }
            Some(None) => {
                let last_idx = self.func_returns.len() - 1;
                self.func_returns[last_idx] = Some(typ);
            }
            Some(Some(v)) if *v == typ => (),
            Some(Some(v)) => self.error(ErrorType::MismatchedReturnTypes(
                self.system.typename(typ),
                self.system.typename(*v),
            ))?,
        }

        Ok(TypedStatement::Return(TypedReturn { body }))
    }

    fn classdecl(&mut self, c: Rc<RefCell<ClassDecl>>) -> Result<Type, ()> {
        let cc = c.borrow();
        let typ = self
            .system
            .class_type(cc.name.clone(), c.borrow().fields.len());
        self.insert_class_scope(&cc.name, typ);
        drop(cc);
        self.type_to_class.insert(typ, c);
        Ok(typ)
    }

    fn expr(&mut self, e: Expression) -> Result<TypedExpression, ()> {
        self.location = Some(e.location.clone());
        let (typedexpressiontype, typ) = match e.etype {
            ExpressionType::While(w) => self.whileexpr(w),
            ExpressionType::For(f) => todo!("{f:?}"),
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
            ExpressionType::PossibleMethodCall(m) => self.possible_method_call(m)?,
        }?;
        // if rv == UNKNOWN_RETURN {
        // self.need_recheck = true;
        // }
        Ok(TypedExpression {
            typ,
            etype: typedexpressiontype,
            location: e.location,
        })
    }

    fn null(&mut self) -> Result<(TypedExpressionType, Type), ()> {
        Ok((TypedExpressionType::Null, NULL))
    }

    fn repeated_arg(&mut self) -> Result<(TypedExpressionType, Type), ()> {
        Ok((
            TypedExpressionType::RepeatedArg,
            self.repeated_arg.clone().unwrap().1,
        ))
    }

    fn binop(&mut self, b: BinOp) -> Result<(TypedExpressionType, Type), ()> {
        let mut lhs = self.expr(*b.lhs)?;
        let mut rhs = self.expr(*b.rhs)?;
        if self.system.is_option(lhs.typ) && rhs.typ == NULL {
            rhs.typ = lhs.typ;
        } else if self.system.is_option(rhs.typ) && lhs.typ == NULL {
            lhs.typ = rhs.typ;
        }
        match self.system.lookup_binop(b.op, lhs.typ, rhs.typ) {
            None => self.error(ErrorType::NoDefinedBinOp(
                self.system.typename(lhs.typ),
                b.op,
                self.system.typename(rhs.typ),
            )),
            Some(typ) => Ok((
                TypedExpressionType::BinOp(TypedBinOp {
                    lhs: Box::new(lhs),
                    op: b.op,
                    rhs: Box::new(rhs),
                }),
                typ,
            )),
        }
    }

    fn preunop(&mut self, p: PreUnOp) -> Result<(TypedExpressionType, Type), ()> {
        let rhs = self.expr(*p.rhs)?;
        match self.system.lookup_preunop(p.op, rhs.typ) {
            None => self.error(ErrorType::NoDefinedUnOp(
                self.system.typename(rhs.typ),
                p.op,
            )),
            Some(typ) => Ok((
                TypedExpressionType::PreUnOp(TypedPreUnOp {
                    rhs: Box::new(rhs),
                    op: p.op,
                }),
                typ,
            )),
        }
    }

    fn postunop(&mut self, p: PostUnOp) -> Result<(TypedExpressionType, Type), ()> {
        let lhs = self.expr(*p.lhs)?;
        match self.system.lookup_postunop(p.op, lhs.typ) {
            None => self.error(ErrorType::NoDefinedUnOp(
                self.system.typename(lhs.typ),
                p.op,
            )),
            Some(typ) => Ok((
                TypedExpressionType::PostUnOp(TypedPostUnOp {
                    lhs: Box::new(lhs),
                    op: p.op,
                }),
                typ,
            )),
        }
    }

    fn refexpr(&mut self, r: RefExpr) -> Result<(TypedExpressionType, Type), ()> {
        match (self.scope_lookup(&r.name), self.allow_insert) {
            (Some(t), _) => {
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        name: r.name,
                        is_decl: r.is_decl,
                    }),
                    t,
                ));
            }
            (None, Some(typ)) => {
                self.insert_scope(&r.name, typ);
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        name: r.name,
                        is_decl: true,
                    }),
                    typ,
                ));
            }
            (None, None) => (),
        }
        if let Some(typ) = self.scope_lookup_class(&r.name) {
            return Ok((
                TypedExpressionType::ClassRefExpr(TypedClassRefExpr { name: r.name }),
                typ,
            ));
        }
        match self.scope_lookup_func(&r.name) {
            Some(typ) if self.allow_raw_func => Ok((
                TypedExpressionType::FuncRefExpr(TypedFuncRefExpr { name: r.name }),
                typ,
            )),
            Some(_) => self.error(ErrorType::FuncTypeUnknown(r.name)),
            None => self.error(ErrorType::NoSuchNameInScope(r.name)),
        }
    }

    // fn funcrefexpr(&mut self, r: &mut FuncRefExpr) -> Result<(TypedExpressionType, Type), ()> {
    // let name = self.system.mangle(&r.name, &r.sig);
    // match self.scope_lookup(&name) {
    // Some(t) => Ok(t),
    // None => unreachable!("Could not find {:?} in scope", r),
    // }
    // }

    fn assignment(&mut self, a: AssignExpr) -> Result<(TypedExpressionType, Type), ()> {
        if !a.lhs.is_lval() {
            return self.error(ErrorType::NotAssignable);
        }
        let rhs = self.expr(*a.rhs)?;
        if rhs.typ == UNIT {
            return self.error(ErrorType::CannotAssignUnit);
        }
        self.allow_insert = Some(rhs.typ);
        let lhs = self.expr(*a.lhs)?;
        self.allow_insert = None;
        match self.system.lookup_assign(a.op, lhs.typ, rhs.typ) {
            Some(typ) => Ok((
                TypedExpressionType::AssignExpr(TypedAssignExpr {
                    lhs: Box::new(lhs),
                    op: a.op,
                    rhs: Box::new(rhs),
                    allow_decl: a.allow_decl,
                }),
                typ,
            )),
            None => self.error(ErrorType::CannotAssignType(
                self.system.typename(lhs.typ),
                a.op,
                self.system.typename(rhs.typ),
            )),
        }
    }

    fn immediate(&mut self, i: Immediate) -> Result<(TypedExpressionType, Type), ()> {
        Ok(match i.value {
            Value::Null => (
                TypedExpressionType::Immediate(TypedImmediate { value: i.value }),
                typesystem::NULL,
            ),
            Value::Bool(_) => (
                TypedExpressionType::Immediate(TypedImmediate { value: i.value }),
                typesystem::BOOL,
            ),
            Value::Char(_) => (
                TypedExpressionType::Immediate(TypedImmediate { value: i.value }),
                typesystem::CHAR,
            ),
            Value::Int(_) => (
                TypedExpressionType::Immediate(TypedImmediate { value: i.value }),
                typesystem::INT,
            ),
            Value::Float(_) => (
                TypedExpressionType::Immediate(TypedImmediate { value: i.value }),
                typesystem::FLOAT,
            ),
            i => unreachable!("Found {i:?}"),
        })
    }

    fn block(&mut self, b: BlockExpr) -> Result<(TypedExpressionType, Type), ()> {
        if b.statements.is_empty() {
            return self.error(ErrorType::EmptyBlock);
        }
        let statements = self.vec_of_statement(b.statements)?;
        let typ = match statements.last().unwrap() {
            TypedStatement::Expr(e) => e.typ,
            _ => typesystem::UNIT,
        };

        Ok((
            TypedExpressionType::BlockExpr(TypedBlockExpr { statements }),
            typ,
        ))
    }

    fn whileexpr(&mut self, w: While) -> Result<(TypedExpressionType, Type), ()> {
        let cond = self.expr(*w.condition);
        match cond {
            Ok(t) if t.typ != BOOL => {
                self.error::<()>(ErrorType::LoopConditionNotBool(self.system.typename(t.typ)));
            }
            _ => (),
        }
        let body = Box::new(self.expr(*w.body)?);
        let condition = Box::new(cond?);

        Ok((
            TypedExpressionType::While(TypedWhile { condition, body }),
            UNIT,
        ))
    }

    fn forexpr(&mut self, _w: &mut For) -> Result<(TypedExpressionType, Type), ()> {
        todo!("Havent worked out the details of iteration yet")
    }

    fn ifexpr(&mut self, i: If) -> Result<(TypedExpressionType, Type), ()> {
        let (tet, typ) = self.ifexpr_int(i, false)?;
        Ok((TypedExpressionType::If(tet), typ))
    }

    fn ifexpr_int(&mut self, i: If, is_and_if: bool) -> Result<(TypedIf, Type), ()> {
        let mut rvtyp = UNIT;
        let mut make_option = false;
        let mut return_unit = false;
        let cond = self.expr(*i.condition);
        match cond {
            Ok(t) if t.typ != BOOL && t.typ != UNKNOWN_RETURN => {
                self.error::<()>(ErrorType::IfConditionNotBool(self.system.typename(t.typ)));
            }
            _ => (),
        }

        let body = self.expr(*i.body);
        let and_bodies_res: Vec<(_, Location)> = i
            .and_bodies
            .into_iter()
            .map(|b| (self.ifexpr_int(b.0, true), b.1))
            .collect();

        let else_body = if let Some(b) = i.else_body {
            Some(self.expr(*b)?)
        } else {
            return_unit = true;
            None
        };

        let and_bodies = Vec::new();
        for (b, l) in and_bodies_res {
            and_bodies.push((b?, l));
        }
        let body = body?;
        let cond = cond?;

        for ((body, typ), location) in and_bodies.iter() {
            if *typ == NULL {
                make_option = true;
            } else if rvtyp == NULL {
                rvtyp = *typ;
                make_option = true;
            } else if rvtyp == UNKNOWN_RETURN {
                rvtyp = *typ;
            } else if rvtyp != *typ {
                return_unit = true;
            }
        }

        if let Some(typ) = else_body.map(|b| b.typ) {
            if typ == NULL {
                make_option = true;
            } else if rvtyp == NULL {
                rvtyp = typ;
                make_option = true;
            } else if rvtyp == UNKNOWN_RETURN {
                rvtyp = typ;
            } else if rvtyp != typ {
                return_unit = true;
            }
        }

        let ti = TypedIf {
            condition: Box::new(cond),
            body: Box::new(body),
            and_bodies,
            else_body: else_body.map(|b| Box::new(b)),
            makes_option: None,
        };

        if return_unit {
            rvtyp = UNIT;
        } else if make_option {
            rvtyp = self.system.option_type(rvtyp);
            ti.makes_option = Some(rvtyp);
        }
        Ok((ti, rvtyp))
    }

    fn list(&mut self, l: List) -> Result<(TypedExpressionType, Type), ()> {
        let mut interior_type = UNIT;
        let exprs = self.vec_of_exprs(l.exprs)?;

        for te in exprs.iter() {
            if interior_type == UNIT {
                interior_type = te.typ;
            } else if te.typ != interior_type {
                self.error::<()>(ErrorType::ListTypeMismatch(
                    self.system.typename(interior_type),
                    self.system.typename(te.typ),
                ));
            }
        }
        let rvtyp = self.system.list_type(interior_type);

        Ok((TypedExpressionType::List(TypedList { exprs }), rvtyp))
    }

    fn index(&mut self, i: IndexExpr) -> Result<(TypedExpressionType, Type), ()> {
        let obj = self.expr(*i.obj)?;
        // in the future there may be objects that are indexable so there will be an addition check
        let return_type = match self.system.underlying_type(obj.typ) {
            None => {
                return self.error(ErrorType::CannotIndexType(self.system.typename(obj.typ)));
            }
            Some(t) => t,
        };
        let args = self.vec_of_exprs(i.args)?;
        if args.len() != 1 || args[0].typ != INT {
            return self.error(ErrorType::IndexNotInt);
        }
        Ok((
            TypedExpressionType::IndexExpr(TypedIndexExpr {
                obj: Box::new(obj),
                args,
            }),
            return_type,
        ))
    }

    fn function(
        &mut self,
        f: Rc<RefCell<parser::Function>>,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let name = f.borrow().name.clone();
        let default = "<anonymous function>".to_owned();
        let typ = self.system.function_type(name.clone().unwrap_or(default));
        if let Some(s) = name {
            self.insert_func_scope(&s, typ);
        }
        self.type_to_func.insert(typ, f.clone());
        Ok((TypedExpressionType::Function, typ))
    }

    fn lambda(&mut self, l: Rc<RefCell<Lambda>>) -> Result<(TypedExpressionType, Type), ()> {
        let typ = self.system.function_type("<lambda>".to_string());
        self.type_to_lambda.insert(typ, l.clone());
        Ok((TypedExpressionType::Lambda, typ))
    }

    fn lambdaarg(&mut self, l: LambdaArg) -> Result<(TypedExpressionType, Type), ()> {
        debug_assert!(
            !self.lambda_args.is_empty(),
            "Trying to derive type of lambda arg in non lambda. This is a bug"
        );
        Ok((
            TypedExpressionType::LambdaArg(TypedLambdaArg { number: l.number }),
            self.lambda_args[l.number],
        ))
    }

    fn string(&mut self, s: Str) -> Result<(TypedExpressionType, Type), ()> {
        Ok((TypedExpressionType::Str(TypedStr { string: s.string }), STR))
    }

    fn possible_method_call(
        &mut self,
        mut m: PossibleMethodCall,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let lhs = self.expr(*m.lhs)?;
        let lhs = self.system.class_underlying(lhs.typ);
        if let Some(classdecl) = self.type_to_class.get(&lhs).cloned() {
            let (function, method_name) = if classdecl.borrow().methods.contains_key(&m.method_name)
            {
                (m.lhs, Some(m.method_name))
            } else {
                (
                    Box::new(Expression {
                        etype: ExpressionType::DottedLookup(DottedLookup {
                            lhs: m.lhs,
                            rhs: m.method_name,
                            index: None,
                        }),
                        derived_type: None,
                        location: m.location,
                    }),
                    None,
                )
            };
            self.call(CallExpr {
                function,
                args: m.args,
                alloc_before_call: None,
                method_name,
            })
        } else {
            self.error(ErrorType::ClassHasNoFieldOrMethod(
                self.system.typename(lhs),
                m.method_name,
            ))
        }
    }

    fn call(&mut self, c: CallExpr) -> Result<(TypedExpressionType, Type), ()> {
        let save = self.allow_raw_func;
        self.allow_raw_func = true;
        let mut functype = self.expr(*c.function);
        self.allow_raw_func = save;
        let mut funcloc = c.function.location.clone();
        let args = self.vec_of_exprs(c.args)?;
        let argtypes: Vec<Type> = args.iter().map(|e| e.typ).collect();
        let subject = functype?;

        let mut override_return = None;
        if self.system.is_class(subject.typ) {
            debug_assert!(
                !c.method_name.is_some(),
                "Class fields/static methods not supported yet"
            );
            let classdecl = self.type_to_class.get(&subject.typ).unwrap().clone();
            let alloc_before_call = classdecl.borrow().fields.len();
            c.alloc_before_call = Some(alloc_before_call);
            if let Some(init) = classdecl.borrow_mut().methods.get_mut("__init__") {
                if let ExpressionType::Function(f) = &mut init.etype {
                    f.borrow_mut().alloc_before_call = c.alloc_before_call;
                }
                let unresolved = self.system.class_new_unresolved(subject.typ);
                argtypes.insert(0, unresolved);
                let init_func = self.expr(init.clone())?;
                return Ok((
                    TypedExpressionType::InitCallExpr(TypedInitCallExpr {
                        obj: Box::new(subject),
                        args,
                        alloc_before_call,
                    }),
                    unresolved,
                ));
            } else {
                return self.error(ErrorType::ClassLacksInit(self.system.typename(subject.typ)));
            }
        } else if let Some(name) = c.method_name {
            let clstype = self.system.class_underlying(subject.typ);
            let classdecl = self.type_to_class.get_mut(&clstype).unwrap().clone();
            if let Some(method) = classdecl.borrow_mut().methods.get_mut(&name) {
                args.insert(0, subject);
                let method_func = self.expr(method.clone())?;
                return Ok((
                    TypedExpressionType::MethodCallExpr(TypedMethodCallExpr {
                        obj: Box::new(subject),
                        args,
                        method_name: name,
                    }),
                    unresolved,
                ));
            } else {
                return self.error(ErrorType::ClassHasNoFieldOrMethod(
                    self.system.typename(subject.typ),
                    name,
                ));
            }
            functype = self.expr(classdecl.borrow_mut().methods.get_mut(name).unwrap())?;
        }

        if !self.system.is_function(functype) {
            return Err(self.error("Trying to call value that is not callable".to_string()));
        }

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
    ) -> Result<TypedExpression, ()> {
        match function.try_borrow_mut() {
            Ok(_) => (),
            Err(_) => {
                return Ok(TypedExpression {
                    etype: TypedExpressionType::UnknownReturn,
                    location: self.location.clone().unwrap(),
                    typ: UNKNOWN_RETURN,
                })
            }
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
        calling_location: &Location,
    ) -> Result<(TypedExpression, usize), ()> {
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
        let rv = self.call_single_check(*function.borrow_mut().body.clone(), calling_location)?;
        Ok((rv, self.closescope()))
    }

    fn call_single_check(
        &mut self,
        function: Expression,
        calling_location: &Location,
    ) -> Result<TypedExpression, ()> {
        self.func_returns.push(None);
        let save_repeated = self.repeated_arg.clone();
        let rv = match self.expr(function) {
            Ok(t) if t.typ == UNKNOWN_RETURN => {
                self.error_with_origin(
                    ErrorType::CannotDeriveReturnType,
                    calling_location.clone(),
                )?;
                return Err(());
            }
            Err(()) => {
                return self.error_add_origin(calling_location.clone());
            }
            Ok(t) => t,
        };
        self.repeated_arg = save_repeated;
        let derived = self.func_returns.pop().unwrap();
        match derived {
            Some(t) if t != rv.typ => {
                return self.error(ErrorType::MismatchedReturnTypes(
                    self.system.typename(rv.typ),
                    self.system.typename(t),
                ));
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
    ) -> Result<TypedExpression, ()> {
        // There may be a systen where we can cache the results of these expressions
        // But currently I'm not 100% sure on the relationship between functions and types
        // And it seems like I should move the the resolved/unresolved system I use for classes
        // But this should work
        // if let Some(t) = self.system.match_signature(functype, &args) {
        // return Ok(t);
        // }

        swap(&mut self.lambda_args, &mut args);

        self.openscope();
        self.func_returns.push(None);
        let rv = self.call_single_check(*lambda.borrow_mut().body.clone(), &funcloc)?;
        lambda.borrow_mut().locals = Some(self.closescope() + args.len());
        swap(&mut self.lambda_args, &mut args);
        let rftyp = self
            .system
            .add_function_signature(functype, Signature::new(args, None, rv.typ));
        c.function.as_mut().derived_type = Some(rftyp);
        Ok(rv)
    }

    fn dotted_lookup(&mut self, d: DottedLookup) -> Result<(TypedExpressionType, Type), ()> {
        let lhs = self.expr(*d.lhs)?;
        let classdecltyp = self.system.class_underlying(lhs.typ);
        let classdecl = self.type_to_class.get(&classdecltyp).unwrap();
        let idx = *classdecl.borrow().fields.get(&d.rhs).unwrap();
        d.index = Some(idx);
        let field_type = self.system.class_query_field(lhs.typ, idx);
        let typ = match (field_type, self.allow_insert) {
            (Some(ft), _) => ft,
            (None, Some(ai)) => {
                self.system.set_field_type(lhs.typ, idx, ai);
                ai
            }
            (None, None) => {
                return self.error(ErrorType::UninitializedAssignment);
            }
        };
        Ok((
            TypedExpressionType::DottedLookup(TypedDottedLookup {
                lhs: Box::new(lhs),
                rhs: d.rhs,
            }),
            typ,
        ))
    }
}
