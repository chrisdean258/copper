#![allow(dead_code)]
use crate::{
    builtins::BuiltinFunction, error::ErrorCollection, location::Location, operation::Operation,
    parser, parser::*, typesystem, typesystem::*, value::Value,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem::{replace, swap, take},
    rc::Rc,
};

#[derive(Debug, Clone)]
pub struct Error {
    loc: Location,
    err: ErrorType,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    BreakNotAllowed,
    ContinueNotAllowed,
    CannotAssignType(String, Operation, String),
    CannotAssignUnit,
    CannotDeriveReturnType,
    CannotIndexType(String),
    CannotIndirectlyCallBuiltins,
    CannotReturnTypeFromNonFunction(String),
    ClassHasNoFieldOrMethod(String, String),
    ClassLacksInit(String),
    EmptyBlock,
    FuncTypeUnknown(String),
    IfConditionNotBool(String),
    IncorrectTypesForBuiltin(String, String),
    IndexNotInt,
    ListTypeMismatch(String, String),
    LoopConditionNotBool(String),
    MismatchedReturnTypes(String, String),
    NoDefinedBinOp(String, Operation, String),
    NoDefinedUnOp(String, Operation),
    NoSuchNameInScope(String),
    NotAssignable,
    NotCallable(String),
    Originating,
    UninitializedAssignment,
    WrongNumberOfArguments(usize, usize),
    WrongNumberOfArgumentsWithDefault(usize, usize, usize),
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.loc, self.err)
    }
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BreakNotAllowed => write!(f, "`break` not allowed outside of loops"),
            Self::ContinueNotAllowed => write!(f, "`continue` not allowed outside of loops"),
            Self::CannotAssignType(t1, op, t2) => write!(f,  "Cannot assign `{t1}` {op} `{t2}`",),
            Self::CannotAssignUnit => write!(f, "Cannot assign unit value"),
            Self::CannotDeriveReturnType => write!(f, "Cannot derive the return type of this function call"),
            Self::CannotIndexType(t) => write!(f,  "Cannot index into type `{t}`"),
            Self::CannotIndirectlyCallBuiltins => write!(f, "Cannot indirectly call buitins yet"),
            Self::CannotReturnTypeFromNonFunction(t) => write!(f,  "Cannot return type `{t}` from outside a function"),
            Self::ClassHasNoFieldOrMethod(t, fm) => write!(f,  "Class `{t}` has no field or method `{fm}`",),
            Self::ClassLacksInit(t) => write!(f,  "class `{t}` lacks the `__init__` method and cannot be constructed"),
            Self::EmptyBlock => write!(f, "Empty Block Expressions are not allowed (yet)"),
            Self::FuncTypeUnknown(func) => write!(f,  "`{func}` is a function whose types cannot be determined. Try wrapping it in a lambda",),
            Self::IfConditionNotBool(t) => write!(f, "if condition must be bool. This one is `{t}`"),
            Self::IncorrectTypesForBuiltin(e, fo) => write!(f, "Functions inputs didnt match. Exepcted `{e}` found `{fo}`"),
            Self::IndexNotInt => write!(f,  "Index expression requires 1 argument of type `int`"),
            Self::ListTypeMismatch(t1, t2) => write!(f,  "List established to contain type `{t1}` but this element was of type `{t2}`",),
            Self::LoopConditionNotBool(t) => write!(f, "While loop condition must be bool. This one is `{t}`"),
            Self::MismatchedReturnTypes(t1, t2) => write!(f,  "Cannot return type `{t1}` from function. Return type already encountered is `{t2}`",),
            Self::NoDefinedBinOp(t1, op, t2) => write!(f,  "Cannot apply binary operation `{t1}` {op} `{t2}`. No operation has been defined between these types",),
            Self::NoDefinedUnOp(t, op) => write!(f,  "Cannot apply binary operation {op} to `{t}`. No operation has been defined",),
            Self::NoSuchNameInScope(s) => write!(f,  "`no name `{s}` in scope",),
            Self::NotAssignable => write!(f, "LHS of assignment is not assignable"),
            Self::NotCallable(t) => write!(f, "`{t}` is not callable"),
            Self::Originating => write!(f, "Originating here"),
            Self::UninitializedAssignment => write!(f, "Trying to assign to variable with uninitialized type. This might be a bug"),
            Self::WrongNumberOfArguments(e, t) => write!(f,  "Wrong number of arguments. Expected {e} found {t}",),
            Self::WrongNumberOfArgumentsWithDefault(a, d, t) => write!(f,  "Wrong number of arguments. Found {a} arguemnts with {d} defaults. Expected a total of {t}",),
        }
    }
}

type ParserFunction = Rc<RefCell<parser::Function>>;
type ParserLambda = Rc<RefCell<parser::Lambda>>;

pub struct TypeChecker {
    pub system: TypeSystem,
    scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,
    func_scopes: Vec<Rc<RefCell<HashMap<String, ParserFunction>>>>,
    class_scopes: Vec<Rc<RefCell<HashMap<String, Type>>>>,

    type_to_func: HashMap<Type, ParserFunction>,
    type_to_lambda: HashMap<Type, Rc<RefCell<Lambda>>>,
    type_to_class: HashMap<Type, Rc<RefCell<ClassDecl>>>,
    type_to_resolved_func: HashMap<Type, TypedExpression>,

    allow_insert: Option<Type>,
    lambda_args: Vec<TypedExpression>,
    location: Option<Location>,
    globals: usize,
    allow_raw_func: bool,
    func_returns: Vec<Option<Type>>,
    need_recheck: bool,
    repeated_arg: Option<(String, Type)>,
    errors: ErrorCollection<Error>,
    in_loop: bool,
}

#[derive(Debug, Clone)]
pub struct TypedParseTree {
    pub statements: Vec<TypedStatement>,
    pub globals: usize,
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
    BuiltinFuncRefExpr(TypedBuiltinFuncRefExpr),
    Immediate(TypedImmediate),
    BlockExpr(TypedBlockExpr),
    BinOp(TypedBinOp),
    PreUnOp(TypedPreUnOp),
    PostUnOp(TypedPostUnOp),
    AssignExpr(TypedAssignExpr),
    // Functions/lambdas fundamentally don't have types until they are called
    // So this requires looking in the scope table
    Function(ParserFunction),
    Lambda(ParserLambda),
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
pub struct TypedReturn {
    pub body: Option<TypedExpression>,
    pub from_function: bool,
}

#[derive(Debug, Clone)]
pub struct TypedBinOp {
    pub lhs: Box<TypedExpression>,
    pub op: Operation,
    pub rhs: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedPreUnOp {
    pub op: Operation,
    pub rhs: Box<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct TypedPostUnOp {
    pub lhs: Box<TypedExpression>,
    pub op: Operation,
}

// TODO: Break this out into a separate struct that incapsulates declarations and variable
// references separately
#[derive(Debug, Clone)]
pub struct TypedVarRefExpr {
    pub name: String,
    pub is_decl: bool,
}

#[derive(Debug, Clone)]
pub struct TypedClassRefExpr {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypedFuncRefExpr {
    pub name: String,
    pub func: ParserFunction,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinFuncRefExpr {
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
    pub sig_idx: Option<usize>,
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
    pub index: usize,
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
            type_to_resolved_func: HashMap::new(),
            allow_insert: None,
            lambda_args: Vec::new(),
            location: None,
            globals: 0,
            allow_raw_func: false,
            func_returns: Vec::new(),
            need_recheck: false,
            repeated_arg: None,
            errors: ErrorCollection::new(),
            in_loop: false,
        };
        rv.openscope();
        for f in BuiltinFunction::get_table(&mut rv.system) {
            rv.insert_scope(&f.name, typesystem::BUILTIN_FUNCTION);
        }
        rv.openscope();
        rv
    }

    pub fn typecheck(
        &mut self,
        mut p: ParseTree,
    ) -> Result<TypedParseTree, ErrorCollection<Error>> {
        let results = match self.vec_of_statement(p.statements) {
            Ok(types) => types,
            Err(()) => return Err(take(&mut self.errors)),
        };
        self.globals += self.scopes[1].borrow().len();
        p.globals = Some(self.scopes[1].borrow().len());

        Ok(TypedParseTree {
            statements: results,
            globals: self.globals,
        })
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
        let _ = self.error::<()>(err);
        let _ = self.error_add_origin::<()>(origin);
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

    fn scope_lookup_func(&self, name: &str) -> Option<ParserFunction> {
        for scope in self.func_scopes.iter().rev() {
            if let Some(t) = scope.borrow().get(name) {
                return Some(t.clone());
            }
        }
        None
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

    fn insert_func_scope(&mut self, name: &str, f: ParserFunction) -> usize {
        let mut scope = self.func_scopes.last().unwrap().borrow_mut();
        let place = scope.len();
        scope.insert(String::from(name), f);
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
            Statement::ClassDecl(_c) => todo!(), //self.classdecl(c.clone())?,
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            // These were moved out of parsing as parsing can only report one error right now
            Statement::Continue(_) => self.continue_()?,
            Statement::Break(_) => self.break_()?,
            Statement::Return(r) => self.return_(r)?,
        })
    }

    fn return_(&mut self, r: Return) -> Result<TypedStatement, ()> {
        let body = match r.body {
            Some(e) => Some(self.expr(*e)?),
            None => None,
        };
        let typ = body.as_ref().map(|b| b.typ).unwrap_or(UNIT);

        // This is an Option<Option<Type>>
        // if outer option is none then we are at global scope
        // if inner is None we havent derived a type yet
        let from_function = !self.func_returns.is_empty();
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

        Ok(TypedStatement::Return(TypedReturn {
            body,
            from_function,
        }))
    }

    fn break_(&mut self) -> Result<TypedStatement, ()> {
        if !self.in_loop {
            return self.error(ErrorType::BreakNotAllowed);
        }
        Ok(TypedStatement::Break)
    }

    fn continue_(&mut self) -> Result<TypedStatement, ()> {
        if !self.in_loop {
            return self.error(ErrorType::BreakNotAllowed);
        }
        Ok(TypedStatement::Break)
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
            ExpressionType::RepeatedArg => self.repeated_arg(),
            ExpressionType::Null => self.null(),
            ExpressionType::PossibleMethodCall(m) => self.possible_method_call(m),
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
                    if t == BUILTIN_FUNCTION {
                        TypedExpressionType::BuiltinFuncRefExpr(TypedBuiltinFuncRefExpr {
                            name: r.name,
                        })
                    } else {
                        TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                            name: r.name,
                            is_decl: r.is_decl,
                        })
                    },
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
            Some(func) if self.allow_raw_func => Ok((
                TypedExpressionType::FuncRefExpr(TypedFuncRefExpr { name: r.name, func }),
                typesystem::UNKNOWN_RETURN,
            )),
            Some(_) => self.error(ErrorType::FuncTypeUnknown(r.name)),
            None => self.error(ErrorType::NoSuchNameInScope(r.name)),
        }
    }

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
        let save_in_loop = replace(&mut self.in_loop, true);
        let cond = self.expr(*w.condition);
        match &cond {
            Ok(t) if t.typ != BOOL => {
                let _ =
                    self.error::<()>(ErrorType::LoopConditionNotBool(self.system.typename(t.typ)));
            }
            _ => (),
        }
        let body = Box::new(self.expr(*w.body)?);
        let condition = Box::new(cond?);
        self.in_loop = save_in_loop;

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
        match &cond {
            Ok(t) if t.typ != BOOL && t.typ != UNKNOWN_RETURN => {
                let _ =
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

        let mut and_bodies = Vec::new();
        for (b, l) in and_bodies_res {
            and_bodies.push((b?, l));
        }
        let body = body?;
        let cond = cond?;

        for ((_body, typ), _location) in and_bodies.iter() {
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

        if let Some(typ) = else_body.as_ref().map(|b| b.typ) {
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
        } else {
            return_unit = !is_and_if;
        }

        let mut ti = TypedIf {
            condition: Box::new(cond),
            body: Box::new(body),
            and_bodies,
            else_body: else_body.map(Box::new),
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
                let _ = self.error::<()>(ErrorType::ListTypeMismatch(
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

    fn function(&mut self, f: ParserFunction) -> Result<(TypedExpressionType, Type), ()> {
        let name = f.borrow().name.clone();
        let default = "<anonymous function>".to_owned();
        let typ = self.system.function_type(name.clone().unwrap_or(default));
        self.type_to_func.insert(typ, f.clone());
        if let Some(s) = name {
            self.insert_func_scope(&s, f.clone());
        }
        Ok((TypedExpressionType::Function(f), typ))
    }

    fn lambda(&mut self, l: ParserLambda) -> Result<(TypedExpressionType, Type), ()> {
        let typ = self.system.function_type("<lambda>".to_string());
        self.type_to_lambda.insert(typ, l.clone());
        Ok((TypedExpressionType::Lambda(l), typ))
    }

    fn lambdaarg(&mut self, l: LambdaArg) -> Result<(TypedExpressionType, Type), ()> {
        debug_assert!(
            !self.lambda_args.is_empty(),
            "Trying to derive type of lambda arg in non lambda. This is a bug"
        );
        Ok((
            TypedExpressionType::LambdaArg(TypedLambdaArg { number: l.number }),
            self.lambda_args[l.number].typ,
        ))
    }

    fn string(&mut self, s: Str) -> Result<(TypedExpressionType, Type), ()> {
        Ok((TypedExpressionType::Str(TypedStr { string: s.string }), STR))
    }

    fn possible_method_call(
        &mut self,
        m: PossibleMethodCall,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let lhs = self.expr(*m.lhs.clone())?;
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

    fn call(&mut self, mut c: CallExpr) -> Result<(TypedExpressionType, Type), ()> {
        let save = self.allow_raw_func;
        self.allow_raw_func = true;
        let functype = self.expr(*c.function.clone());
        self.allow_raw_func = save;

        // let mut funcloc = c.function.location.clone();
        let mut args = self.vec_of_exprs(c.args)?;
        let mut argtypes: Vec<Type> = args.iter().map(|e| e.typ).collect();
        let mut subject = functype?;

        // let mut override_return = None;
        if self.system.is_class(subject.typ) {
            debug_assert!(
                c.method_name.is_none(),
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
                let _init_func = self.expr(init.clone())?;
                // let _ = (
                // TypedExpressionType::InitCallExpr(TypedInitCallExpr {
                // obj: Box::new(init_func),
                // args,
                // alloc_before_call,
                // }),
                // unresolved,
                // );
            } else {
                return self.error(ErrorType::ClassLacksInit(self.system.typename(subject.typ)));
            }
            drop(classdecl); // for some reason the rust compiler wants to drop this too early
        } else if let Some(name) = c.method_name {
            let clstype = self.system.class_underlying(subject.typ);
            let classdecl = self.type_to_class.get_mut(&clstype).unwrap().clone();
            if let Some(method) = classdecl.borrow_mut().methods.get_mut(&name) {
                args.insert(0, subject);
                let _method_func = self.expr(method.clone())?;
                if true {
                    todo!()
                }
                // let rv = Ok((
                // TypedExpressionType::MethodCallExpr(TypedMethodCallExpr {
                // obj: Box::new(subject),
                // args,
                // method_name: name,
                // }),
                // clstype,
                // ));
            } else {
                return self.error(ErrorType::ClassHasNoFieldOrMethod(
                    self.system.typename(subject.typ),
                    name,
                ));
            }
            subject = self.expr(classdecl.borrow_mut().methods.get(&name).unwrap().clone())?;
        }

        if !self.system.is_function(subject.typ) {
            return self.error(ErrorType::NotCallable(self.system.typename(subject.typ)));
        }

        if subject.typ == BUILTIN_FUNCTION {
            let ExpressionType::RefExpr(refexpr) =  &c.function.as_ref().etype else {
                return self.error(ErrorType::CannotIndirectlyCallBuiltins);
            };
            let funcname = refexpr.name.clone();
            let builtins = BuiltinFunction::get_hashmap(&mut self.system);
            // Ok to unwrap here because we have already verified that this is a builtin function
            let func = builtins.get(&funcname).unwrap();
            let outtyp = func.signature.match_inputs(&argtypes).ok_or_else(|| {
                let _ = self.error::<()>(ErrorType::IncorrectTypesForBuiltin(
                    self.system.format_args_from_sig(&func.signature),
                    self.system.format_args(&argtypes),
                ));
            })?;
            return Ok((
                TypedExpressionType::CallExpr(TypedCallExpr {
                    function: Box::new(subject),
                    args,
                    sig_idx: None,
                }),
                outtyp,
            ));
        }

        if let TypedExpressionType::FuncRefExpr(f) = &subject.etype {
            self.call_func_with_args(f.func.clone(), subject, args)
        } else if let Some(func) = self.type_to_func.get(&subject.typ) {
            self.call_func_with_args(func.clone(), subject, args)
        } else if let Some(lambda) = self.type_to_lambda.get(&subject.typ) {
            self.call_lambda_with_args(lambda.clone(), subject, args)
        } else {
            todo!()
        }
    }

    fn call_func_with_args(
        &mut self,
        f: ParserFunction,
        subject: TypedExpression,
        args: Vec<TypedExpression>,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let b = match f.try_borrow() {
            Ok(a) => a,
            Err(_) => {
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args,
                        sig_idx: None,
                    }),
                    UNKNOWN_RETURN,
                ))
            }
        };
        for (i, sig) in b.signatures.iter().enumerate() {
            if sig.inputs == args.iter().map(|a| a.typ).collect::<Vec<_>>() {
                let out = sig.output;
                drop(b);
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args,
                        sig_idx: Some(i),
                    }),
                    out,
                ));
            }
        }
        drop(b);
        let calling_location = self.location.clone().unwrap();
        let (te, sig_idx) = self.call_function(f, args.clone(), &calling_location)?;
        Ok((
            TypedExpressionType::CallExpr(TypedCallExpr {
                function: Box::new(subject),
                args,
                sig_idx,
            }),
            te.typ,
        ))
    }

    #[allow(dead_code)]
    fn call_function(
        &mut self,
        function: ParserFunction,
        args: Vec<TypedExpression>,
        calling_location: &Location,
    ) -> Result<(TypedExpression, Option<usize>), ()> {
        if function.try_borrow_mut().is_err() {
            return Ok((
                TypedExpression {
                    etype: TypedExpressionType::UnknownReturn,
                    location: self.location.clone().unwrap(),
                    typ: UNKNOWN_RETURN,
                },
                None,
            ));
        }

        let argtypes = args.iter().map(|a| a.typ).collect();
        let rv = self.call_function_int(function.clone(), args, calling_location)?;

        // TODO: This might be the right algorithm
        let func_sig = if function.borrow().repeated.is_none() {
            Signature::new(argtypes, None, rv.typ)
        } else {
            let num = function.borrow().argnames.len();
            Signature::new(
                argtypes.iter().take(num).cloned().collect(),
                argtypes.last().cloned(),
                rv.typ,
            )
        };

        let idx = function.borrow().signatures.len();
        function.borrow_mut().typed_bodies.push(rv.clone());
        function.borrow_mut().signatures.push(func_sig.clone());

        if let Some(name) = &function.borrow().name {
            self.func_mangle_name_scope_insert(name, func_sig);
        }
        Ok((rv, Some(idx)))
    }

    fn call_function_single_check(
        &mut self,
        function: &ParserFunction,
        args: &[TypedExpression],
        calling_location: &Location,
    ) -> Result<(TypedExpression, usize), ()> {
        self.openscope();
        if let Some(name) = function.borrow().repeated.clone() {
            if args.len() > function.borrow().argnames.len() {
                self.repeated_arg = Some((name, args.last().unwrap().typ));
            } else {
                self.repeated_arg = Some((name, 0));
            }
        }
        for (typ, name) in args.iter().zip(function.borrow().argnames.iter()) {
            self.insert_scope(name, typ.typ);
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
        let save_in_loop = replace(&mut self.in_loop, false);
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
        self.in_loop = save_in_loop;
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
        function: ParserFunction,
        mut args: Vec<TypedExpression>,
        calling_location: &Location,
    ) -> Result<TypedExpression, ()> {
        let num_default_needed = if function.borrow().repeated.is_some() {
            0
        } else {
            let argnamelen = function.borrow().argnames.len();
            if argnamelen < args.len() {
                return self.error(ErrorType::WrongNumberOfArguments(argnamelen, args.len()));
            }
            argnamelen - args.len()
        };

        if num_default_needed > function.borrow().default_args.len() {
            return self.error(ErrorType::WrongNumberOfArgumentsWithDefault(
                args.len(),
                function.borrow().default_args.len(),
                function.borrow().argnames.len(),
            ));
        }

        let first_default = function.borrow().default_args.len() - num_default_needed;

        let mut def_args = Vec::new();
        for i in 0..num_default_needed {
            let expr = function.borrow().default_args[i + first_default].clone();
            def_args.push(self.expr(expr));
        }
        for e in def_args {
            args.push(e?);
        }

        let save = self.need_recheck;
        self.need_recheck = false;
        let (rv, num_locals) =
            self.call_function_single_check(&function, &args, calling_location)?;
        function.borrow_mut().locals = Some(num_locals);

        if self.need_recheck {
            let (new_rv, num_locals) =
                self.call_function_single_check(&function, &args, calling_location)?;
            if new_rv.typ != rv.typ {
                self.type_to_resolved_func.remove(&rv.typ);
                todo!();
                // return self.error_with_origin(ErrorType::MismatchedReturnTypes(
                // self.system.typename(new_rv.typ),
                // self.system.typename(rv.typ),
                // ));
            }
            self.type_to_resolved_func.insert(rv.typ, new_rv.clone());
            debug_assert_eq!(function.borrow_mut().locals.unwrap(), num_locals);
            self.need_recheck = save;
            Ok(new_rv)
        } else {
            self.need_recheck = save;
            Ok(rv)
        }
    }

    fn call_lambda_with_args(
        &mut self,
        l: ParserLambda,
        subject: TypedExpression,
        args: Vec<TypedExpression>,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let b = match l.try_borrow() {
            Ok(a) => a,
            Err(_) => {
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args,
                        sig_idx: None,
                    }),
                    UNKNOWN_RETURN,
                ))
            }
        };
        for (i, sig) in b.signatures.iter().enumerate() {
            if sig.inputs == args.iter().map(|a| a.typ).collect::<Vec<_>>() {
                let out = sig.output;
                drop(b);
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args,
                        sig_idx: Some(i),
                    }),
                    out,
                ));
            }
        }
        drop(b);
        let calling_location = self.location.clone().unwrap();
        let (te, sig_idx) = self.call_lambda(l, args.clone(), &calling_location)?;
        Ok((
            TypedExpressionType::CallExpr(TypedCallExpr {
                function: Box::new(subject),
                args,
                sig_idx,
            }),
            te.typ,
        ))
    }

    #[allow(dead_code)]
    fn call_lambda(
        &mut self,
        lambda: ParserLambda,
        mut args: Vec<TypedExpression>,
        funcloc: &Location,
    ) -> Result<(TypedExpression, Option<usize>), ()> {
        // There may be a systen where we can cache the results of these expressions
        // But currently I'm not 100% sure on the relationship between functions and types
        // And it seems like I should move the the resolved/unresolved system I use for classes
        // But this should work
        // if let Some(t) = self.system.match_signature(functype, &args) {
        // return Ok(t);
        // }

        swap(&mut self.lambda_args, &mut args);
        let argtypes = args.iter().map(|a| a.typ).collect();
        self.openscope();
        self.func_returns.push(None);
        let rv = self.call_single_check(*lambda.borrow_mut().body.clone(), funcloc)?;
        let func_sig = Signature::new(argtypes, None, rv.typ);
        let idx = lambda.borrow().signatures.len();
        lambda.borrow_mut().typed_bodies.push(rv.clone());
        lambda.borrow_mut().signatures.push(func_sig);
        lambda.borrow_mut().locals = Some(self.closescope() + args.len());
        swap(&mut self.lambda_args, &mut args);
        Ok((rv, Some(idx)))
    }

    fn dotted_lookup(&mut self, mut d: DottedLookup) -> Result<(TypedExpressionType, Type), ()> {
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
                index: idx,
            }),
            typ,
        ))
    }
}
