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
    CannotExtractFromNonOption(String),
    CannotIndexType(String),
    CannotIndirectlyCallBuiltins,
    CannotIterateOverType(String),
    CannotIterateOverTypeLacksInit(String),
    CannotIterateOverTypeLacksNext(String),
    CannotReturnTypeFromNonFunction(String),
    ClassHasNoFieldOrMethod(String, String),
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
    ParseError(parser::Error),
    UninitializedAssignment,
    WrongNumberOfArguments(usize, usize),
    WrongNumberOfArgumentsWithDefault(usize, usize, usize),
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.err {
            ErrorType::ParseError(e) => write!(f, "{e}"),
            _ => write!(f, "{}: {}", self.loc, self.err),
        }
    }
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::BreakNotAllowed => write!(f, "`break` not allowed outside of loops"),
            Self::CannotAssignType(t1, op, t2) => write!(f,  "Cannot assign `{t1}` {op} `{t2}`",),
            Self::CannotAssignUnit => write!(f, "Cannot assign unit value"),
            Self::CannotDeriveReturnType => write!(f, "Cannot derive the return type of this function call"),
            Self::CannotExtractFromNonOption(t) => write!(f, "Cannot extract value from type `{t}`"),
            Self::CannotIndexType(t) => write!(f,  "Cannot index into type `{t}`"),
            Self::CannotIndirectlyCallBuiltins => write!(f, "Cannot indirectly call buitins yet"),
            Self::CannotIterateOverType(t) => write!(f, "Cannot iterate over type `{t}`"),
            Self::CannotIterateOverTypeLacksInit(t) => write!(f, "Cannot iterate over class `{t}` as it lacks the __init__ method"),
            Self::CannotIterateOverTypeLacksNext(t) => write!(f, "Cannot iterate over class `{t}` as it lacks the __next__ method"),
            Self::CannotReturnTypeFromNonFunction(t) => write!(f,  "Cannot return type `{t}` from outside a function"),
            Self::ClassHasNoFieldOrMethod(t, fm) => write!(f,  "Class `{t}` has no field or method `{fm}`",),
            Self::ContinueNotAllowed => write!(f, "`continue` not allowed outside of loops"),
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
            Self::NoSuchNameInScope(s) => write!(f,  "No variable, class, or function `{s}` in scope",),
            Self::NotAssignable => write!(f, "LHS of assignment is not assignable"),
            Self::NotCallable(t) => write!(f, "`{t}` is not callable"),
            Self::Originating => write!(f, "Originating here"),
            Self::ParseError(e) => write!(f, "{e}"),
            Self::UninitializedAssignment => write!(f, "Trying to assign to variable with uninitialized type. This might be a bug"),
            Self::WrongNumberOfArguments(e, t) => write!(f,  "Wrong number of arguments. Expected {e} found {t}",),
            Self::WrongNumberOfArgumentsWithDefault(a, d, t) => write!(f,  "Wrong number of arguments. Found {a} arguemnts with {d} defaults. Expected a total of {t}",),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum VarLoc {
    Global(usize),
    Local(usize),
}

pub struct TypeChecker {
    pub system: TypeSystem,
    scopes: Vec<HashMap<String, (Type, VarLoc)>>,
    func_scopes: Vec<HashMap<String, TypedFunction>>,
    class_scopes: Vec<HashMap<String, Type>>,

    type_to_func: HashMap<Type, TypedFunction>,
    type_to_lambda: HashMap<Type, TypedLambda>,
    type_to_class: HashMap<Type, TypedClassDecl>,
    type_to_obj: HashMap<Type, TypedObject>,

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

    builtins: HashMap<String, BuiltinFunction>,
}

#[derive(Debug, Clone)]
pub struct TypedParseTree {
    pub statements: Vec<TypedStatement>,
    pub globals: usize,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TypedStatement {
    Expr(TypedExpression),
    ClassDecl(TypedClassDecl),
    Import(Import),
    FromImport(FromImport),
}

#[derive(Debug, Clone)]
pub struct TypedExpression {
    pub etype: TypedExpressionType,
    pub location: Location,
    pub typ: Type,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TypedExpressionType {
    While(TypedWhile),
    ForList(TypedForList),
    ForClass(TypedForClass),
    If(TypedIf),
    CallExpr(TypedCallExpr),
    InitExpr(TypedInitExpr),
    VarRefExpr(TypedVarRefExpr),
    ClassRefExpr(TypedClassRefExpr),
    BuiltinFuncRefExpr(TypedBuiltinFuncRefExpr),
    DirectFuncRef(TypedFunction),
    Immediate(TypedImmediate),
    BlockExpr(TypedBlockExpr),
    BinOp(TypedBinOp),
    PreUnOp(TypedPreUnOp),
    PostUnOp(TypedPostUnOp),
    AssignExpr(TypedAssignExpr),
    Function(TypedFunction),
    Lambda(TypedLambda),
    List(TypedList),
    Str(TypedStr),
    IndexExpr(TypedIndexExpr),
    DottedLookup(TypedDottedLookup),
    LambdaArg(TypedLambdaArg),
    RepeatedArg,
    Null,
    Break,
    Continue,
    Return(TypedReturn),
    Unreachable,
}

#[derive(Debug, Clone)]
pub struct TypedReturn {
    pub body: Box<Option<TypedExpression>>,
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
    pub place: VarLoc,
    pub name: String,
    pub is_decl: bool,
}

#[derive(Debug, Clone)]
pub struct TypedClassRefExpr {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct TypedBuiltinFuncRefExpr {
    pub idx: usize,
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
pub struct TypedForList {
    pub reference: Box<TypedExpression>,
    pub items: Box<TypedExpression>,
    pub body: Box<TypedExpression>,
    pub internal_type: Type,
}

#[derive(Debug, Clone)]
pub struct TypedForClass {
    pub reference: Box<TypedExpression>,
    pub items: Box<TypedExpression>,
    pub body: Box<TypedExpression>,
    pub iter_func: TypedFunction,
    pub iter_func_sig_idx: usize,
    pub next_func: TypedFunction,
    pub next_func_sig_idx: usize,
    pub mid_typ: Type,
    pub none_type: Type,
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
pub struct TypedInitExpr {
    pub alloc_before_call: usize,
    pub callexpr: Option<TypedCallExpr>,
}

#[derive(Debug, Clone)]
pub struct TypedMethodCallExpr {
    pub obj: Box<TypedExpression>,
    pub args: Vec<TypedExpression>,
    pub method_name: String,
}

#[derive(Debug, Clone)]
pub struct TypedDottedLookup {
    pub lhs: Box<TypedExpression>,
    pub rhs: String,
    pub index: usize,
}

#[derive(Debug, Clone)]
pub struct TypedFunctionInternal {
    pub function: parser::Function,
    pub alloc_before_call: Option<usize>,
    pub signatures: Vec<Signature>,
    pub typed_bodies: Vec<TypedExpression>,
    pub code_locations: RefCell<Vec<usize>>,
    pub locals: usize,
    pub typ: Type,
    pub is_method: bool,
}

pub type TypedFunction = Rc<RefCell<TypedFunctionInternal>>;

#[derive(Debug, Clone)]
pub struct TypedLambdaInternal {
    pub lambda: Lambda,
    pub locals: usize,

    pub signatures: Vec<Signature>,
    pub typed_bodies: Vec<TypedExpression>,
}

pub type TypedLambda = Rc<RefCell<TypedLambdaInternal>>;

#[derive(Debug, Clone)]
pub struct TypedClassDeclInternal {
    pub typ: Type,
    pub classdecl: parser::ClassDecl,
    pub methods: HashMap<String, (TypedFunction, Location)>,
}

pub type TypedClassDecl = Rc<RefCell<TypedClassDeclInternal>>;

#[derive(Debug, Clone)]
pub struct TypedObject {
    pub classdecl: TypedClassDecl,
    pub fields: Vec<Option<Type>>,
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
            type_to_obj: HashMap::new(),
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
            builtins: BuiltinFunction::get_hashmap(),
        };
        rv.openscope();
        for f in BuiltinFunction::get_table() {
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
        self.globals += self.scopes[1].len();
        p.globals = Some(self.scopes[1].len());

        Ok(TypedParseTree {
            statements: results,
            globals: self.globals,
        })
    }

    pub fn vec_of_statement(&mut self, stats: Vec<Statement>) -> Result<Vec<TypedStatement>, ()> {
        let mut is_err = false;
        let mut rv = Vec::new();
        for stat in stats {
            match self.statement(stat) {
                Ok(s) => rv.push(s),
                Err(()) => is_err = true,
            }
        }
        if is_err {
            Err(())
        } else {
            Ok(rv)
        }
    }

    pub fn vec_of_exprs(&mut self, exprs: Vec<Expression>) -> Result<Vec<TypedExpression>, ()> {
        let mut is_err = false;
        let mut rv = Vec::new();
        for expr in exprs {
            match self.expr(expr) {
                Ok(s) => rv.push(s),
                Err(()) => is_err = true,
            }
        }
        if is_err {
            Err(())
        } else {
            Ok(rv)
        }
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

    fn scope_lookup_general<T>(&self, name: &str, scopes: &[HashMap<String, T>]) -> Option<T>
    where
        T: Copy,
    {
        for scope in scopes.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(*t);
            }
        }
        None
    }

    fn scope_lookup(&self, name: &str) -> Option<(Type, VarLoc)> {
        self.scope_lookup_general(name, &self.scopes)
    }

    fn scope_lookup_func(&self, name: &str) -> Option<TypedFunction> {
        for scope in self.func_scopes.iter().rev() {
            if let Some(t) = scope.get(name) {
                return Some(t.clone());
            }
        }
        None
    }

    fn scope_lookup_class(&self, name: &str) -> Option<Type> {
        self.scope_lookup_general(name, &self.class_scopes)
    }

    fn insert_scope(&mut self, name: &str, t: Type) -> VarLoc {
        let is_global = self.scopes.len() == 2;
        let scope = self.scopes.last_mut().unwrap();
        let place = if is_global {
            VarLoc::Global(scope.len())
        } else {
            VarLoc::Local(scope.len())
        };
        scope.insert(String::from(name), (t, place));
        place
    }

    // TODO: This should likely be replaced with a function that looks up scope then replaces it in
    // that particular scope
    fn replace_cur_scope(&mut self, name: String, t: Type, place: VarLoc) {
        let scope = self.scopes.last_mut().unwrap();
        scope.entry(name).and_modify(|e| *e = (t, place));
    }

    fn insert_func_scope(&mut self, name: &str, f: TypedFunction) -> usize {
        let scope = self.func_scopes.last_mut().unwrap();
        let place = scope.len();
        scope.insert(String::from(name), f);
        place
    }

    fn insert_class_scope(&mut self, name: &str, t: Type) -> usize {
        let scope = self.class_scopes.last_mut().unwrap();
        let place = scope.len();
        scope.insert(String::from(name), t);
        place
    }

    fn openscope(&mut self) {
        self.scopes.push(HashMap::new());
        self.func_scopes.push(HashMap::new());
        self.class_scopes.push(HashMap::new());
    }

    fn closescope(&mut self) -> usize {
        self.func_scopes.pop();
        self.class_scopes.pop();
        debug_assert!(!self.scopes.is_empty());
        let rv = self.scopes.last().unwrap().len();
        self.scopes.pop();
        rv
    }

    fn statement(&mut self, s: Statement) -> Result<TypedStatement, ()> {
        Ok(match s {
            Statement::Expr(e) => TypedStatement::Expr(self.expr(e)?),
            Statement::ClassDecl(c) => self.classdecl(c)?,
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::ParseError(e) => self.error(ErrorType::ParseError(e))?,
            Statement::ExpressionError(e) => TypedStatement::Expr(self.expr(e)?),
        })
    }

    fn return_(&mut self, r: Return) -> Result<(TypedExpressionType, Type), ()> {
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

        Ok((
            TypedExpressionType::Return(TypedReturn {
                body: Box::new(body),
                from_function,
            }),
            UNREACHABLE,
        ))
    }

    fn break_(&mut self) -> Result<(TypedExpressionType, Type), ()> {
        if !self.in_loop {
            return self.error(ErrorType::BreakNotAllowed);
        }
        Ok((TypedExpressionType::Break, UNREACHABLE))
    }

    fn continue_(&mut self) -> Result<(TypedExpressionType, Type), ()> {
        if !self.in_loop {
            return self.error(ErrorType::ContinueNotAllowed);
        }
        Ok((TypedExpressionType::Continue, UNREACHABLE))
    }

    fn classdecl(&mut self, c: ClassDecl) -> Result<TypedStatement, ()> {
        let typ = self.system.class_type(c.name.clone());
        self.insert_class_scope(&c.name, typ);
        let mut methods: HashMap<String, (TypedFunction, Location)> = HashMap::new();
        for (name, (method, location)) in c.methods.iter() {
            let (meth, _methtype) = self.function(method.clone(), true)?;
            let fmeth = match meth {
                TypedExpressionType::Function(f) => f,
                _ => unreachable!(),
            };
            methods.insert(name.clone(), (fmeth, location.clone()));
        }
        let rv = Rc::new(RefCell::new(TypedClassDeclInternal {
            typ,
            classdecl: c,
            methods,
        }));
        self.type_to_class.insert(typ, rv.clone());
        Ok(TypedStatement::ClassDecl(rv))
    }

    fn expr(&mut self, e: Expression) -> Result<TypedExpression, ()> {
        self.location = Some(e.location.clone());
        let (typedexpressiontype, typ) = match e.etype {
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
            ExpressionType::Function(f) => self.function(f, false),
            ExpressionType::Lambda(l) => self.lambda(l),
            ExpressionType::List(l) => self.list(l),
            ExpressionType::IndexExpr(i) => self.index(i),
            ExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
            ExpressionType::Str(s) => self.string(s),
            ExpressionType::RepeatedArg => self.repeated_arg(),
            ExpressionType::Null => self.null(),
            ExpressionType::PossibleMethodCall(m) => self.possible_method_call(m),
            ExpressionType::ParseError(e) => self.error(ErrorType::ParseError(e))?,
            ExpressionType::Continue => self.continue_(),
            ExpressionType::Break => self.break_(),
            ExpressionType::Return(r) => self.return_(r),
        }?;
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
        let lhs = self.expr(*b.lhs);
        let mut rhs = self.expr(*b.rhs)?;
        let mut lhs = lhs?;
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
            (None, Some(typ)) => {
                let place = self.insert_scope(&r.name, typ);
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        place,
                        name: r.name,
                        is_decl: true,
                    }),
                    typ,
                ));
            }
            (Some((UNREACHABLE, place)), Some(typ)) => {
                self.replace_cur_scope(r.name.clone(), typ, place);
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        place,
                        name: r.name,
                        is_decl: true,
                    }),
                    typ,
                ));
            }
            (Some((NULL, place)), Some(typ)) => {
                let typ = self.system.option_type(typ);
                self.replace_cur_scope(r.name.clone(), typ, place);
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        place,
                        name: r.name,
                        is_decl: false,
                    }),
                    typ,
                ));
            }
            (Some((EMPTYLIST, place)), Some(typ)) if self.system.is_list(typ) => {
                self.replace_cur_scope(r.name.clone(), typ, place);
                return Ok((
                    TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                        place,
                        name: r.name,
                        is_decl: false,
                    }),
                    typ,
                ));
            }
            (Some((t, place)), _) => {
                return Ok((
                    if t == BUILTIN_FUNCTION {
                        TypedExpressionType::BuiltinFuncRefExpr(TypedBuiltinFuncRefExpr {
                            idx: self.builtins[&r.name].idx,
                        })
                    } else {
                        TypedExpressionType::VarRefExpr(TypedVarRefExpr {
                            place,
                            name: r.name,
                            is_decl: r.is_decl,
                        })
                    },
                    t,
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
            Some(func) if self.allow_raw_func => {
                Ok((TypedExpressionType::DirectFuncRef(func), UNKNOWN_RETURN))
            }
            Some(_) => self.error(ErrorType::FuncTypeUnknown(r.name)),
            None => self.error(ErrorType::NoSuchNameInScope(r.name)),
        }
    }

    fn assignment(&mut self, a: AssignExpr) -> Result<(TypedExpressionType, Type), ()> {
        let rhsloc = a.rhs.location.clone();
        let rhs = self.expr(*a.rhs);
        if !a.lhs.is_lval() {
            let _ = self.error::<()>(ErrorType::NotAssignable);
        }
        let mut has_error = false;
        let rhs = match rhs {
            Ok(r) => {
                if r.typ == UNIT {
                    let _ = self.error::<()>(ErrorType::CannotAssignUnit);
                }
                self.allow_insert = Some(r.typ);
                r
            }
            Err(()) => {
                has_error = true;
                self.allow_insert = Some(UNREACHABLE);
                TypedExpression {
                    etype: TypedExpressionType::Unreachable,
                    location: rhsloc,
                    typ: UNREACHABLE,
                }
            }
        };
        if a.op == Operation::Extract {
            if !self.system.is_option(rhs.typ) {
                return self.error(ErrorType::CannotExtractFromNonOption(
                    self.system.typename(rhs.typ),
                ));
            }
            self.allow_insert = self.system.underlying_type(rhs.typ);
        }
        let lhs = self.expr(*a.lhs);
        self.allow_insert = None;
        let lhs = lhs?;
        let rv = match self.system.lookup_assign(a.op, lhs.typ, rhs.typ) {
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
        };
        if has_error {
            Err(())
        } else {
            rv
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

    fn forexpr(&mut self, f: For) -> Result<(TypedExpressionType, Type), ()> {
        let items = self.expr(*f.items);
        if !f.reference.is_lval() {
            let _ = self.error::<()>(ErrorType::NotAssignable);
        }
        let items = items?;
        if let Some(typ) = self.system.get_typeof_list_type(items.typ) {
            let save = self.allow_insert;
            self.allow_insert = Some(typ);
            let reference = self.expr(*f.reference);
            self.allow_insert = save;
            let save_in_loop = replace(&mut self.in_loop, true);
            let body = self.expr(*f.body)?;
            self.in_loop = save_in_loop;
            Ok((
                TypedExpressionType::ForList(TypedForList {
                    body: Box::new(body),
                    items: Box::new(items),
                    reference: Box::new(reference?),
                    internal_type: typ,
                }),
                UNIT,
            ))
        } else if self.system.is_object(items.typ) {
            let classdecl = self.type_to_obj.get(&items.typ).unwrap();
            let classdecl2 = classdecl.clone();
            let cd = classdecl2.classdecl.borrow();
            let Some((iter, loc)) = cd.methods.get("__iter__") else {
                drop(cd);
                return self.error(ErrorType::CannotIterateOverTypeLacksInit(
                        self.system.typename(items.typ),
                ));
            };
            let (middle, mid_typ) = self.call_function(
                iter.clone(),
                TypedExpression {
                    etype: TypedExpressionType::DirectFuncRef(iter.clone()),
                    location: loc.clone(),
                    typ: UNKNOWN_RETURN,
                },
                vec![items.clone()],
                vec![items.typ],
            )?;

            let TypedExpressionType::CallExpr(ce) = &middle else {
                panic!("call_function did not return a TypedCallExpr");
            };
            let iter_sig_idx = ce.sig_idx.unwrap();

            let iterclassdecl = match self.type_to_obj.get(&mid_typ) {
                Some(cd) => cd.clone(),
                None => {
                    return self.error(ErrorType::CannotIterateOverTypeLacksNext(
                        self.system.typename(mid_typ),
                    ))
                }
            };
            let iterclassdecl2 = iterclassdecl;
            let cd = iterclassdecl2.classdecl.borrow();
            let Some((next, loc)) = cd.methods.get("__next__") else {
                return self.error(ErrorType::CannotIterateOverTypeLacksNext(
                        self.system.typename(mid_typ),
                ))
            };

            let (item, item_typ) = self.call_function(
                next.clone(),
                TypedExpression {
                    etype: TypedExpressionType::DirectFuncRef(next.clone()),
                    location: loc.clone(),
                    typ: UNKNOWN_RETURN,
                },
                vec![TypedExpression {
                    etype: middle,
                    location: loc.clone(),
                    typ: mid_typ,
                }],
                vec![mid_typ],
            )?;
            let TypedExpressionType::CallExpr(ce) = &item else {
                panic!("call_function did not return a TypedCallExpr");
            };
            let next_sig_idx = ce.sig_idx.unwrap();

            let save = self.allow_insert;
            if let Some(ityp) = self.system.get_typeof_option_type(item_typ) {
                self.allow_insert = Some(ityp);
            } else {
                self.allow_insert = Some(item_typ);
            }
            let reference = self.expr(*f.reference);
            self.allow_insert = save;
            let save_in_loop = replace(&mut self.in_loop, true);
            let body = self.expr(*f.body)?;
            self.in_loop = save_in_loop;
            Ok((
                TypedExpressionType::ForClass(TypedForClass {
                    items: Box::new(items),
                    body: Box::new(body),
                    reference: Box::new(reference?),
                    iter_func: iter.clone(),
                    iter_func_sig_idx: iter_sig_idx,
                    next_func: next.clone(),
                    next_func_sig_idx: next_sig_idx,
                    mid_typ,
                    none_type: item_typ,
                }),
                UNIT,
            ))
        } else {
            self.error(ErrorType::CannotIterateOverType(
                self.system.typename(items.typ),
            ))
        }
    }

    fn ifexpr(&mut self, i: If) -> Result<(TypedExpressionType, Type), ()> {
        let (tet, typ) = self.ifexpr_int(i, false)?;
        Ok((TypedExpressionType::If(tet), typ))
    }

    fn ifexpr_int(&mut self, i: If, is_and_if: bool) -> Result<(TypedIf, Type), ()> {
        let mut make_option = false;
        let mut return_unit = i.else_body.is_none();
        let cond = self.expr(*i.condition)?;
        if cond.typ != BOOL && cond.typ != UNKNOWN_RETURN {
            self.error(ErrorType::IfConditionNotBool(
                self.system.typename(cond.typ),
            ))?;
        }

        let body = self.expr(*i.body);
        let and_bodies_res: Vec<(_, Location)> = i
            .and_bodies
            .into_iter()
            .map(|(b, l)| (self.ifexpr_int(b, true), l))
            .collect();
        let else_body_opt_res = i.else_body.map(|b| self.expr(*b));

        let else_body = if let Some(r) = else_body_opt_res {
            Some(r?)
        } else {
            None
        };
        let mut and_bodies = Vec::new();
        for (b, l) in and_bodies_res {
            and_bodies.push((b?, l));
        }
        let body = body?;
        let mut rvtyp = body.typ;

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
            } else if typ == UNKNOWN_RETURN {
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
            // TODO: This can cause some awkwardness with else if statements
            // e.g
            // `if false null else if true null else 1`
            // will return an Option<Option<int>> rather than an Option<int>
            rvtyp = self.system.option_type(rvtyp);
            ti.makes_option = Some(rvtyp);
        }
        Ok((ti, rvtyp))
    }

    fn list(&mut self, l: List) -> Result<(TypedExpressionType, Type), ()> {
        let mut interior_type = UNREACHABLE;
        let exprs = self.vec_of_exprs(l.exprs)?;

        for te in exprs.iter() {
            if interior_type == UNREACHABLE {
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

    fn function(
        &mut self,
        f: parser::Function,
        is_method: bool,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let name = f.name.clone();
        let default = "<anonymous function>".to_owned();
        let typ = self.system.function_type(name.clone().unwrap_or(default));
        let rv = Rc::new(RefCell::new(TypedFunctionInternal {
            function: f,
            alloc_before_call: None,
            signatures: Vec::new(),
            typed_bodies: Vec::new(),
            code_locations: RefCell::new(Vec::new()),
            locals: 0,
            typ,
            is_method,
        }));
        self.type_to_func.insert(typ, rv.clone());
        if !is_method {
            if let Some(s) = name {
                self.insert_func_scope(&s, rv.clone());
            };
        }
        Ok((TypedExpressionType::Function(rv), typ))
    }

    fn lambda(&mut self, l: parser::Lambda) -> Result<(TypedExpressionType, Type), ()> {
        let typ = self.system.function_type("<lambda>".to_string());
        let rv = Rc::new(RefCell::new(TypedLambdaInternal {
            lambda: l,
            locals: 0,
            signatures: Vec::new(),
            typed_bodies: Vec::new(),
        }));
        self.type_to_lambda.insert(typ, rv.clone());
        Ok((TypedExpressionType::Lambda(rv), typ))
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
        let save = self.allow_raw_func;
        self.allow_raw_func = true;
        let lhs = self.expr(*m.lhs.clone())?;
        self.allow_raw_func = save;
        let Some(objref) = self.type_to_obj.get_mut(&lhs.typ) else {
            return self.error(ErrorType::ClassHasNoFieldOrMethod(self.system.typename(lhs.typ) , m.method_name));
        };
        let cd = objref.classdecl.borrow();
        if let Some((func, loc)) = cd.methods.get(&m.method_name).cloned() {
            drop(cd);
            let mut args = self.vec_of_exprs(m.args)?;
            let mut argtypes: Vec<Type> = args.iter().map(|e| e.typ).collect();
            argtypes.insert(0, lhs.typ);
            args.insert(0, lhs.clone());
            let typ = func.borrow().typ;
            let subject = TypedExpression {
                etype: TypedExpressionType::DirectFuncRef(func.clone()),
                location: loc,
                typ,
            };
            self.call_function(func, subject, args, argtypes)
        } else if cd.classdecl.fields.contains_key(&m.method_name) {
            let expr = parser::CallExpr {
                function: Box::new(parser::Expression {
                    etype: parser::ExpressionType::DottedLookup(parser::DottedLookup {
                        lhs: m.lhs,
                        rhs: m.method_name,
                    }),
                    location: m.location,
                }),
                args: m.args,
            };
            drop(cd);
            self.call(expr)
        } else {
            drop(cd);
            self.error(ErrorType::ClassHasNoFieldOrMethod(
                self.system.typename(lhs.typ),
                m.method_name,
            ))
        }
    }

    fn call(&mut self, c: CallExpr) -> Result<(TypedExpressionType, Type), ()> {
        let save = self.allow_raw_func;
        self.allow_raw_func = true;
        let functype = self.expr(*c.function.clone());
        self.allow_raw_func = save;

        // let mut funcloc = c.function.location.clone();
        let args = self.vec_of_exprs(c.args)?;
        let argtypes: Vec<Type> = args.iter().map(|e| e.typ).collect();
        let subject = functype?;

        // let mut override_return = None;
        if self.system.is_class(subject.typ) {
            return self.alloc_and_initialize(subject, args, argtypes);
        }

        if !self.system.is_function(subject.typ) {
            return self.error(ErrorType::NotCallable(self.system.typename(subject.typ)));
        }

        if subject.typ == BUILTIN_FUNCTION {
            let ExpressionType::RefExpr(refexpr) =  &c.function.as_ref().etype else {
                return self.error(ErrorType::CannotIndirectlyCallBuiltins);
            };
            let funcname = refexpr.name.clone();
            let builtins = BuiltinFunction::get_hashmap();
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

        if let TypedExpressionType::DirectFuncRef(f) = &subject.etype {
            self.call_function(f.clone(), subject, args, argtypes)
        } else if let Some(func) = self.type_to_func.get(&subject.typ) {
            self.call_function(func.clone(), subject, args, argtypes)
        } else if let Some(lambda) = self.type_to_lambda.get(&subject.typ) {
            self.call_lambda_with_args(lambda.clone(), subject, args)
        } else {
            todo!()
        }
    }

    fn alloc_and_initialize(
        &mut self,
        mut subject: TypedExpression,
        mut args: Vec<TypedExpression>,
        mut argtypes: Vec<Type>,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let classdecl = self.type_to_class.get(&subject.typ).unwrap().clone();
        let to_alloc = classdecl.borrow().classdecl.fields.len();
        let cdc = classdecl.clone();
        let cd = cdc.borrow();
        let Some((init, location)) = cd.methods.get("__init__") else {
            return Ok((
                    TypedExpressionType::InitExpr(TypedInitExpr {
                        alloc_before_call: to_alloc,
                        callexpr: None,
                    }), subject.typ
            ));
        };
        let rvtype = self.system.class_variant(subject.typ);
        subject.typ = rvtype;
        let num_field = cd.classdecl.fields.len();
        self.type_to_obj.insert(
            subject.typ,
            TypedObject {
                classdecl,
                fields: vec![None; num_field],
            },
        );
        argtypes.insert(0, subject.typ);
        args.insert(0, subject);
        let new_subject = TypedExpression {
            location: location.clone(),
            typ: UNIT,
            etype: TypedExpressionType::DirectFuncRef(init.clone()),
        };
        let (tet, _typ) =
            self.call_function(init.clone(), new_subject, args.clone(), argtypes.clone())?;

        let TypedExpressionType::CallExpr(tce) = tet else {
            panic!("weird return type {:?}", tet);
        };

        Ok((
            TypedExpressionType::InitExpr(TypedInitExpr {
                alloc_before_call: to_alloc,
                callexpr: Some(tce),
            }),
            rvtype,
        ))
    }

    fn call_function(
        &mut self,
        f: TypedFunction,
        subject: TypedExpression,
        mut args: Vec<TypedExpression>,
        mut argtypes: Vec<Type>,
    ) -> Result<(TypedExpressionType, Type), ()> {
        let mut b = match f.try_borrow_mut() {
            Ok(a) => a,
            Err(_) => {
                // This might already be borrowed if we are in a recurisive context
                // This is ok but we can't derive the signature
                self.need_recheck = true;
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args: args.clone(),
                        sig_idx: None,
                    }),
                    UNKNOWN_RETURN,
                ));
            }
        };

        self.function_fixup_args(&mut b, &mut args, &mut argtypes)?;

        for (i, sig) in b.signatures.iter().enumerate() {
            if sig.inputs == argtypes {
                let out = sig.output;
                drop(b);
                return Ok((
                    TypedExpressionType::CallExpr(TypedCallExpr {
                        function: Box::new(subject),
                        args: args.clone(),
                        sig_idx: Some(i),
                    }),
                    out,
                ));
            }
        }
        let calling_location = self.location.clone().unwrap();
        let te = self.call_function_int(&mut b, &args, &calling_location)?;

        let func_sig = if b.function.repeated.is_none() {
            Signature::new(argtypes, None, te.typ)
        } else {
            let num = b.function.argnames.len();
            Signature::new(
                argtypes.iter().take(num).cloned().collect(),
                argtypes.last().cloned(),
                te.typ,
            )
        };

        let sig_idx = b.signatures.len();
        b.typed_bodies.push(te.clone());
        b.code_locations.borrow_mut().push(0);
        b.signatures.push(func_sig);

        Ok((
            TypedExpressionType::CallExpr(TypedCallExpr {
                function: Box::new(subject),
                args,
                sig_idx: Some(sig_idx),
            }),
            te.typ,
        ))
    }

    fn function_fixup_args(
        &mut self,
        f: &mut TypedFunctionInternal,
        args: &mut Vec<TypedExpression>,
        argtypes: &mut Vec<Type>,
    ) -> Result<(), ()> {
        if f.function.repeated.is_some() {
            return Ok(());
        }
        let argnamelen = f.function.argnames.len();
        if argnamelen < args.len() {
            return self.error(ErrorType::WrongNumberOfArguments(argnamelen, args.len()));
        }
        let num_default_needed = argnamelen - args.len();
        if num_default_needed > f.function.default_args.len() {
            return self.error(ErrorType::WrongNumberOfArgumentsWithDefault(
                args.len(),
                f.function.default_args.len(),
                f.function.argnames.len(),
            ));
        }
        let first_default = f.function.default_args.len() - num_default_needed;
        let mut def_args = Vec::new();
        for i in 0..num_default_needed {
            let expr = f.function.default_args[i + first_default].clone();
            def_args.push(self.expr(expr));
        }
        for e in def_args {
            let e = e?;
            argtypes.push(e.typ);
            args.push(e);
        }

        let len = argtypes.len();
        let def_args_all = self.vec_of_exprs(f.function.default_args.clone())?;
        for (i, def) in def_args_all.iter().rev().enumerate() {
            if self.system.is_option(def.typ) && argtypes[len - 1 - i] != def.typ {
                let typ = argtypes[len - 1 - i];
                let newtype = self.system.option_type(typ);
                argtypes[len - 1 - i] = newtype;
                args[len - 1 - i].typ = newtype;
            }
        }

        Ok(())
    }

    fn call_function_int(
        &mut self,
        function: &mut TypedFunctionInternal,
        args: &[TypedExpression],
        calling_location: &Location,
    ) -> Result<TypedExpression, ()> {
        let save = self.need_recheck;
        self.need_recheck = false;
        let (rv, num_locals) = self.call_function_single_check(function, args, calling_location)?;
        function.locals = num_locals;

        if self.need_recheck {
            let (new_rv, num_locals) =
                self.call_function_single_check(function, args, calling_location)?;
            if new_rv.typ != rv.typ {
                todo!();
                // return self.error_with_origin(ErrorType::MismatchedReturnTypes(
                // self.system.typename(new_rv.typ),
                // self.system.typename(rv.typ),
                // ));
            }
            debug_assert_eq!(function.locals, num_locals);
            self.need_recheck = save;
            Ok(new_rv)
        } else {
            self.need_recheck = save;
            Ok(rv)
        }
    }

    fn call_function_single_check(
        &mut self,
        function: &mut TypedFunctionInternal,
        args: &[TypedExpression],
        calling_location: &Location,
    ) -> Result<(TypedExpression, usize), ()> {
        self.openscope();
        if let Some(name) = function.function.repeated.clone() {
            if args.len() > function.function.argnames.len() {
                self.repeated_arg = Some((name, args.last().unwrap().typ));
            } else {
                self.repeated_arg = Some((name, 0));
            }
        }
        for (typ, name) in args.iter().zip(function.function.argnames.iter()) {
            self.insert_scope(name, typ.typ);
        }
        let rv = self.call_single_check(*function.function.body.clone(), calling_location)?;
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

    fn call_lambda_with_args(
        &mut self,
        l: TypedLambda,
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

    fn call_lambda(
        &mut self,
        lambda: TypedLambda,
        mut args: Vec<TypedExpression>,
        funcloc: &Location,
    ) -> Result<(TypedExpression, Option<usize>), ()> {
        swap(&mut self.lambda_args, &mut args);
        let argtypes = args.iter().map(|a| a.typ).collect();
        self.openscope();
        self.func_returns.push(None);
        let rv = self.call_single_check(*lambda.borrow_mut().lambda.body.clone(), funcloc)?;
        let func_sig = Signature::new(argtypes, None, rv.typ);
        let idx = lambda.borrow().signatures.len();
        lambda.borrow_mut().typed_bodies.push(rv.clone());
        lambda.borrow_mut().signatures.push(func_sig);
        lambda.borrow_mut().locals = self.closescope() + args.len();
        swap(&mut self.lambda_args, &mut args);
        Ok((rv, Some(idx)))
    }

    fn dotted_lookup(&mut self, d: DottedLookup) -> Result<(TypedExpressionType, Type), ()> {
        let lhs = self.expr(*d.lhs)?;
        let Some(objref) = self.type_to_obj.get_mut(&lhs.typ) else {
            return self.error(ErrorType::ClassHasNoFieldOrMethod(self.system.typename(lhs.typ) , d.rhs));
        };
        let cd = objref.classdecl.borrow();
        let Some(obj_idx) = cd.classdecl.fields.get(&d.rhs).copied() else {
            drop(cd);
            return self.error(ErrorType::ClassHasNoFieldOrMethod(self.system.typename(lhs.typ) , d.rhs));
        };
        let field_type = objref.fields[obj_idx];
        let typ = match (field_type, self.allow_insert) {
            (None, Some(ai)) | (Some(UNREACHABLE), Some(ai)) => {
                objref.fields[obj_idx] = Some(ai);
                ai
            }
            (Some(NULL), Some(ai)) => {
                objref.fields[obj_idx] = Some(self.system.option_type(ai));
                ai
            }
            (Some(EMPTYLIST), Some(ai)) if self.system.is_list(ai) => {
                objref.fields[obj_idx] = Some(ai);
                ai
            }
            (Some(ft), _) => ft,
            (None, None) => {
                drop(cd);
                return self.error(ErrorType::UninitializedAssignment);
            }
        };
        Ok((
            TypedExpressionType::DottedLookup(TypedDottedLookup {
                lhs: Box::new(lhs),
                rhs: d.rhs,
                index: obj_idx,
            }),
            typ,
        ))
    }
}
