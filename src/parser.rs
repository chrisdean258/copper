use crate::{
    lex::*, location::Location, operation::Operation, typesystem::Signature, typesystem::Type,
    value::Value,
};
use std::{cell::RefCell, collections::HashMap, iter::Peekable, mem::swap, rc::Rc};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Statement {
    Expr(Expression),
    ClassDecl(Rc<RefCell<ClassDecl>>),
    Import(Import),
    FromImport(FromImport),
    Continue(Continue),
    Return(Return),
    Break(Break),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub etype: ExpressionType,
    pub location: Location,
    pub derived_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum ExpressionType {
    While(While),
    For(For),
    If(If),
    CallExpr(CallExpr),
    PossibleMethodCall(PossibleMethodCall),
    RefExpr(RefExpr),
    Immediate(Immediate),
    BlockExpr(BlockExpr),
    BinOp(BinOp),
    PreUnOp(PreUnOp),
    PostUnOp(PostUnOp),
    AssignExpr(AssignExpr),
    Function(Rc<RefCell<Function>>),
    Lambda(Rc<RefCell<Lambda>>),
    List(List),
    Str(Str),
    IndexExpr(IndexExpr),
    DottedLookup(DottedLookup),
    LambdaArg(LambdaArg),
    FuncRefExpr(FuncRefExpr),
    RepeatedArg,
    Null,
}

impl Expression {
    pub fn is_lval(&self) -> bool {
        use ExpressionType::*;
        matches!(self.etype, RefExpr(_) | IndexExpr(_) | DottedLookup(_))
    }
}

#[derive(Debug, Clone)]
pub struct ParseTree {
    pub statements: Vec<Statement>,
    max_arg: Vec<usize>,
    loop_count: usize,
    repeated_arg: Option<String>,
    pub globals: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub location: Location,
    pub filename: String,
}

#[derive(Debug, Clone)]
pub struct FromImport {
    pub location: Location,
    pub file: String,
    pub what: String,
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub location: Location,
    pub name: String,
    pub fields: HashMap<String, usize>,
    pub methods: HashMap<String, Expression>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub reference: Box<Expression>,
    pub items: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub and_bodies: Vec<(If, Location)>,
    pub else_body: Option<Box<Expression>>,
    pub makes_option: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
    pub alloc_before_call: Option<usize>,
    pub method_name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct PossibleMethodCall {
    pub lhs: Box<Expression>,
    pub method_name: String,
    pub args: Vec<Expression>,
    pub alloc_before_call: Option<usize>,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct IndexExpr {
    pub obj: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub name: String,
    pub is_decl: bool,
}

#[derive(Debug, Clone)]
pub struct FuncRefExpr {
    pub name: String,
    pub sig: Signature,
}

#[derive(Debug, Clone)]
pub struct LambdaArg {
    pub number: usize,
}

#[derive(Debug, Clone)]
pub struct Immediate {
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Box<Expression>,
    pub op: Operation,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub lhs: Box<Expression>,
    pub op: Operation,
    pub rhs: Box<Expression>,
    pub allow_decl: bool,
}

#[derive(Debug, Clone)]
pub struct Deref {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct PreUnOp {
    pub op: Operation,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PostUnOp {
    pub lhs: Box<Expression>,
    pub op: Operation,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub argnames: Vec<String>,
    pub repeated: Option<String>,
    pub body: Box<Expression>,
    pub name: Option<String>,
    pub default_args: Vec<Expression>,
    pub locals: Option<usize>,
    pub alloc_before_call: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub num_args: usize,
    pub body: Box<Expression>,
    pub locals: Option<usize>,
}

#[derive(Debug, Clone)]
pub struct List {
    pub exprs: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Str {
    pub string: String,
}

#[derive(Debug, Clone)]
pub struct DottedLookup {
    pub lhs: Box<Expression>,
    pub rhs: String,
    pub index: Option<usize>,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Break {
    location: Location,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub location: Location,
    pub body: Option<Box<Expression>>,
    pub from_function: bool,
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub location: Location,
}

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedEOF(&'static str),
    UnexpectedToken(Token, Option<&'static str>),
    ContinueNotAllowed(Location),
    BreakNotAllowed(Location),
    MethodRedefinition(Location, String),
    FieldRedefinition(Location, String),
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedEOF(c) => write!(f, "Unexpected EOF while parsing {c}"),
            Self::UnexpectedToken(t, expt) => {
                write!(f, "{}: Unexpected `{}`.", t.location, t.token_type)?;
                if let Some(expected) = expt {
                    write!(f, " Expected `{}`.", expected)?;
                }
                Ok(())
            }
            Self::ContinueNotAllowed(l) => {
                write!(f, "{}: `continue` not allowed here", l)
            }
            Self::BreakNotAllowed(l) => {
                write!(f, "{}: `break` not allowed here", l)
            }
            Self::MethodRedefinition(l, n) => {
                write!(f, "{}: Redefinition of method `{}`", l, n)
            }
            Self::FieldRedefinition(l, n) => {
                write!(f, "{}: Redefinition of field `{}`", l, n)
            }
        }
    }
}

fn unexpected(token: Token) -> Error {
    Error::UnexpectedToken(token, None)
}

fn unexpect_known(token: Token, expt: &'static str) -> Error {
    Error::UnexpectedToken(token, Some(expt))
}

macro_rules! binop {
    ( $name:ident, $next:ident, $( $token:ident ),+) => {
        fn $name(
            &mut self,
            lexer: &mut Peekable<Lexer>,
            ) -> Result<Expression, Error> {
            let mut lhs = self.$next(lexer)?;
            loop {
                let token = match lexer.peek() {
                    Some(t) => t.clone(),
                    None => return Ok(lhs),
                };
                let optype = match token.token_type {
                    $( TokenType::$token => { lexer.next(); Operation::$token}, )+
                        _ => return Ok(lhs),
                };
                let rhs = self.$next(lexer)?;
                lhs = Expression {
                    derived_type: None,
                    location: token.location,
                    etype:ExpressionType::BinOp(BinOp {
                    lhs: Box::new(lhs),
                    op: optype,
                    rhs: Box::new(rhs),
                })}
            }
        }
    }
}

macro_rules! expect {
    ( $lexer:ident, $token:path, $context:expr ) => {{
        let token = $lexer.next().ok_or(Error::UnexpectedEOF($context))?;
        match &token.token_type {
            $token => token,
            _ => return Err(unexpect_known(token, stringify!($token))),
        }
    }};
}

macro_rules! expect_val {
    ( $lexer:ident, $token:path, $context:expr ) => {{
        let token = $lexer.next().ok_or(Error::UnexpectedEOF($context))?;
        match token.token_type {
            $token(v) => v,
            _ => return Err(unexpect_known(token, stringify!($token))),
        }
    }};
}

macro_rules! if_expect {
    ( $lexer:ident, $($token:path),* $(,)? ) => {
        if let Some(token) = $lexer.peek() {
            match &token.token_type {
                $($token => {
                    $lexer.next();
                    true
                })*
                _ => false,
            }
        } else {
            false
        }
    };
}

impl ParseTree {
    pub fn new() -> ParseTree {
        ParseTree {
            statements: Vec::new(),
            max_arg: Vec::new(),
            loop_count: 0,
            repeated_arg: None,
            globals: None,
        }
    }

    pub fn parse_statements(&mut self, lexer: Lexer) -> Result<(), Error> {
        let mut peekable = lexer.peekable();
        while peekable.peek().is_some() {
            let statement = self.parse_statement(&mut peekable);
            self.statements.push(statement?);
        }
        Ok(())
    }

    fn parse_statement(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let token = lexer.peek().ok_or(Error::UnexpectedEOF("statement"))?;
        let rv = Ok(match &token.token_type {
            TokenType::Class => self.parse_class_decl(lexer)?,
            TokenType::Import => self.parse_import(lexer)?,
            TokenType::From => self.parse_from_import(lexer)?,
            TokenType::Break => self.parse_break(lexer)?,
            TokenType::Continue => self.parse_continue(lexer)?,
            TokenType::Return => self.parse_return(lexer)?,
            _ => Statement::Expr(self.parse_expr(lexer)?),
        });
        // eat all the semicolons
        while if_expect!(lexer, TokenType::Semicolon) {}
        rv
    }

    fn parse_continue(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::Continue, "continue").location;
        if self.loop_count == 0 {
            return Err(Error::ContinueNotAllowed(location));
        }
        Ok(Statement::Continue(Continue { location }))
    }

    fn parse_return(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::Return, "return").location;
        let body = if if_expect!(lexer, TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expr(lexer)?))
        };
        Ok(Statement::Return(Return {
            location,
            body,
            from_function: true,
        }))
    }

    fn parse_break(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::Break, "break").location;
        if self.loop_count == 0 {
            return Err(Error::BreakNotAllowed(location));
        }
        Ok(Statement::Break(Break { location }))
    }

    fn parse_import(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::Import, "import").location;
        let filename = expect_val!(lexer, TokenType::Identifier, "import");
        Ok(Statement::Import(Import { location, filename }))
    }

    fn parse_from_import(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::From, "from import").location;
        let file = expect_val!(lexer, TokenType::Identifier, "from import");
        expect!(lexer, TokenType::Import, "from import");
        let what = expect_val!(lexer, TokenType::Identifier, "from import");
        Ok(Statement::FromImport(FromImport {
            location,
            file,
            what,
        }))
    }

    fn parse_class_decl(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Statement, Error> {
        let location = expect!(lexer, TokenType::Class, "class declaraction").location;
        let name = expect_val!(lexer, TokenType::Identifier, "class declaration");
        expect!(lexer, TokenType::OpenBrace, "class declaration");

        let mut fields = HashMap::new();
        let mut methods = HashMap::new();
        loop {
            let token = lexer
                .peek()
                .ok_or(Error::UnexpectedEOF("class declaration"))?;
            match token.token_type {
                TokenType::CloseBrace => {
                    expect!(lexer, TokenType::CloseBrace, "class declaration");
                    break;
                }
                TokenType::Function => {
                    let fun = self.parse_function(lexer, true)?;
                    let fname = match &fun.etype {
                        ExpressionType::Function(f) => f.borrow().name.clone().unwrap(),
                        _ => unreachable!(),
                    };
                    if methods.insert(fname.clone(), fun.clone()).is_some() {
                        return Err(Error::MethodRedefinition(location, fname));
                    }
                }
                TokenType::Field => {
                    expect!(lexer, TokenType::Field, "fields");
                    loop {
                        let token = lexer.next().ok_or(Error::UnexpectedEOF("fields"))?;
                        let fieldname = match token.token_type {
                            TokenType::Identifier(s) => s,
                            _ => break,
                        };
                        if fields.insert(fieldname.clone(), fields.len()).is_some() {
                            return Err(Error::FieldRedefinition(location, fieldname));
                        }
                        match lexer
                            .peek()
                            .ok_or(Error::UnexpectedEOF("fields"))?
                            .token_type
                        {
                            TokenType::Semicolon => {
                                lexer.next();
                                break;
                            }
                            TokenType::Comma => {
                                lexer.next();
                            }
                            _ => break,
                        }
                    }
                }
                _ => return Err(unexpected(token.clone())),
            }
        }

        Ok(Statement::ClassDecl(Rc::new(RefCell::new(ClassDecl {
            location,
            name,
            fields,
            methods,
        }))))
    }

    fn parse_block(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let mut rv = Vec::new();
        let location = expect!(lexer, TokenType::OpenBrace, "block").location;

        while !if_expect!(lexer, TokenType::CloseBrace) {
            let statement = self.parse_statement(lexer);
            rv.push(statement?);
        }
        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::BlockExpr(BlockExpr { statements: rv }),
        })
    }

    fn parse_for(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let location = expect!(lexer, TokenType::For, "for loop").location;
        let reference = Box::new(self.parse_ref(lexer)?);
        expect!(lexer, TokenType::In, "for loop");
        let items = Box::new(self.parse_expr(lexer)?);
        self.loop_count += 1;
        let body = Box::new(self.parse_expr(lexer)?);
        self.loop_count -= 1;

        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::For(For {
                reference,
                items,
                body,
            }),
        })
    }

    fn parse_while(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let location = expect!(lexer, TokenType::While, "while loop").location;
        let condition = Box::new(self.parse_expr(lexer)?);
        self.loop_count += 1;
        let body = Box::new(self.parse_expr(lexer)?);
        self.loop_count -= 1;

        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::While(While { condition, body }),
        })
    }

    fn parse_if(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        use crate::lex::TokenType::*;
        let (mut fif, location) = self.parse_if_internal(lexer)?;

        while let Some(token) = lexer.peek() {
            match token.token_type {
                And => {
                    fif.and_bodies.push(self.parse_and(lexer)?);
                }
                Else => {
                    fif.else_body = Some(Box::new(self.parse_else(lexer)?));
                    break;
                }
                _ => break,
            }
        }

        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::If(fif),
        })
    }

    fn parse_and(&mut self, lexer: &mut Peekable<Lexer>) -> Result<(If, Location), Error> {
        expect!(lexer, TokenType::And, "and if statement");
        self.parse_if_internal(lexer)
    }

    fn parse_if_internal(&mut self, lexer: &mut Peekable<Lexer>) -> Result<(If, Location), Error> {
        use crate::lex::TokenType::If as tokenIf;
        let location = expect!(lexer, tokenIf, "if statement").location;
        let condition = Box::new(self.parse_expr(lexer)?);
        let body = Box::new(self.parse_expr(lexer)?);
        Ok((
            If {
                condition,
                body,
                and_bodies: Vec::new(),
                else_body: None,
                makes_option: None,
            },
            location,
        ))
    }

    fn parse_else(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        expect!(lexer, TokenType::Else, "else statement");

        let token = lexer.peek().ok_or(Error::UnexpectedEOF("else statement"))?;
        match &token.token_type {
            TokenType::If => self.parse_if(lexer),
            TokenType::OpenBrace => self.parse_block(lexer),
            _ => self.parse_expr(lexer),
        }
    }

    fn parse_eq(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        use crate::lex::TokenType::*;
        let lhs = self.parse_boolean_op(lexer)?;

        let mut token = match lexer.peek() {
            Some(t) => t.clone(),
            None => return Ok(lhs),
        };
        let mut allow_decl = false;
        let reflhs = match &mut token.token_type {
            Equal => {
                lexer.next();
                allow_decl = true;
                match lhs.etype {
                    ExpressionType::RefExpr(_) => lhs,
                    ExpressionType::IndexExpr(_) => lhs,
                    ExpressionType::DottedLookup(_) => lhs,
                    _ => return Err(unexpected(token)),
                }
            }
            AndEq | XorEq | OrEq | PlusEq | MinusEq | TimesEq | DivEq | ModEq => match lhs.etype {
                ExpressionType::RefExpr(_) => {
                    lexer.next();
                    lhs
                }
                ExpressionType::IndexExpr(_) => {
                    lexer.next();
                    lhs
                }
                _ => return Err(unexpected(token)),
            },
            _ => return Ok(lhs),
        };
        let rhs = self.parse_eq(lexer)?;
        let op = match token.token_type {
            TokenType::Equal => Operation::Equal,
            TokenType::AndEq => Operation::AndEq,
            TokenType::XorEq => Operation::XorEq,
            TokenType::OrEq => Operation::OrEq,
            TokenType::PlusEq => Operation::PlusEq,
            TokenType::MinusEq => Operation::MinusEq,
            TokenType::TimesEq => Operation::TimesEq,
            TokenType::DivEq => Operation::DivEq,
            TokenType::ModEq => Operation::ModEq,
            TokenType::BitShiftRightEq => Operation::BitShiftRightEq,
            TokenType::BitShiftLeftEq => Operation::BitShiftLeftEq,
            _ => unreachable!(),
        };
        Ok(Expression {
            derived_type: None,
            location: token.location,
            etype: ExpressionType::AssignExpr(AssignExpr {
                lhs: Box::new(reflhs),
                op,
                rhs: Box::new(rhs),
                allow_decl,
            }),
        })
    }

    binop! {parse_boolean_op, parse_bitwise_or, BoolOr, BoolXor, BoolAnd}
    binop! {parse_bitwise_or, parse_bitwise_xor, BitOr}
    binop! {parse_bitwise_xor, parse_bitwise_and, BitXor}
    binop! {parse_bitwise_and, parse_comparision, BitAnd}

    // Todo: Comparision operator chaining
    binop! {parse_comparision, parse_bitshift, CmpGE, CmpGT, CmpLE, CmpLT, CmpEq, CmpNotEq }

    binop! {parse_bitshift, parse_additive, BitShiftLeft, BitShiftRight}
    binop! {parse_additive, parse_multiplicative, Minus, Plus}
    binop! {parse_multiplicative, parse_pre_unary, Times, Mod, Div}

    fn parse_pre_unary(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        if let Some(token) = lexer.peek() {
            let mut needs_ref = false;
            let optype = match token.token_type {
                TokenType::BoolNot => Operation::BoolNot,
                TokenType::BitNot => Operation::BitNot,
                TokenType::Minus => Operation::Negate,
                TokenType::Plus => Operation::UnaryPlus,
                TokenType::Times => Operation::Deref,
                TokenType::Inc => {
                    needs_ref = true;
                    Operation::PreInc
                }
                TokenType::Dec => {
                    needs_ref = true;
                    Operation::PreDec
                }
                _ => return self.parse_post_unary(lexer),
            };
            let token = lexer
                .next()
                .ok_or(Error::UnexpectedEOF("pre-unary expression"))?;
            let rhs = self.parse_pre_unary(lexer)?;
            if needs_ref {
                match rhs.etype {
                    ExpressionType::RefExpr(_) => (),
                    _ => return Err(unexpected(token)),
                };
            }
            if let ExpressionType::RefExpr(r) = &rhs.etype {
                if Some(&r.name) == self.repeated_arg.as_ref() && optype == Operation::Deref {
                    return Ok(Expression {
                        derived_type: None,
                        location: token.location,
                        etype: ExpressionType::RepeatedArg,
                    });
                }
            }
            Ok(Expression {
                derived_type: None,
                location: token.location,
                etype: ExpressionType::PreUnOp(PreUnOp {
                    op: optype,
                    rhs: Box::new(rhs),
                }),
            })
        } else {
            self.parse_post_unary(lexer)
        }
    }

    fn parse_post_unary(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let mut lhs = self.parse_ref(lexer)?;
        while let Some(token) = lexer.peek().cloned() {
            lhs = match token.token_type {
                TokenType::Inc => Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::PostUnOp(PostUnOp {
                        lhs: if lhs.is_lval() {
                            Box::new(lhs)
                        } else {
                            return Err(unexpected(token));
                        },
                        op: Operation::PostInc,
                    }),
                },
                TokenType::Dec => Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::PostUnOp(PostUnOp {
                        lhs: if lhs.is_lval() {
                            Box::new(lhs)
                        } else {
                            return Err(unexpected(token));
                        },
                        op: Operation::PostDec,
                    }),
                },
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer, "function call")?;
                    Expression {
                        derived_type: None,
                        location: token.location,
                        etype: ExpressionType::CallExpr(CallExpr {
                            function: Box::new(lhs),
                            args,
                            alloc_before_call: None,
                            method_name: None,
                        }),
                    }
                }
                TokenType::OpenBracket => {
                    let location =
                        expect!(lexer, TokenType::OpenBracket, "index expression").location;
                    let args = self.parse_cse(lexer, "index expression")?;
                    expect!(lexer, TokenType::CloseBracket, "index expression").location;
                    Expression {
                        derived_type: None,
                        location,
                        etype: ExpressionType::IndexExpr(IndexExpr {
                            obj: Box::new(lhs),
                            args,
                        }),
                    }
                }
                _ => return Ok(lhs),
            }
        }
        Ok(lhs)
    }

    fn parse_function(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        must_be_named: bool,
    ) -> Result<Expression, Error> {
        let save_loop = self.loop_count;
        self.loop_count = 0;
        let rv = self.parse_function_impl(lexer, must_be_named);
        self.loop_count = save_loop;
        rv
    }

    fn parse_function_impl(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        must_be_named: bool,
    ) -> Result<Expression, Error> {
        let loctoken = expect!(lexer, TokenType::Function, "function");
        let token = lexer.peek().ok_or(Error::UnexpectedEOF("function"))?;
        let name = match &token.token_type {
            TokenType::Identifier(s) => {
                let rv = s.clone();
                lexer.next();
                Some(rv)
            }
            _ if must_be_named => return Err(unexpected(token.clone())),
            _ => None,
        };
        expect!(lexer, TokenType::OpenParen, "function");
        let mut args = Vec::new();
        let mut repeated = None;
        let mut default_args = Vec::new();
        let mut needs_default = false;
        let mut last = 0;
        loop {
            let token = lexer.next().ok_or(Error::UnexpectedEOF("function"))?;
            match &token.token_type {
                TokenType::Identifier(s) if last == 0 => args.push(s.clone()),
                TokenType::Identifier(s) if last == 1 => {
                    last = 2;
                    repeated = Some(s.clone());
                }
                TokenType::Times => {
                    last = 1;
                    continue;
                }
                TokenType::CloseParen => break,
                _ => return Err(unexpected(token)),
            }
            if needs_default {
                expect!(lexer, TokenType::Equal, "default argument");
                default_args.push(self.parse_expr(lexer)?);
            }
            let token = lexer.next().ok_or(Error::UnexpectedEOF("argument"))?;
            match &token.token_type {
                TokenType::Comma => (),
                TokenType::CloseParen => break,
                TokenType::Equal if !needs_default => {
                    needs_default = true;
                    default_args.push(self.parse_expr(lexer)?);
                    let token = lexer.next().ok_or(Error::UnexpectedEOF("arguements"))?;
                    match &token.token_type {
                        TokenType::Comma => (),
                        TokenType::CloseParen => break,
                        _ => return Err(unexpected(token)),
                    }
                }
                _ => return Err(unexpected(token)),
            }
        }
        swap(&mut self.repeated_arg, &mut repeated);
        let body = self.parse_expr(lexer)?;
        swap(&mut self.repeated_arg, &mut repeated);

        Ok(Expression {
            derived_type: None,
            location: loctoken.location,
            etype: ExpressionType::Function(Rc::new(RefCell::new(Function {
                argnames: args,
                repeated,
                body: Box::new(body),
                name,
                default_args,
                locals: None,
                alloc_before_call: None,
            }))),
        })
    }

    fn parse_lambda(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let save_loop = self.loop_count;
        self.loop_count = 0;
        let rv = self.parse_lambda_impl(lexer);
        self.loop_count = save_loop;
        rv
    }

    fn parse_lambda_impl(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let location = lexer.peek().unwrap().location.clone();
        if_expect!(lexer, TokenType::Lambda); //we may or may not have started this lambda with a signifier
        self.max_arg.push(0);
        let body = self.parse_expr(lexer)?;
        let num_args = self.max_arg.pop().unwrap();
        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::Lambda(Rc::new(RefCell::new(Lambda {
                num_args,
                body: Box::new(body),
                locals: None,
            }))),
        })
    }

    fn parse_paren_cse(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        context: &'static str,
    ) -> Result<Vec<Expression>, Error> {
        expect!(lexer, TokenType::OpenParen, context);
        if if_expect!(lexer, TokenType::CloseParen) {
            return Ok(Vec::new());
        }
        let cse = self.parse_cse(lexer, context)?;
        expect!(lexer, TokenType::CloseParen, context);
        Ok(cse)
    }

    fn parse_cse(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        context: &'static str,
    ) -> Result<Vec<Expression>, Error> {
        let mut rv = vec![self.parse_expr(lexer)?];
        while if_expect!(lexer, TokenType::Comma) {
            match lexer.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::CloseBrace) => break,
                Some(TokenType::CloseBracket) => break,
                Some(TokenType::CloseParen) => break,
                None => return Err(Error::UnexpectedEOF(context)),
                _ => (),
            }
            rv.push(self.parse_expr(lexer)?);
        }
        Ok(rv)
    }

    #[allow(dead_code)]
    fn parse_ref(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        if let Some(token) = lexer.peek().cloned() {
            let mut rv = match &token.token_type {
                TokenType::Identifier(i) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::RefExpr(RefExpr {
                        name: i.clone(),
                        is_decl: false,
                    }),
                }),
                TokenType::LambdaArg(a) if !self.max_arg.is_empty() => {
                    if *a + 1 > *self.max_arg.last().unwrap() {
                        *self.max_arg.last_mut().unwrap() = *a + 1;
                    }
                    lexer.next();
                    Ok(Expression {
                        derived_type: None,
                        location: token.location.clone(),
                        etype: ExpressionType::LambdaArg(LambdaArg { number: *a }),
                    })
                }
                TokenType::LambdaArg(_) if self.max_arg.is_empty() => self.parse_lambda(lexer),
                TokenType::Char(c) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Immediate(Immediate {
                        value: Value::Char(*c as u8),
                    }),
                }),
                TokenType::Str(s) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Str(Str { string: s.clone() }),
                }),
                TokenType::Int(i) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Immediate(Immediate {
                        value: Value::Int(*i),
                    }),
                }),
                TokenType::Float(f) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Immediate(Immediate {
                        value: Value::Float(*f),
                    }),
                }),
                TokenType::Bool(b) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Immediate(Immediate {
                        value: Value::Bool(*b),
                    }),
                }),
                TokenType::Null => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Null,
                }),
                TokenType::OpenParen => self.parse_paren(lexer),
                TokenType::OpenBrace => self.parse_block(lexer),
                TokenType::OpenBracket => self.parse_list(lexer),
                TokenType::Function => self.parse_function(lexer, false),
                TokenType::Lambda => self.parse_lambda(lexer),
                TokenType::While => self.parse_while(lexer),
                TokenType::For => self.parse_for(lexer),
                TokenType::If => self.parse_if(lexer),
                _ => Err(unexpected(token)),
            }?;
            while let Some(TokenType::Dot) = lexer.peek().map(|x| &x.token_type) {
                rv = self.parse_dotted_lookup(lexer, rv)?;
            }
            Ok(rv)
        } else {
            self.parse_paren(lexer)
        }
    }

    fn parse_dotted_lookup(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        lhs: Expression,
    ) -> Result<Expression, Error> {
        let location = expect!(lexer, TokenType::Dot, "dotted lookup").location;
        let rhs = expect_val!(lexer, TokenType::Identifier, "dotted lookup");
        Ok(match lexer.peek() {
            Some(Token {
                token_type: TokenType::OpenParen,
                location: l,
                ..
            }) => Expression {
                derived_type: None,
                location: l.clone(),
                etype: ExpressionType::PossibleMethodCall(PossibleMethodCall {
                    lhs: Box::new(lhs),
                    method_name: rhs,
                    args: self.parse_paren_cse(lexer, "function call")?,
                    alloc_before_call: None,
                    location,
                }),
            },
            _ => Expression {
                derived_type: None,
                location,
                etype: ExpressionType::DottedLookup(DottedLookup {
                    lhs: Box::new(lhs),
                    rhs,
                    index: None,
                }),
            },
        })
    }

    fn parse_list(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        let loc = expect!(lexer, TokenType::OpenBracket, "list").location;
        let exprs = if if_expect!(lexer, TokenType::CloseBracket) {
            Vec::new()
        } else {
            let r = self.parse_cse(lexer, "list")?;
            expect!(lexer, TokenType::CloseBracket, "list");
            r
        };
        Ok(Expression {
            derived_type: None,
            location: loc,
            etype: ExpressionType::List(List { exprs }),
        })
    }

    fn parse_paren(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        expect!(lexer, TokenType::OpenParen, "expression");
        let rv = self.parse_expr(lexer)?;
        expect!(lexer, TokenType::CloseParen, "expression");
        Ok(rv)
    }

    fn parse_expr(&mut self, lexer: &mut Peekable<Lexer>) -> Result<Expression, Error> {
        self.parse_eq(lexer)
    }
}

pub fn parse(lexer: Lexer) -> Result<ParseTree, Error> {
    let mut tree = ParseTree::new();
    tree.parse_statements(lexer)?;
    Ok(tree)
}
