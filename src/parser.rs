use crate::{
    lex::*, location::Location, operation::Operation, typecheck::TypedExpression,
    typesystem::Signature, typesystem::Type, value::Value,
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
    ExpressionError(Expression),
    ParseError(Error),
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub etype: ExpressionType,
    pub location: Location,
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
    RepeatedArg,
    Null,
    ParseError(Error),
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
    repeated_arg: Option<String>,
    pub globals: Option<usize>,
    has_error: bool,
    eof: Location,
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

    pub signatures: Vec<Signature>,
    pub typed_bodies: Vec<TypedExpression>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub num_args: usize,
    pub body: Box<Expression>,
    pub locals: Option<usize>,

    pub signatures: Vec<Signature>,
    pub typed_bodies: Vec<TypedExpression>,
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
    Lexing(Location, char, &'static str, Option<&'static str>),
    FieldRedefinition(Location, String),
    MethodRedefinition(Location, String),
    UnexpectedEOF(&'static str),
    UnexpectedToken(Token, Option<&'static str>),
}

impl Error {
    pub fn eof(location: Location, context: &'static str) -> Expression {
        Expression {
            location,
            etype: ExpressionType::ParseError(Self::UnexpectedEOF(context)),
        }
    }
}

impl std::error::Error for Error {}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Lexing(l, c, con, expt) => {
                write!(f, "{l}: Unable to lex `{c}` while trying to lex {con}.")?;
                if let Some(expected) = expt {
                    write!(f, " Was expecting `{}`.", expected)?;
                }
                Ok(())
            }
            Self::UnexpectedEOF(c) => write!(f, "Unexpected EOF while parsing {c}"),
            Self::UnexpectedToken(t, expt) => {
                write!(f, "{}: Unexpected `{}`.", t.location, t.token_type)?;
                if let Some(expected) = expt {
                    write!(f, " Expected `{}`.", expected)?;
                }
                Ok(())
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

fn unexpected_int(token: Token, expt: Option<&'static str>) -> Expression {
    Expression {
        location: token.location.clone(),
        etype: ExpressionType::ParseError(if let TokenType::ErrChar(e, c) = token.token_type {
            Error::Lexing(token.location, e, c, expt)
        } else {
            Error::UnexpectedToken(token, expt)
        }),
    }
}

fn unexpected(token: Token) -> Expression {
    unexpected_int(token, None)
}

fn unexpect_known(token: Token, expt: &'static str) -> Expression {
    unexpected_int(token, Some(expt))
}

macro_rules! binop {
    ( $name:ident, $next:ident, $( $token:ident ),+) => {
        fn $name(
            &mut self,
            lexer: &mut Peekable<Lexer>,
            ) -> Expression {
            let mut lhs = self.$next(lexer);
            loop {
                let token = match lexer.peek() {
                    Some(t) => t.clone(),
                    None => return lhs,
                };
                let optype = match token.token_type {
                    $( TokenType::$token => { lexer.next(); Operation::$token}, )+
                        _ => return lhs,
                };
                let rhs = self.$next(lexer);
                lhs = Expression {
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

macro_rules! peek_handle_eof {
    ( $self:expr, $lexer:ident, $context:expr) => {
        peek_handle_eof!($self, $lexer, $context, |i| i)
    };
    ( $self:expr, $lexer:ident, $context:expr, $conv:expr) => {
        if let Some(t) = $lexer.peek() {
            t
        } else {
            return $conv(Error::eof($self.eof.clone(), $context));
        }
    };
}

macro_rules! next_handle_eof {
    ( $self:expr, $lexer:ident, $context:expr) => {{
        next_handle_eof!($self, $lexer, $context, |i| i)
    }};
    ( $self:expr, $lexer:ident, $context:expr, $conv:expr) => {{
        peek_handle_eof!($self, $lexer, $context, $conv);
        $lexer.next().unwrap()
    }};
}

macro_rules! expect {
    ( $self:expr, $lexer:ident, $token:path, $context:expr ) => {{
        expect!($self, $lexer, $token, $context, |i| i)
    }};
    ( $self:expr, $lexer:ident, $token:path, $context:expr, $conv:expr ) => {{
        let token = next_handle_eof!($self, $lexer, $context, $conv);
        match &token.token_type {
            $token => token,
            _ => return $conv(unexpect_known(token, stringify!($token))),
        }
    }};
}

macro_rules! expect_val {
    ( $self:expr, $lexer:ident, $token:path, $context:expr ) => {{
        expect_val!($self, $lexer, $token, $context, |i| i)
    }};
    ( $self:expr, $lexer:ident, $token:path, $context:expr, $conv:expr ) => {{
        let token = next_handle_eof!($self, $lexer, $context, $conv);
        match token.token_type {
            $token(v) => v,
            _ => return $conv(unexpect_known(token, stringify!($token))),
        }
    }};
}

macro_rules! if_expect {
    ( $self:expr, $lexer:ident, $($token:path),* $(,)? ) => {
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

macro_rules! recover {
    ( $lexer:ident, $($token:path),* $(,)? ) => {
        eat_until!($lexer, $($token),*);
        if_expect!($lexer, $($token),*)
    };
}

macro_rules! eat_until {
    ( $lexer:ident, $($token:path),* $(,)? ) => {
            while let Some(t) = $lexer.peek() {
                match &t.token_type {
                    $($token => {
                        break;
                    })*
                    _ => {
                        $lexer.next();
                    }
                }
            }
    };
}

impl ParseTree {
    pub fn new(eof: Location) -> ParseTree {
        ParseTree {
            statements: Vec::new(),
            max_arg: Vec::new(),
            repeated_arg: None,
            globals: None,
            has_error: false,
            eof,
        }
    }

    pub fn parse_statements(&mut self, lexer: Lexer) {
        let mut peekable = lexer.peekable();
        while peekable.peek().is_some() {
            let s = self.parse_statement(&mut peekable);
            self.statements.push(s);
        }
    }

    fn parse_statement(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let token = peek_handle_eof!(self, lexer, "statement", Statement::ExpressionError);
        let rv = match &token.token_type {
            TokenType::Class => self.parse_class_decl(lexer),
            TokenType::Import => self.parse_import(lexer),
            TokenType::From => self.parse_from_import(lexer),
            TokenType::Break => self.parse_break(lexer),
            TokenType::Continue => self.parse_continue(lexer),
            TokenType::Return => self.parse_return(lexer),
            _ => Statement::Expr(self.parse_expr(lexer)),
        };
        // eat all the semicolons
        while if_expect!(self, lexer, TokenType::Semicolon) {}
        rv
    }

    fn parse_continue(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::Continue,
            "continue",
            Statement::ExpressionError
        )
        .location;
        Statement::Continue(Continue { location })
    }

    fn parse_return(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::Return,
            "return",
            Statement::ExpressionError
        )
        .location;
        let body = if if_expect!(self, lexer, TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expr(lexer)))
        };
        Statement::Return(Return {
            location,
            body,
            from_function: true,
        })
    }

    fn parse_break(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::Break,
            "break",
            Statement::ExpressionError
        )
        .location;
        Statement::Break(Break { location })
    }

    fn parse_import(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::Import,
            "import",
            Statement::ExpressionError
        )
        .location;
        let filename = expect_val!(
            self,
            lexer,
            TokenType::Identifier,
            "import",
            Statement::ExpressionError
        );
        Statement::Import(Import { location, filename })
    }

    fn parse_from_import(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::From,
            "from import",
            Statement::ExpressionError
        )
        .location;
        let file = expect_val!(
            self,
            lexer,
            TokenType::Identifier,
            "from import",
            Statement::ExpressionError
        );
        expect!(
            self,
            lexer,
            TokenType::Import,
            "from import",
            Statement::ExpressionError
        );
        let what = expect_val!(
            self,
            lexer,
            TokenType::Identifier,
            "from import",
            Statement::ExpressionError
        );
        Statement::FromImport(FromImport {
            location,
            file,
            what,
        })
    }

    fn parse_class_decl(&mut self, lexer: &mut Peekable<Lexer>) -> Statement {
        let location = expect!(
            self,
            lexer,
            TokenType::Class,
            "class declaraction",
            Statement::ExpressionError
        )
        .location;
        let name = expect_val!(
            self,
            lexer,
            TokenType::Identifier,
            "class declaration",
            Statement::ExpressionError
        );
        expect!(
            self,
            lexer,
            TokenType::OpenBrace,
            "class declaration",
            Statement::ExpressionError
        );

        let mut fields = HashMap::new();
        let mut methods = HashMap::new();
        loop {
            let token =
                peek_handle_eof!(self, lexer, "class declaration", Statement::ExpressionError);
            match token.token_type {
                TokenType::CloseBrace => {
                    expect!(
                        self,
                        lexer,
                        TokenType::CloseBrace,
                        "class declaration",
                        Statement::ExpressionError
                    );
                    break;
                }
                TokenType::Function => {
                    let fun = self.parse_function(lexer, true);
                    let fname = match &fun.etype {
                        ExpressionType::Function(f) => f.borrow().name.clone().unwrap(),
                        _ => unreachable!(),
                    };
                    if methods.insert(fname.clone(), fun.clone()).is_some() {
                        return Statement::ParseError(Error::MethodRedefinition(location, fname));
                    }
                }
                TokenType::Field => {
                    expect!(
                        self,
                        lexer,
                        TokenType::Field,
                        "fields",
                        Statement::ExpressionError
                    );
                    loop {
                        let token =
                            next_handle_eof!(self, lexer, "fields", Statement::ExpressionError);
                        let fieldname = match token.token_type {
                            TokenType::Identifier(s) => s,
                            _ => break,
                        };
                        if fields.insert(fieldname.clone(), fields.len()).is_some() {
                            return Statement::ParseError(Error::FieldRedefinition(
                                location, fieldname,
                            ));
                        }
                        match peek_handle_eof!(self, lexer, "fields", Statement::ExpressionError)
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
                _ => return Statement::ExpressionError(unexpected(token.clone())),
            }
        }

        Statement::ClassDecl(Rc::new(RefCell::new(ClassDecl {
            location,
            name,
            fields,
            methods,
        })))
    }

    fn parse_block(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let mut rv = Vec::new();
        let location = expect!(self, lexer, TokenType::OpenBrace, "block").location;

        while !if_expect!(self, lexer, TokenType::CloseBrace) {
            rv.push(self.parse_statement(lexer));
        }
        Expression {
            location,
            etype: ExpressionType::BlockExpr(BlockExpr { statements: rv }),
        }
    }

    fn parse_for(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let location = expect!(self, lexer, TokenType::For, "for loop").location;
        let reference = Box::new(self.parse_ref(lexer));
        expect!(self, lexer, TokenType::In, "for loop");
        let items = Box::new(self.parse_expr(lexer));
        let body = Box::new(self.parse_expr(lexer));

        Expression {
            location,
            etype: ExpressionType::For(For {
                reference,
                items,
                body,
            }),
        }
    }

    fn parse_while(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let location = expect!(self, lexer, TokenType::While, "while loop").location;
        let condition = Box::new(self.parse_expr(lexer));
        let body = Box::new(self.parse_expr(lexer));

        Expression {
            location,
            etype: ExpressionType::While(While { condition, body }),
        }
    }

    fn parse_if(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        use crate::lex::TokenType::*;
        let (mut fif, location) = self.parse_if_internal(lexer).unwrap();

        while let Some(token) = lexer.peek() {
            match token.token_type {
                And => {
                    fif.and_bodies.push(self.parse_and(lexer).unwrap());
                }
                Else => {
                    fif.else_body = Some(Box::new(self.parse_else(lexer)));
                    break;
                }
                _ => break,
            }
        }

        Expression {
            location,
            etype: ExpressionType::If(fif),
        }
    }

    fn parse_and(&mut self, lexer: &mut Peekable<Lexer>) -> Result<(If, Location), Expression> {
        expect!(self, lexer, TokenType::And, "and if statement", |i| Err(i));
        self.parse_if_internal(lexer)
    }

    fn parse_if_internal(
        &mut self,
        lexer: &mut Peekable<Lexer>,
    ) -> Result<(If, Location), Expression> {
        use crate::lex::TokenType::If as tokenIf;
        let location = expect!(self, lexer, tokenIf, "if statement", |i| Err(i)).location;
        let condition = Box::new(self.parse_expr(lexer));
        let body = Box::new(self.parse_expr(lexer));
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

    fn parse_else(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        expect!(self, lexer, TokenType::Else, "else statement");

        let token = peek_handle_eof!(self, lexer, "else statement");
        match &token.token_type {
            TokenType::If => self.parse_if(lexer),
            TokenType::OpenBrace => self.parse_block(lexer),
            _ => self.parse_expr(lexer),
        }
    }

    fn parse_eq(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        use crate::lex::TokenType::*;
        let lhs = self.parse_boolean_op(lexer);

        let mut token = match lexer.peek() {
            Some(t) => t.clone(),
            None => return lhs,
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
                    _ => return unexpected(token),
                }
            }
            AndEq | XorEq | OrEq | PlusEq | MinusEq | TimesEq | DivEq | ModEq | BitShiftLeftEq
            | BitShiftRightEq => match lhs.etype {
                ExpressionType::RefExpr(_) => {
                    lexer.next();
                    lhs
                }
                ExpressionType::IndexExpr(_) => {
                    lexer.next();
                    lhs
                }
                _ => return unexpected(token),
            },
            _ => return lhs,
        };
        let rhs = self.parse_eq(lexer);
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
        Expression {
            location: token.location,
            etype: ExpressionType::AssignExpr(AssignExpr {
                lhs: Box::new(reflhs),
                op,
                rhs: Box::new(rhs),
                allow_decl,
            }),
        }
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

    fn parse_pre_unary(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let token = peek_handle_eof!(self, lexer, "expression");
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
        let token = next_handle_eof!(self, lexer, "pre-unary expression");
        let rhs = self.parse_pre_unary(lexer);
        if needs_ref {
            match rhs.etype {
                ExpressionType::RefExpr(_) => (),
                _ => return unexpected(token),
            };
        }
        if let ExpressionType::RefExpr(r) = &rhs.etype {
            if Some(&r.name) == self.repeated_arg.as_ref() && optype == Operation::Deref {
                return Expression {
                    location: token.location,
                    etype: ExpressionType::RepeatedArg,
                };
            }
        }
        Expression {
            location: token.location,
            etype: ExpressionType::PreUnOp(PreUnOp {
                op: optype,
                rhs: Box::new(rhs),
            }),
        }
    }

    fn parse_post_unary(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let mut lhs = self.parse_ref(lexer);
        while let Some(token) = lexer.peek().cloned() {
            lhs = match token.token_type {
                TokenType::Inc => Expression {
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::PostUnOp(PostUnOp {
                        lhs: if lhs.is_lval() {
                            Box::new(lhs)
                        } else {
                            Box::new(unexpected(token))
                        },
                        op: Operation::PostInc,
                    }),
                },
                TokenType::Dec => Expression {
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::PostUnOp(PostUnOp {
                        lhs: if lhs.is_lval() {
                            Box::new(lhs)
                        } else {
                            Box::new(unexpected(token))
                        },
                        op: Operation::PostDec,
                    }),
                },
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer, "function call");
                    Expression {
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
                        expect!(self, lexer, TokenType::OpenBracket, "index expression").location;
                    let args = self.parse_cse(lexer, "index expression");
                    expect!(self, lexer, TokenType::CloseBracket, "index expression").location;
                    Expression {
                        location,
                        etype: ExpressionType::IndexExpr(IndexExpr {
                            obj: Box::new(lhs),
                            args,
                        }),
                    }
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_function(&mut self, lexer: &mut Peekable<Lexer>, must_be_named: bool) -> Expression {
        let loctoken = expect!(self, lexer, TokenType::Function, "function");
        let token = peek_handle_eof!(self, lexer, "function");
        let name = match &token.token_type {
            TokenType::Identifier(s) => {
                let rv = s.clone();
                lexer.next();
                Some(rv)
            }
            _ if must_be_named => return unexpected(token.clone()),
            _ => None,
        };
        expect!(self, lexer, TokenType::OpenParen, "function");
        let mut args = Vec::new();
        let mut repeated = None;
        let mut default_args = Vec::new();
        let mut needs_default = false;
        let mut last = 0;
        loop {
            let token = next_handle_eof!(self, lexer, "function");
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
                _ => return unexpected(token),
            }
            if needs_default {
                expect!(self, lexer, TokenType::Equal, "default argument");
                default_args.push(self.parse_expr(lexer));
            }
            let token = next_handle_eof!(self, lexer, "argument");
            match &token.token_type {
                TokenType::Comma => (),
                TokenType::CloseParen => break,
                TokenType::Equal if !needs_default => {
                    needs_default = true;
                    default_args.push(self.parse_expr(lexer));
                    let token = next_handle_eof!(self, lexer, "arguments");
                    match &token.token_type {
                        TokenType::Comma => (),
                        TokenType::CloseParen => break,
                        _ => return unexpected(token),
                    }
                }
                _ => return unexpected(token),
            }
        }
        swap(&mut self.repeated_arg, &mut repeated);
        let body = self.parse_expr(lexer);
        swap(&mut self.repeated_arg, &mut repeated);

        Expression {
            location: loctoken.location,
            etype: ExpressionType::Function(Rc::new(RefCell::new(Function {
                argnames: args,
                repeated,
                body: Box::new(body),
                name,
                default_args,
                locals: None,
                alloc_before_call: None,
                signatures: Vec::new(),
                typed_bodies: Vec::new(),
            }))),
        }
    }

    fn parse_lambda(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let location = lexer.peek().unwrap().location.clone();
        if_expect!(self, lexer, TokenType::Lambda); //we may or may not have started this lambda with a signifier
        self.max_arg.push(0);
        let body = self.parse_expr(lexer);
        let num_args = self.max_arg.pop().unwrap();
        Expression {
            location,
            etype: ExpressionType::Lambda(Rc::new(RefCell::new(Lambda {
                num_args,
                body: Box::new(body),
                locals: None,
                signatures: Vec::new(),
                typed_bodies: Vec::new(),
            }))),
        }
    }

    fn parse_paren_cse(
        &mut self,
        lexer: &mut Peekable<Lexer>,
        context: &'static str,
    ) -> Vec<Expression> {
        expect!(self, lexer, TokenType::OpenParen, context, |i| vec![i]);
        if if_expect!(self, lexer, TokenType::CloseParen) {
            return Vec::new();
        }
        let cse = self.parse_cse(lexer, context);
        expect!(self, lexer, TokenType::CloseParen, context, |i| vec![i]);
        cse
    }

    fn parse_cse(&mut self, lexer: &mut Peekable<Lexer>, context: &'static str) -> Vec<Expression> {
        let mut rv = vec![self.parse_expr(lexer)];
        while if_expect!(self, lexer, TokenType::Comma) {
            match lexer.peek().map(|t| t.token_type.clone()) {
                Some(TokenType::CloseBrace) => break,
                Some(TokenType::CloseBracket) => break,
                Some(TokenType::CloseParen) => break,
                None => {
                    rv.push(Error::eof(self.eof.clone(), context));
                    break;
                }
                _ => (),
            }
            rv.push(self.parse_expr(lexer));
        }
        rv
    }

    fn parse_ref(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let token = peek_handle_eof!(self, lexer, "reference");
        let mut rv = match &token.token_type.clone() {
            TokenType::Identifier(i) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::RefExpr(RefExpr {
                    name: i.clone(),
                    is_decl: false,
                }),
            },
            TokenType::LambdaArg(a) if !self.max_arg.is_empty() => {
                if *a + 1 > *self.max_arg.last().unwrap() {
                    *self.max_arg.last_mut().unwrap() = *a + 1;
                }
                Expression {
                    location: lexer.next().unwrap().location.clone(),
                    etype: ExpressionType::LambdaArg(LambdaArg { number: *a }),
                }
            }
            TokenType::LambdaArg(_) if self.max_arg.is_empty() => self.parse_lambda(lexer),
            TokenType::Char(c) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Immediate(Immediate {
                    value: Value::Char(*c as u8),
                }),
            },
            TokenType::Str(s) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Str(Str { string: s.clone() }),
            },
            TokenType::Int(i) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Immediate(Immediate {
                    value: Value::Int(*i),
                }),
            },
            TokenType::Float(f) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Immediate(Immediate {
                    value: Value::Float(*f),
                }),
            },
            TokenType::Bool(b) => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Immediate(Immediate {
                    value: Value::Bool(*b),
                }),
            },
            TokenType::Null => Expression {
                location: lexer.next().unwrap().location,
                etype: ExpressionType::Null,
            },
            TokenType::OpenParen => self.parse_paren(lexer),
            TokenType::OpenBrace => self.parse_block(lexer),
            TokenType::OpenBracket => self.parse_list(lexer),
            TokenType::Function => self.parse_function(lexer, false),
            TokenType::Lambda => self.parse_lambda(lexer),
            TokenType::While => self.parse_while(lexer),
            TokenType::For => self.parse_for(lexer),
            TokenType::If => self.parse_if(lexer),
            _ => unexpected(token.clone()),
        };
        while let Some(TokenType::Dot) = lexer.peek().map(|x| &x.token_type) {
            rv = self.parse_dotted_lookup(lexer, rv);
        }
        rv
    }

    fn parse_dotted_lookup(&mut self, lexer: &mut Peekable<Lexer>, lhs: Expression) -> Expression {
        let location = expect!(self, lexer, TokenType::Dot, "dotted lookup").location;
        let rhs = expect_val!(self, lexer, TokenType::Identifier, "dotted lookup");
        match lexer.peek() {
            Some(Token {
                token_type: TokenType::OpenParen,
                location: l,
                ..
            }) => Expression {
                location: l.clone(),
                etype: ExpressionType::PossibleMethodCall(PossibleMethodCall {
                    lhs: Box::new(lhs),
                    method_name: rhs,
                    args: self.parse_paren_cse(lexer, "function call"),
                    alloc_before_call: None,
                    location,
                }),
            },
            _ => Expression {
                location,
                etype: ExpressionType::DottedLookup(DottedLookup {
                    lhs: Box::new(lhs),
                    rhs,
                    index: None,
                }),
            },
        }
    }

    fn parse_list(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        let loc = expect!(self, lexer, TokenType::OpenBracket, "list").location;
        let exprs = if if_expect!(self, lexer, TokenType::CloseBracket) {
            Vec::new()
        } else {
            let r = self.parse_cse(lexer, "list");
            expect!(self, lexer, TokenType::CloseBracket, "list");
            r
        };
        Expression {
            location: loc,
            etype: ExpressionType::List(List { exprs }),
        }
    }

    fn parse_paren(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        expect!(self, lexer, TokenType::OpenParen, "expression").location;
        let rv = self.parse_expr(lexer);
        expect!(self, lexer, TokenType::CloseParen, "expression");
        rv
    }

    fn parse_expr(&mut self, lexer: &mut Peekable<Lexer>) -> Expression {
        self.parse_eq(lexer)
    }
}

pub fn parse(lexer: Lexer) -> ParseTree {
    let mut tree = ParseTree::new(lexer.eof());
    tree.parse_statements(lexer);
    tree
}
