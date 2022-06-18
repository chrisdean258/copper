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
    pub is_init: Option<usize>,
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
}

#[derive(Debug, Clone)]
pub struct Continue {
    pub location: Location,
}

fn unexpected(token: &Token) -> String {
    token
        .location
        .errfmt(format_args!("Unexpected `{}`", token.token_type))
}

macro_rules! binop {
    ( $name:ident, $next:ident, $( $token:ident ),+) => {
        fn $name<T: Iterator<Item = String>>(
            &mut self,
            lexer: &mut Peekable<Lexer<T>>,
            ) -> Result<Expression, String> {
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
    ( $lexer:ident, $token:path ) => {{
        let token = $lexer.peek().ok_or("Unexpected EOF")?;
        match &token.token_type {
            $token => $lexer.next().unwrap(),
            _ => {
                return Err(format!(
                    "{}. Expected {}",
                    unexpected(&token),
                    stringify!($token)
                ))
            }
        }
    }};
}

macro_rules! expect_val {
    ( $lexer:ident, $token:path ) => {{
        let token = $lexer.peek().ok_or("Unexpected EOF")?;
        match &token.token_type {
            $token(v) => {
                let a = v.clone();
                $lexer.next();
                a
            }
            _ => {
                return Err(format!(
                    "{}. Expected {}",
                    unexpected(&token),
                    stringify!($token)
                ))
            }
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

    pub fn parse_statements<T: Iterator<Item = String>>(
        &mut self,
        lexer: Lexer<T>,
    ) -> Result<(), String> {
        let mut peekable = lexer.peekable();
        while peekable.peek().is_some() {
            let statement = self.parse_statement(&mut peekable);
            self.statements.push(statement?);
        }
        Ok(())
    }

    fn parse_statement<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let token = lexer.peek().unwrap();
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

    fn parse_continue<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Continue).location;
        if self.loop_count == 0 {
            return Err(location.err("`continue` not allowed in the current context"));
        }
        Ok(Statement::Continue(Continue { location }))
    }

    fn parse_return<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Return).location;
        let body = if if_expect!(lexer, TokenType::Semicolon) {
            None
        } else {
            Some(Box::new(self.parse_expr(lexer)?))
        };
        Ok(Statement::Return(Return { location, body }))
    }

    fn parse_break<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Break).location;
        if self.loop_count == 0 {
            return Err(location.err("`break` not allowed in the current context"));
        }
        Ok(Statement::Break(Break { location }))
    }

    fn parse_import<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Import).location;
        let filename = expect_val!(lexer, TokenType::Identifier);
        Ok(Statement::Import(Import { location, filename }))
    }

    fn parse_from_import<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::From).location;
        let file = expect_val!(lexer, TokenType::Identifier);
        expect!(lexer, TokenType::Import);
        let what = expect_val!(lexer, TokenType::Identifier);
        Ok(Statement::FromImport(FromImport {
            location,
            file,
            what,
        }))
    }

    fn parse_class_decl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Class).location;
        let name = expect_val!(lexer, TokenType::Identifier);
        expect!(lexer, TokenType::OpenBrace);

        let mut fields = HashMap::new();
        let mut methods = HashMap::new();
        loop {
            let token = lexer.peek().ok_or("Unexpected EOF")?;
            match token.token_type {
                TokenType::CloseBrace => {
                    expect!(lexer, TokenType::CloseBrace);
                    break;
                }
                TokenType::Function => {
                    let fun = self.parse_function(lexer, true)?;
                    let fname = match &fun.etype {
                        ExpressionType::Function(f) => f.borrow().name.clone().unwrap(),
                        _ => unreachable!(),
                    };
                    if methods.insert(fname.clone(), fun.clone()).is_some() {
                        return Err(fun
                            .location
                            .errfmt(format_args!("Redefinition of method `{}`", fname)));
                    }
                }
                TokenType::Field => {
                    expect!(lexer, TokenType::Field);
                    loop {
                        let token = lexer.next().ok_or("Unexpected EOF")?;
                        let fieldname = match token.token_type {
                            TokenType::Identifier(s) => s,
                            _ => break,
                        };
                        if fields.insert(fieldname.clone(), fields.len()).is_some() {
                            return Err(token
                                .location
                                .errfmt(format_args!("Redefinition of field {}", fieldname)));
                        }
                        match lexer.peek().ok_or("Unexpected EOF")?.token_type {
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
                _ => return Err(unexpected(token)),
            }
        }

        Ok(Statement::ClassDecl(Rc::new(RefCell::new(ClassDecl {
            location,
            name,
            fields,
            methods,
        }))))
    }

    fn parse_block<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let mut rv = Vec::new();
        let location = expect!(lexer, TokenType::OpenBrace).location;

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

    fn parse_for<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let location = expect!(lexer, TokenType::For).location;
        let reference = Box::new(self.parse_ref(lexer)?);
        expect!(lexer, TokenType::In);
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

    fn parse_while<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let location = expect!(lexer, TokenType::While).location;
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

    fn parse_if<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
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

    fn parse_and<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<(If, Location), String> {
        expect!(lexer, TokenType::And);
        self.parse_if_internal(lexer)
    }

    fn parse_if_internal<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<(If, Location), String> {
        use crate::lex::TokenType::If as tokenIf;
        let location = expect!(lexer, tokenIf).location;
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

    fn parse_else<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        expect!(lexer, TokenType::Else);

        let token = lexer.peek().ok_or("Unexpected EOF")?;
        match &token.token_type {
            TokenType::If => self.parse_if(lexer),
            TokenType::OpenBrace => self.parse_block(lexer),
            _ => self.parse_expr(lexer),
        }
    }

    fn parse_eq<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
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
                    _ => return Err(unexpected(&token)),
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
                _ => return Err(unexpected(&token)),
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

    fn parse_pre_unary<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        if let Some(token) = lexer.peek() {
            let mut needs_ref = false;
            let optype = match token.token_type {
                TokenType::BoolNot => Operation::BoolNot,
                TokenType::BitNot => Operation::BitNot,
                TokenType::Minus => Operation::UnaryMinus,
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
            let token = lexer.next().unwrap();
            let rhs = self.parse_pre_unary(lexer)?;
            if needs_ref {
                match rhs.etype {
                    ExpressionType::RefExpr(_) => (),
                    _ => return Err(unexpected(&token)),
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

    fn parse_post_unary<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let mut lhs = self.parse_ref(lexer)?;
        while let Some(token) = lexer.peek() {
            let token = token.clone();
            lhs = match token.token_type {
                TokenType::Inc => Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::PostUnOp(PostUnOp {
                        lhs: if lhs.is_lval() {
                            Box::new(lhs)
                        } else {
                            return Err(unexpected(&token));
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
                            return Err(unexpected(&token));
                        },
                        op: Operation::PostDec,
                    }),
                },
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer)?;
                    Expression {
                        derived_type: None,
                        location: token.location,
                        etype: ExpressionType::CallExpr(CallExpr {
                            function: Box::new(lhs),
                            args,
                            is_init: None,
                        }),
                    }
                }
                TokenType::OpenBracket => {
                    let location = expect!(lexer, TokenType::OpenBracket).location;
                    let args = self.parse_cse(lexer)?;
                    expect!(lexer, TokenType::CloseBracket).location;
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

    fn parse_function<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
        must_be_named: bool,
    ) -> Result<Expression, String> {
        let save_loop = self.loop_count;
        self.loop_count = 0;
        let rv = self.parse_function_impl(lexer, must_be_named);
        self.loop_count = save_loop;
        rv
    }

    fn parse_function_impl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
        must_be_named: bool,
    ) -> Result<Expression, String> {
        let loctoken = expect!(lexer, TokenType::Function);
        let token = lexer.peek().ok_or("Unexpected EOF")?;
        let name = match &token.token_type {
            TokenType::Identifier(s) => {
                let rv = s.clone();
                lexer.next();
                Some(rv)
            }
            _ if must_be_named => return Err(unexpected(token)),
            _ => None,
        };
        expect!(lexer, TokenType::OpenParen);
        let mut args = Vec::new();
        let mut repeated = None;
        let mut default_args = Vec::new();
        let mut needs_default = false;
        let mut last = 0;
        loop {
            let token = lexer.next().ok_or("Unexpected EOF")?;
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
                _ => return Err(unexpected(&token)),
            }
            if needs_default {
                expect!(lexer, TokenType::Equal);
                default_args.push(self.parse_expr(lexer)?);
            }
            let token = lexer.next().ok_or("Unexpected EOF")?;
            match &token.token_type {
                TokenType::Comma => (),
                TokenType::CloseParen => break,
                TokenType::Equal if !needs_default => {
                    needs_default = true;
                    default_args.push(self.parse_expr(lexer)?);
                    let token = lexer.next().ok_or("Unexpected EOF")?;
                    match &token.token_type {
                        TokenType::Comma => (),
                        TokenType::CloseParen => break,
                        _ => return Err(unexpected(&token)),
                    }
                }
                _ => return Err(unexpected(&token)),
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
            }))),
        })
    }

    fn parse_lambda<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let save_loop = self.loop_count;
        self.loop_count = 0;
        let rv = self.parse_lambda_impl(lexer);
        self.loop_count = save_loop;
        rv
    }

    fn parse_lambda_impl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
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

    fn parse_paren_cse<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Vec<Expression>, String> {
        expect!(lexer, TokenType::OpenParen);
        if if_expect!(lexer, TokenType::CloseParen) {
            return Ok(Vec::new());
        }
        let cse = self.parse_cse(lexer)?;
        expect!(lexer, TokenType::CloseParen);
        Ok(cse)
    }

    fn parse_cse<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Vec<Expression>, String> {
        let mut rv = vec![self.parse_expr(lexer)?];
        while if_expect!(lexer, TokenType::Comma) {
            rv.push(self.parse_expr(lexer)?);
        }
        Ok(rv)
    }

    #[allow(dead_code)]
    fn parse_ref<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
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
                _ => Err(unexpected(&token)),
            }?;
            while let Some(TokenType::Dot) = lexer.peek().map(|x| &x.token_type) {
                rv = self.parse_dotted_lookup(lexer, rv)?;
            }
            Ok(rv)
        } else {
            self.parse_paren(lexer)
        }
    }

    fn parse_dotted_lookup<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
        lhs: Expression,
    ) -> Result<Expression, String> {
        let location = expect!(lexer, TokenType::Dot).location;
        let rhs = expect_val!(lexer, TokenType::Identifier);
        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::DottedLookup(DottedLookup {
                lhs: Box::new(lhs),
                rhs,
                index: None,
            }),
        })
    }

    fn parse_list<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let loc = expect!(lexer, TokenType::OpenBracket).location;
        let exprs = if if_expect!(lexer, TokenType::CloseBracket) {
            Vec::new()
        } else {
            let r = self.parse_cse(lexer)?;
            expect!(lexer, TokenType::CloseBracket);
            r
        };
        Ok(Expression {
            derived_type: None,
            location: loc,
            etype: ExpressionType::List(List { exprs }),
        })
    }

    fn parse_paren<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        expect!(lexer, TokenType::OpenParen);
        let rv = self.parse_expr(lexer)?;
        expect!(lexer, TokenType::CloseParen);
        Ok(rv)
    }

    fn parse_expr<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        self.parse_eq(lexer)
    }
}

pub fn parse<T: Iterator<Item = String>>(lexer: Lexer<T>) -> Result<ParseTree, String> {
    let mut tree = ParseTree::new();
    tree.parse_statements(lexer)?;
    Ok(tree)
}
