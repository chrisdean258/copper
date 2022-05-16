use crate::lex::*;
use crate::location::Location;
use crate::operation::Operation;
use crate::typesystem::Type;
use crate::value::Value;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::iter::Peekable;
use std::rc::Rc;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Statement {
    Expr(Expression),
    ClassDecl(ClassDecl),
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
}

impl Expression {
    pub fn is_lval(&self) -> bool {
        use ExpressionType::*;
        match self.etype {
            RefExpr(_) | IndexExpr(_) | DottedLookup(_) => true,
            _ => false,
        }
    }

    pub fn typ(&self) -> Type {
        assert!(self.derived_type.is_some());
        self.derived_type.unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct ParseTree {
    pub statements: Vec<Statement>,
    max_arg: Vec<usize>,
    loop_count: usize,
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
    pub fields: HashSet<String>,
    pub methods: HashMap<String, Function>,
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
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
    pub sig_handle: Option<usize>,
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

#[allow(dead_code)]
fn err_msg(token: &Token, reason: &str) -> String {
    format!("{}: {}", token.location, reason)
}

fn unexpected(token: &Token) -> String {
    format!("{}: Unexpected `{}`", token.location, token.token_type)
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
            _ => return Err(format!("{}. Expected {}", unexpected(&token), $token)),
        }
    }};
}

macro_rules! if_expect {
    ( $lexer:ident, $token:path ) => {
        if let Some(token) = $lexer.peek() {
            match &token.token_type {
                $token => {
                    $lexer.next();
                    true
                }
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
            return Err(format!(
                "{}: `continue` not allowed in the current context",
                location
            ));
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
            return Err(format!(
                "{}: `break` not allowed in the current context",
                location
            ));
        }
        Ok(Statement::Break(Break { location }))
    }

    fn parse_import<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Import).location;
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let filename = match token.token_type {
            TokenType::Identifier(s) => s,
            _ => return Err(unexpected(&token)),
        };
        Ok(Statement::Import(Import { location, filename }))
    }

    fn parse_from_import<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::From).location;
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let file = match token.token_type {
            TokenType::Identifier(s) => s,
            _ => return Err(unexpected(&token)),
        };
        expect!(lexer, TokenType::Import);
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let what = match token.token_type {
            TokenType::Identifier(s) => s,
            _ => return Err(unexpected(&token)),
        };
        Ok(Statement::FromImport(FromImport {
            location,
            file,
            what,
        }))
    }

    #[allow(dead_code)]
    fn parse_class_decl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let _location = expect!(lexer, TokenType::Class).location;
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let _name = match &token.token_type {
            TokenType::Identifier(s) => s.clone(),
            _ => return Err(unexpected(&token)),
        };

        expect!(lexer, TokenType::OpenBrace);

        let mut fields = HashSet::new();
        let mut methods = HashMap::new();
        loop {
            let token = lexer.peek().ok_or("Unexpected EOF")?;
            match token.token_type {
                TokenType::CloseBrace => {
                    lexer.next();
                    break;
                }
                TokenType::Function => {
                    let fun = self.parse_function(lexer, true)?;
                    let ffun = match &fun.etype {
                        ExpressionType::Function(f) => f,
                        _ => unreachable!(),
                    };
                    if methods
                        .insert(ffun.borrow().name.as_ref().unwrap().clone(), fun.clone())
                        .is_some()
                    {
                        return Err(format!(
                            "{}: redefinition of method `{}`",
                            fun.location,
                            ffun.borrow().name.clone().unwrap()
                        ));
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
                        if !fields.insert(fieldname.clone()) {
                            return Err(format!(
                                "{}: redefinition of field {}",
                                token.location, fieldname
                            ));
                        }
                        if if_expect!(lexer, TokenType::Semicolon) {
                            break;
                        }
                        if !if_expect!(lexer, TokenType::Comma) {
                            break;
                        }
                    }
                }
                _ => return Err(unexpected(&token)),
            }
        }

        todo!();
        /*Ok(Statement::ClassDecl(ClassDecl {
            location,
            name,
            fields,
            methods,
        }))*/
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
            },
            location,
        ))
    }

    fn parse_else<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        expect!(lexer, TokenType::Else);

        let token = lexer.peek().expect("Unexpected EOF".into());
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
                TokenType::Times => Operation::Times,
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
                            sig_handle: None,
                        }),
                    }
                }
                TokenType::OpenBracket => {
                    let location = expect!(lexer, TokenType::OpenBracket).location;
                    let args = self.parse_cse(lexer)?;
                    expect!(lexer, TokenType::CloseBracket).location;
                    Expression {
                        derived_type: None,
                        location: location,
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
            _ if must_be_named => return Err(unexpected(&token)),
            _ => None,
        };
        expect!(lexer, TokenType::OpenParen);
        let mut args = Vec::new();
        let mut default_args = Vec::new();
        let mut needs_default = false;
        loop {
            let token = lexer.next().ok_or("Unexpected EOF")?;
            match &token.token_type {
                TokenType::Identifier(s) => args.push(s.clone()),
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
        let body = self.parse_expr(lexer)?;

        Ok(Expression {
            derived_type: None,
            location: loctoken.location.clone(),
            etype: ExpressionType::Function(Rc::new(RefCell::new(Function {
                argnames: args,
                body: Box::new(body),
                name: name.clone(),
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
        let mut rv = Vec::new();
        rv.push(self.parse_expr(lexer)?);
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
        if let Some(token) = lexer.peek().map(|t| t.clone()) {
            let mut rv = match &token.token_type {
                TokenType::Identifier(i) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location.clone(),
                    etype: ExpressionType::RefExpr(RefExpr {
                        name: i.clone(),
                        is_decl: false,
                    }),
                }),
                TokenType::LambdaArg(a) if self.max_arg.len() > 0 => {
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
                TokenType::LambdaArg(_) if self.max_arg.len() == 0 => self.parse_lambda(lexer),
                TokenType::Char(c) => Ok(Expression {
                    derived_type: None,
                    location: lexer.next().unwrap().location,
                    etype: ExpressionType::Immediate(Immediate {
                        value: Value::Char(c.clone() as u8),
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
                    etype: ExpressionType::Immediate(Immediate { value: Value::Null }),
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
            loop {
                match lexer.peek().map(|x| &x.token_type) {
                    Some(TokenType::Dot) => {
                        rv = self.parse_dotted_lookup(lexer, rv)?;
                    }
                    _ => break,
                }
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
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let rhs = match token.token_type {
            TokenType::Identifier(s) => s,
            _ => return Err(unexpected(&token)),
        };
        Ok(Expression {
            derived_type: None,
            location,
            etype: ExpressionType::DottedLookup(DottedLookup {
                lhs: Box::new(lhs),
                rhs,
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
            etype: ExpressionType::List(List { exprs: exprs }),
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
