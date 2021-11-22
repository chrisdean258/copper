use crate::lex::*;
use crate::location::Location;
use std::iter::Peekable;

pub struct Parser<T: Iterator<Item = String>> {
    lexer: Lexer<T>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expression {
    While(While),
    If(If),
    CallExpr(CallExpr),
    RefExpr(RefExpr),
    Immediate(Immediate),
    BlockExpr(BlockExpr),
    BinOp(BinOp),
    PreUnOp(PreUnOp),
    PostUnOp(PostUnOp),
    AssignExpr(AssignExpr),
    Function(Function),
    Lambda(Lambda),
}

#[derive(Debug, Clone)]
pub struct ParseTree {
    pub statements: Vec<Statement>,
    max_arg: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub location: Location,
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub location: Location,
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub and_bodies: Vec<If>,
    pub else_body: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub location: Location,
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub value: Token,
}

#[derive(Debug, Clone)]
pub struct Immediate {
    pub value: Token,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub location: Location,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub lhs: Box<Expression>,
    pub op: Token,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub lhs: Box<RefExpr>,
    pub op: Token,
    pub rhs: Box<Expression>,
    pub allow_decl: bool,
}

#[derive(Debug, Clone)]
pub struct PreUnOp {
    pub op: Token,
    pub rhs: Box<RefExpr>,
}

#[derive(Debug, Clone)]
pub struct PostUnOp {
    pub lhs: Box<RefExpr>,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub location: Location,
    pub argnames: Vec<String>,
    pub body: Box<Expression>,
    pub name: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub location: Location,
    pub max_arg: usize,
    pub body: Box<Expression>,
}

#[allow(dead_code)]
fn err_msg(token: &Token, reason: &str) -> String {
    format!("{}: {}", token.location, reason)
}

fn unexpected(token: &Token) -> String {
    format!("{}: Unexpected {:?}", token.location, token.token_type)
}

macro_rules! binop {
    ( $name:ident, $next:ident, $( $token:path ),+) => {
        fn $name<T: Iterator<Item = String>>(
            &mut self,
            lexer: &mut Peekable<Lexer<T>>,
            ) -> Result<Expression, String> {
            use crate::lex::TokenType::*;
            let lhs = self.$next(lexer)?;
            let token = match lexer.peek() {
                Some(t) => t.clone(),
                None => return Ok(lhs),
            };
            match token.token_type {
                $(
                    $token => {
                        lexer.next();
                    },
                    )+
                    _ => return Ok(lhs),
            }
            let rhs = self.$name(lexer)?;
            Ok(Expression::BinOp(BinOp {
                lhs: Box::new(lhs),
                op: token.clone(),
                rhs: Box::new(rhs),
            }))
        }
    }
}
macro_rules! expect {
    ( $lexer:ident, $token:path ) => {
        if let Some(token) = $lexer.peek() {
            match &token.token_type {
                $token => $lexer.next().unwrap(),
                _ => return Err(unexpected(&token)),
            }
        } else {
            return Err("Unexpected EOF".into());
        }
    };
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
            _ => Statement::Expr(self.parse_expr(lexer)?),
        });
        // eat all the semicolons
        while if_expect!(lexer, TokenType::Semicolon) {}
        rv
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
        Ok(Expression::BlockExpr(BlockExpr {
            location,
            statements: rv,
        }))
    }

    fn parse_while<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let location = expect!(lexer, TokenType::While).location;
        let condition = Box::new(self.parse_paren(lexer)?);
        let body = Box::new(self.parse_expr(lexer)?);

        Ok(Expression::While(While {
            location,
            condition,
            body,
        }))
    }

    fn parse_if<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let mut fif = self.parse_if_internal(lexer)?;

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

        Ok(Expression::If(fif))
    }

    fn parse_and<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<If, String> {
        expect!(lexer, TokenType::And);
        self.parse_if_internal(lexer)
    }

    fn parse_if_internal<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<If, String> {
        use crate::lex::TokenType::If as tokenIf;
        let location = expect!(lexer, tokenIf).location;
        let condition = Box::new(self.parse_paren(lexer)?);
        let body = Box::new(self.parse_expr(lexer)?);
        Ok(If {
            location,
            condition,
            body,
            and_bodies: Vec::new(),
            else_body: None,
        })
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

        let token = match lexer.peek() {
            Some(t) => t.clone(),
            None => return Ok(lhs),
        };
        let mut allow_decl = false;
        let reflhs = match &token.token_type {
            Equal => {
                lexer.next();
                allow_decl = true;
                match lhs {
                    Expression::RefExpr(re) => re,
                    _ => return Err(unexpected(&token)),
                }
            }
            AndEq | XorEq | OrEq | PlusEq | MinusEq | TimesEq | DivEq | ModEq => match lhs {
                Expression::RefExpr(re) => {
                    lexer.next();
                    re
                }
                _ => return Err(unexpected(&token)),
            },
            _ => return Ok(lhs),
        };
        let rhs = self.parse_eq(lexer)?;

        Ok(Expression::AssignExpr(AssignExpr {
            lhs: Box::new(reflhs),
            op: token.clone(),
            rhs: Box::new(rhs),
            allow_decl,
        }))
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
            match token.token_type {
                TokenType::BoolNot => (),
                TokenType::BitNot => (),
                TokenType::Minus => (),
                TokenType::Plus => (),
                TokenType::Inc => (),
                TokenType::Dec => (),
                TokenType::Times => (),
                _ => return self.parse_post_unary(lexer),
            }
            let token = lexer.next().unwrap();
            let rhs = self.parse_pre_unary(lexer)?;
            let rhs = match rhs {
                Expression::RefExpr(re) => re,
                _ => return Err(unexpected(&token)),
            };
            Ok(Expression::PreUnOp(PreUnOp {
                op: token,
                rhs: Box::new(rhs),
            }))
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
                TokenType::Inc => Expression::PostUnOp(PostUnOp {
                    lhs: match lhs {
                        Expression::RefExpr(re) => Box::new(re),
                        _ => return Err(unexpected(&token)),
                    },
                    op: lexer.next().unwrap(),
                }),
                TokenType::Dec => Expression::PostUnOp(PostUnOp {
                    lhs: match lhs {
                        Expression::RefExpr(re) => Box::new(re),
                        _ => return Err(unexpected(&token)),
                    },
                    op: lexer.next().unwrap(),
                }),
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer)?;
                    Expression::CallExpr(CallExpr {
                        location: token.location,
                        function: Box::new(lhs),
                        args,
                    })
                }
                TokenType::OpenBracket => todo!(),
                _ => return Ok(lhs),
            }
        }
        Ok(lhs)
    }

    fn parse_function<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let loctoken = expect!(lexer, TokenType::Function);
        let token = lexer.peek().expect("Unexpected EOF".into());
        let name = match &token.token_type {
            TokenType::Identifier(s) => {
                let rv = s.clone();
                lexer.next();
                Some(rv)
            }
            _ => None,
        };
        expect!(lexer, TokenType::OpenParen);
        let mut args = Vec::new();
        loop {
            let token = lexer.next().expect("Unexpected EOF".into());
            match &token.token_type {
                TokenType::Identifier(s) => args.push(s.clone()),
                TokenType::CloseParen => break,
                _ => return Err(unexpected(&token)),
            }
            let token = lexer.next().expect("Unexpected EOF".into());
            match &token.token_type {
                TokenType::Comma => (),
                TokenType::CloseParen => break,
                _ => return Err(unexpected(&token)),
            }
        }
        let body = self.parse_expr(lexer)?;

        Ok(Expression::Function(Function {
            location: loctoken.location,
            argnames: args,
            body: Box::new(body),
            name: name,
        }))
    }

    fn parse_lambda<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let location = lexer.peek().unwrap().location.clone();
        if_expect!(lexer, TokenType::Lambda); //we may or may not have started this lambda with a signifier
        self.max_arg.push(0);
        let body = self.parse_expr(lexer)?;
        let max_arg = self.max_arg.pop().unwrap();
        Ok(Expression::Lambda(Lambda {
            location,
            max_arg,
            body: Box::new(body),
        }))
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

    fn parse_ref<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        if let Some(token) = lexer.peek() {
            match &token.token_type {
                TokenType::Identifier(_) => Ok(Expression::RefExpr(RefExpr {
                    value: lexer.next().unwrap(),
                })),
                TokenType::LambdaArg(a) if self.max_arg.len() > 0 => {
                    if *a > *self.max_arg.last().unwrap() {
                        *self.max_arg.last_mut().unwrap() = *a;
                    }
                    Ok(Expression::RefExpr(RefExpr {
                        value: lexer.next().unwrap(),
                    }))
                }
                TokenType::LambdaArg(_) if self.max_arg.len() == 0 => self.parse_lambda(lexer),
                TokenType::Char(_) => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::Str(_) => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::Int(_) => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::Float(_) => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::Bool(_) => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::Null => Ok(Expression::Immediate(Immediate {
                    value: lexer.next().unwrap(),
                })),
                TokenType::OpenParen => self.parse_paren(lexer),
                TokenType::OpenBrace => self.parse_block(lexer),
                TokenType::Function => self.parse_function(lexer),
                TokenType::Lambda => self.parse_lambda(lexer),
                TokenType::While => self.parse_while(lexer),
                TokenType::If => self.parse_if(lexer),
                _ => Err(unexpected(&token)),
            }
        } else {
            self.parse_paren(lexer)
        }
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

impl<T: Iterator<Item = String>> Parser<T> {
    pub fn new(lexer: Lexer<T>) -> Parser<T> {
        Parser { lexer }
    }

    pub fn parse(self) -> Result<ParseTree, String> {
        let mut tree = ParseTree::new();
        tree.parse_statements(self.lexer)?;
        Ok(tree)
    }
}
