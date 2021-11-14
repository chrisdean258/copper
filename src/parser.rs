use crate::lex;
use crate::lex::*;
use crate::reiter::ReIter;
use crate::reiter::ReIterable;

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
pub struct While {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
    pub and_bodies: Vec<If>,
    pub else_body: Option<Box<Expression>>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
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
pub struct ParseTree {
    pub statements: Vec<Statement>,
    max_lambda: usize,
}

#[derive(Debug, Clone)]
pub struct BlockExpr {
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
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct PostUnOp {
    pub lhs: Box<Expression>,
    pub op: Token,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub argnames: Vec<String>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub max_arg: usize,
    pub body: Box<Expression>,
}

#[allow(dead_code)]
fn err_msg(token: &Token, reason: &str) -> String {
    format!("{}: {}", token.location, reason)
}

fn unexpected(token: &Token) -> String {
    panic!("{}", format!("{}: Unexpected {:?}", token.location, token))
}

macro_rules! binop {
    ( $name:ident, $next:ident, $( $token:path ),+) => {
        fn $name<T: Iterator<Item = String>>(
            &mut self,
            lexer: &mut ReIterable<Lexer<T>>,
        ) -> Result<Expression, String> {
            use crate::lex::TokenType::*;
            let lhs = self.$next(lexer)?;
            let token = match lexer.peek() {
                Some(t) => t,
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
                op: token,
                rhs: Box::new(rhs),
            }))
        }
    }
}
macro_rules! expect {
    ( $lexer:ident, $token:path ) => {
        if let Some(token) = $lexer.peek() {
            match &token.token_type {
                $token => $lexer.next(),
                _ => return Err(unexpected(&token)),
            }
        } else {
            return Err("Unexpected EOF".into());
        }
    };
}

impl ParseTree {
    pub fn new() -> ParseTree {
        ParseTree {
            statements: Vec::new(),
            max_lambda: 0,
        }
    }

    pub fn parse_statements<T: Iterator<Item = String>>(
        &mut self,
        lexer: Lexer<T>,
    ) -> Result<(), String> {
        let mut peekable = lexer.reiter();
        while peekable.peek().is_some() {
            let statement = self.parse_statement(&mut peekable);
            self.statements.push(statement?);
        }
        Ok(())
    }

    fn parse_statement<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Statement, String> {
        // use crate::lex::Keyword::*;
        use crate::lex::TokenType::*;
        use std::mem::discriminant as disc;
        let token = lexer.peek().unwrap();
        let rv = Ok(match &token.token_type {
            _ => Statement::Expr(self.parse_expr(lexer)?),
        });
        // eat all the semicolons
        while lexer
            .next_if(|t| disc(&t.token_type) == disc(&Semicolon))
            .is_some()
        {}
        rv
    }

    fn parse_block<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use std::mem::discriminant as disc;
        let mut rv = Vec::new();
        expect!(lexer, TokenType::OpenBrace);

        while lexer
            .next_if(|t| disc(&TokenType::CloseBrace) == disc(&t.token_type))
            .is_none()
        {
            let statement = self.parse_statement(lexer);
            rv.push(statement?);
        }
        Ok(Expression::BlockExpr(BlockExpr { statements: rv }))
    }

    fn parse_while<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let token = lexer.peek().unwrap();
        match &token.token_type {
            Keyword(lex::Keyword::While) => lexer.next(),
            _ => unreachable!(),
        };

        let condition = Box::new(self.parse_paren(lexer)?);
        let body = Box::new(self.parse_expr(lexer)?);

        Ok(Expression::While(While { condition, body }))
    }

    fn parse_if<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::Keyword::{And, Else};
        use crate::lex::TokenType::*;
        let mut fif = self.parse_if_internal(lexer)?;

        while let Some(token) = lexer.peek() {
            match token.token_type {
                Keyword(And) => {
                    fif.and_bodies.push(self.parse_and(lexer)?);
                }
                Keyword(Else) => {
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
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<If, String> {
        if let Some(token) = lexer.peek() {
            match &token.token_type {
                TokenType::Keyword(Keyword::And) => lexer.next(),
                _ => return Err(unexpected(&token)),
            };
            self.parse_if_internal(lexer)
        } else {
            Err("Unexpected EOF".into())
        }
    }

    fn parse_if_internal<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<If, String> {
        use crate::lex::TokenType::*;
        if let Some(token) = lexer.peek() {
            match &token.token_type {
                Keyword(lex::Keyword::If) => lexer.next(),
                _ => unreachable!(),
            };
            let condition = Box::new(self.parse_paren(lexer)?);
            let body = Box::new(self.parse_expr(lexer)?);
            Ok(If {
                condition,
                body,
                and_bodies: Vec::new(),
                else_body: None,
            })
        } else {
            Err("Unepxtected EOF. Expecting If statement".into())
        }
    }

    fn parse_else<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        if let Some(token) = lexer.peek() {
            match &token.token_type {
                TokenType::Keyword(Keyword::Else) => lexer.next(),
                _ => return Err(unexpected(&token)),
            }
        } else {
            return Err("Unexpected EOF".into());
        };

        if let Some(token) = lexer.peek() {
            match &token.token_type {
                TokenType::Keyword(Keyword::If) => self.parse_if(lexer),
                TokenType::OpenBrace => self.parse_block(lexer),
                _ => self.parse_expr(lexer),
            }
        } else {
            Err("Unexpected EOF".into())
        }
    }

    fn assemble_lambda(&mut self, expr: Expression) -> Result<Expression, String> {
        Ok(Expression::Lambda(Lambda {
            max_arg: self.max_lambda,
            body: Box::new(expr),
        }))
    }

    fn parse_eq<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let lhs = self.parse_lambda(lexer)?;

        let token = match lexer.peek() {
            Some(t) => t,
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
            op: token,
            rhs: Box::new(rhs),
            allow_decl,
        }))
    }

    fn parse_lambda<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let mut rv = self.parse_boolean_op(lexer)?;
        if self.max_lambda > 0 {
            rv = self.assemble_lambda(rv)?;
            self.max_lambda = 0;
        }
        Ok(rv)
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
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        if let Some(token) = lexer.peek() {
            match token.token_type {
                TokenType::BoolNot => (),
                TokenType::BitNot => (),
                TokenType::Minus => (),
                TokenType::Plus => (),
                _ => return self.parse_post_unary(lexer),
            }
            lexer.next();
            let rhs = self.parse_post_unary(lexer)?;
            Ok(Expression::PreUnOp(PreUnOp {
                op: lexer.next().unwrap(),
                rhs: Box::new(rhs),
            }))
        } else {
            self.parse_post_unary(lexer)
        }
    }

    fn parse_function<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let token = lexer.next().unwrap();
        match token.token_type {
            //todo fix this
            TokenType::Keyword(Keyword::Function) => (),
            _ => return Err(unexpected(&token)),
        };

        expect!(lexer, TokenType::OpenParen);
        let mut args = Vec::new();
        loop {
            if let Some(token) = lexer.next() {
                match &token.token_type {
                    TokenType::Identifier(s) => args.push(s.clone()),
                    TokenType::CloseParen => break,
                    _ => return Err(unexpected(&token)),
                }
                if let Some(token) = lexer.next() {
                    match &token.token_type {
                        TokenType::Comma => (),
                        TokenType::CloseParen => break,
                        _ => return Err(unexpected(&token)),
                    }
                } else {
                    return Err("Unexpected EOF while aprsing function".into());
                }
            } else {
                return Err("Unexpected EOF while aprsing function".into());
            }
        }

        let body = self.parse_expr(lexer)?;

        Ok(Expression::Function(Function {
            argnames: args,
            body: Box::new(body),
        }))
    }

    fn parse_post_unary<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let mut lhs = self.parse_ref(lexer)?;
        while let Some(token) = lexer.peek() {
            match token.token_type {
                TokenType::Inc => todo!(),
                TokenType::Dec => todo!(),
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer)?;
                    lhs = Expression::CallExpr(CallExpr {
                        function: Box::new(lhs),
                        args,
                    });
                }
                TokenType::OpenBracket => todo!(),
                _ => return Ok(lhs),
            }
        }
        Ok(lhs)
    }

    fn parse_paren_cse<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Vec<Expression>, String> {
        use std::mem::discriminant as disc;
        expect!(lexer, TokenType::OpenParen);
        if lexer
            .next_if(|t| disc(&t.token_type) == disc(&TokenType::CloseParen))
            .is_some()
        {
            return Ok(Vec::new());
        }
        let cse = self.parse_cse(lexer)?;
        expect!(lexer, TokenType::CloseParen);
        Ok(cse)
    }

    fn parse_cse<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Vec<Expression>, String> {
        use std::mem::discriminant as disc;
        let mut rv = Vec::new();
        rv.push(self.parse_expr(lexer)?);
        while lexer
            .next_if(|t| disc(&t.token_type) == disc(&TokenType::Comma))
            .is_some()
        {
            rv.push(self.parse_expr(lexer)?);
        }
        Ok(rv)
    }

    fn parse_ref<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        if let Some(token) = lexer.peek() {
            match &token.token_type {
                TokenType::Identifier(_) => Ok(Expression::RefExpr(RefExpr {
                    value: lexer.next().unwrap(),
                })),
                TokenType::LambdaArg(a) => {
                    self.max_lambda = if *a > self.max_lambda {
                        *a
                    } else {
                        self.max_lambda
                    };
                    Ok(Expression::RefExpr(RefExpr {
                        value: lexer.next().unwrap(),
                    }))
                }
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
                TokenType::Keyword(Keyword::Function) => self.parse_function(lexer),
                TokenType::Keyword(Keyword::While) => self.parse_while(lexer),
                TokenType::Keyword(Keyword::If) => self.parse_if(lexer),
                _ => Err(unexpected(&token)),
            }
        } else {
            self.parse_paren(lexer)
        }
    }

    fn parse_paren<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        expect!(lexer, TokenType::OpenParen);
        let rv = self.parse_expr(lexer)?;
        expect!(lexer, TokenType::CloseParen);
        Ok(rv)
    }

    fn parse_expr<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
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
