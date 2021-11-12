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
        use crate::lex::Keyword::*;
        use crate::lex::TokenType::*;
        use std::mem::discriminant as disc;
        let token = lexer.peek().unwrap();
        let rv = Ok(match &token.token_type {
            CloseParen => return Err(err_msg(&token, "Unexpected closing paren")),
            Comma => return Err(err_msg(&token, "Unexpected comma")),
            Semicolon => return Err(err_msg(&token, "Unexpected semicolon")),
            Dot => return Err(err_msg(&token, "Unexpected dot")),
            Equal => return Err(err_msg(&token, "Unexpected equal")),
            OpenParen => Statement::Expr(self.parse_expr(lexer)?),
            OpenBrace => Statement::Expr(self.parse_block(lexer)?),
            Identifier(_) => Statement::Expr(self.parse_expr(lexer)?),
            Str(_) => Statement::Expr(self.parse_expr(lexer)?),
            Int(_) => Statement::Expr(self.parse_expr(lexer)?),
            Float(_) => Statement::Expr(self.parse_expr(lexer)?),
            Char(_) => Statement::Expr(self.parse_expr(lexer)?),
            ErrChar(c) => return Err(err_msg(&token, &format!("{:?}", c))),
            Keyword(While) => Statement::Expr(self.parse_while(lexer)?),
            Keyword(If) => Statement::Expr(self.parse_if(lexer)?),
            Keyword(_) => todo!(),
            CloseBrace => todo!(),
            _ => todo!(),
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

    fn parse_eq<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let lhs = self.parse_boolean_op(lexer)?;
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

    fn parse_post_unary<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let lhs = self.parse_ref(lexer)?;
        if let Some(token) = lexer.peek() {
            match token.token_type {
                TokenType::Inc => (),
                TokenType::Dec => (),
                TokenType::OpenParen => {
                    let args = self.parse_paren_cse(lexer)?;
                    return Ok(Expression::CallExpr(CallExpr {
                        function: Box::new(lhs),
                        args,
                    }));
                }
                TokenType::OpenBracket => todo!(),
                _ => return Ok(lhs),
            }
            lexer.next();
            Ok(Expression::PostUnOp(PostUnOp {
                lhs: Box::new(lhs),
                op: lexer.next().unwrap(),
            }))
        } else {
            Ok(lhs)
        }
    }

    fn parse_paren_cse<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Vec<Expression>, String> {
        expect!(lexer, TokenType::OpenParen);
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
                TokenType::OpenParen => self.parse_paren(lexer),
                TokenType::OpenBrace => self.parse_block(lexer),
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
        use crate::lex::TokenType::*;
        let token = lexer.peek().unwrap();
        Ok(match token.token_type {
            Identifier(_) => self.parse_eq(lexer)?,
            Str(_) => self.parse_eq(lexer)?,
            Int(_) => self.parse_eq(lexer)?,
            Float(_) => self.parse_eq(lexer)?,
            Char(_) => self.parse_eq(lexer)?,
            Bool(_) => self.parse_eq(lexer)?,
            OpenBrace => self.parse_block(lexer)?,
            OpenParen => self.parse_paren(lexer)?,
            _ => return Err(unexpected(&token)),
        })
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
