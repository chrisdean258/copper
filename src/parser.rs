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
    CallExpr(CallExpr),
    RefExpr(RefExpr),
    Immediate(Immediate),
    BlockExpr(BlockExpr),
    BinOp(BinOp),
    PreUnOp(PreUnOp),
    PostUnOp(PostUnOp),
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
        use crate::lex::TokenType::*;
        use std::mem::discriminant as disc;
        let token = lexer.peek().unwrap();
        let rv = Ok(Statement::Expr(match &token.token_type {
            CloseParen => return Err(err_msg(&token, "Unexpected closing paren")),
            Comma => return Err(err_msg(&token, "Unexpected comma")),
            Semicolon => return Err(err_msg(&token, "Unexpected semicolon")),
            Dot => return Err(err_msg(&token, "Unexpected dot")),
            Equal => return Err(err_msg(&token, "Unexpected equal")),
            OpenParen => self.parse_expr(lexer)?,
            OpenBrace => self.parse_block(lexer)?,
            Identifier(_) => self.parse_expr(lexer)?,
            Str(_) => self.parse_expr(lexer)?,
            Int(_) => self.parse_expr(lexer)?,
            Float(_) => self.parse_expr(lexer)?,
            Char(_) => self.parse_expr(lexer)?,
            ErrChar(c) => return Err(err_msg(&token, &format!("{:?}", c))),
            Keyword(_) => todo!(),
            CloseBrace => todo!(),
            _ => todo!(),
        }));
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

    binop! {parse_eq, parse_boolean_op, Equal, AndEq, XorEq, PlusEq, MinusEq, TimesEq, DivEq, ModEq}
    binop! {parse_boolean_op, parse_bitwise_or, BoolOr, BoolXor, BoolAnd}
    binop! {parse_bitwise_or, parse_bitwise_xor, BitOr}
    binop! {parse_bitwise_xor, parse_bitwise_and, BitXor}
    binop! {parse_bitwise_and, parse_comparision, BitAnd}

    // Todo: Comparision operator chaining
    binop! {parse_comparision, parse_bitshift, CmpEqual, CmpGE, CmpGT, CmpLE, CmpLT }

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
                TokenType::OpenParen => self.parse_paren(lexer),
                _ => Err(unexpected(&token)),
            }
        } else {
            self.parse_paren(lexer)
        }
    }

    fn parse_paren<T: Iterator<Item = String>>(
        &mut self,
        _lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        todo!()
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
