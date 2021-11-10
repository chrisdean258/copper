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
    EqualExpr(EqualExpr),
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub function: Box<Expression>,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct EqualExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct RefExpr {
    pub value: Token,
}

#[derive(Debug, Clone)]
pub struct Immediate {
    pub value: Token,
}

#[derive(Debug)]
pub struct ParseTree {
    pub statements: Vec<Statement>,
}

fn err_msg(token: &Token, reason: &str) -> String {
    format!("{}: {}", token.location, reason)
}

fn unexpected(unexpected: &Token, expected: Vec<TokenType>) -> String {
    let expected_str = expected
        .iter()
        .map(|t| format!("{:?}", t))
        .reduce(|a, b| format!("{}, {}", a, b))
        .unwrap();
    let reason = format!("Expected one of ({}), found {:?}", expected_str, unexpected);
    err_msg(unexpected, &reason)
}

impl ParseTree {
    pub fn new() -> ParseTree {
        ParseTree {
            statements: Vec::new(),
        }
    }

    pub fn expect<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
        expected: Vec<TokenType>,
    ) -> Result<(), String> {
        use std::mem::discriminant;
        let token = lexer.peek().unwrap();
        let disc = discriminant(&token.token_type);
        for v in expected.clone() {
            if disc == discriminant(&v) {
                lexer.next();
                return Ok(());
            }
        }

        Err(unexpected(&token, expected))
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
        use std::mem::discriminant;
        let token = lexer.peek().unwrap();
        let rv = Ok(Statement::Expr(match &token.token_type {
            CloseParen => return Err(err_msg(&token, "Unexpected closing paren")),
            Comma => return Err(err_msg(&token, "Unexpected comma")),
            Semicolon => return Err(err_msg(&token, "Unexpected semicolon")),
            Dot => return Err(err_msg(&token, "Unexpected dot")),
            Equal => return Err(err_msg(&token, "Unexpected equal")),
            OpenParen => self.parse_expr(lexer)?,
            Identifier(_) => self.parse_expr(lexer)?,
            Str(_) => self.parse_expr(lexer)?,
            Int(_) => self.parse_expr(lexer)?,
            Float(_) => self.parse_expr(lexer)?,
            Char(_) => self.parse_expr(lexer)?,
            ErrChar(c) => return Err(err_msg(&token, &format!("{:?}", c))),
        }));
        lexer.next_if(|t| discriminant(&t.token_type) == discriminant(&Semicolon));
        rv
    }

    fn parse_expr_id<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        lexer.record();
        let idtoken = lexer.next().unwrap();
        let token = lexer.peek();
        if token.is_none() {
            lexer.commit();
            return Ok(Expression::RefExpr(RefExpr { value: idtoken }));
        }
        lexer.rollback();
        let token = token.unwrap();
        match &token.token_type {
            TokenType::Identifier(_) => todo!(),
            _ => (),
        }

        let lhs = self.parse_ref(lexer)?;
        let token = match lexer.peek() {
            None => return Ok(lhs),
            Some(t) => t,
        };
        Ok(match &token.token_type {
            TokenType::OpenParen => self.parse_call_args(lexer, lhs)?,
            TokenType::Equal => self.parse_equal_expr(lexer, lhs)?,
            TokenType::Semicolon => lhs,
            TokenType::CloseParen => lhs,
            t => {
                println!("{:?}", t);
                todo!()
            }
        })
    }

    fn parse_equal_expr<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
        lhs: Expression,
    ) -> Result<Expression, String> {
        use std::mem::discriminant;
        if lexer
            .next_if(|t| discriminant(&TokenType::Equal) == discriminant(&t.token_type))
            .is_none()
        {
            unreachable!();
        }
        let rhs = self.parse_expr(lexer)?;
        Ok(Expression::EqualExpr(EqualExpr {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }))
    }

    fn parse_expr<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let token = lexer.peek().unwrap();
        Ok(match token.token_type {
            Identifier(_) => self.parse_expr_id(lexer)?,
            Str(_) => Expression::Immediate(Immediate {
                value: lexer.next().unwrap(),
            }),
            Int(_) => Expression::Immediate(Immediate {
                value: lexer.next().unwrap(),
            }),
            Float(_) => Expression::Immediate(Immediate {
                value: lexer.next().unwrap(),
            }),
            OpenParen => todo!(),
            CloseParen => todo!(),
            Comma => todo!(),
            Dot => todo!(),
            Equal => todo!(),
            Char(_) => Expression::Immediate(Immediate {
                value: lexer.next().unwrap(),
            }),
            ErrChar(c) => {
                return Err(err_msg(
                    &lexer.next().unwrap(),
                    &format!("unexpected char {}", c),
                ))
            }
            Semicolon => {
                return Err(err_msg(
                    &lexer.next().unwrap(),
                    &format!("unexpected semicolon"),
                ))
            }
        })
    }

    fn parse_call_args<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
        function: Expression,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let mut args: Vec<Expression> = Vec::new();
        let mut need_comma = false;
        self.expect(lexer, vec![OpenParen])?;
        loop {
            if let Some(token) = lexer.peek() {
                match token.token_type {
                    CloseParen => {
                        lexer.next();
                        break;
                    }
                    Comma => {
                        if need_comma {
                            continue;
                        } else {
                            return Err(err_msg(&token, "unexpected comma"));
                        }
                    }
                    _ => {
                        if need_comma {
                            return Err(err_msg(&token, "expected comma"));
                        } else {
                            args.push(self.parse_expr(lexer)?);
                            need_comma = true;
                        }
                    }
                }
            } else {
                return Err("Unexpected EOF while parsing arguments to call expression".into());
            }
        }
        Ok(Expression::CallExpr(CallExpr {
            function: Box::new(function),
            args,
        }))
    }

    fn parse_ref<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut ReIterable<Lexer<T>>,
    ) -> Result<Expression, String> {
        use crate::lex::TokenType::*;
        let token = lexer.next().unwrap();
        match token.token_type {
            Identifier(_) => (),
            _ => return Err(unexpected(&token, vec![Identifier("".into())])),
        }

        Ok(Expression::RefExpr(RefExpr { value: token }))
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
