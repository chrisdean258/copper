use crate::lex::*;
use crate::location::Location;
use std::collections::{HashMap, HashSet};
use std::iter::Peekable;

pub struct Parser<T: Iterator<Item = String>> {
    lexer: Lexer<T>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(Expression),
    GlobalDecl(GlobalDecl),
    ClassDecl(ClassDecl),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expression {
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
    Function(Function),
    Lambda(Lambda),
    List(List),
    IndexExpr(IndexExpr),
    DottedLookup(DottedLookup),
}

impl Expression {
    pub fn location(&self) -> Location {
        match self {
            Expression::While(w) => w.location.clone(),
            Expression::For(f) => f.location.clone(),
            Expression::If(i) => i.location.clone(),
            Expression::CallExpr(c) => c.location.clone(),
            Expression::RefExpr(r) => r.value.location.clone(),
            Expression::Immediate(i) => i.value.location.clone(),
            Expression::BlockExpr(b) => b.location.clone(),
            Expression::BinOp(b) => b.op.location.clone(),
            Expression::PreUnOp(u) => u.op.location.clone(),
            Expression::PostUnOp(u) => u.op.location.clone(),
            Expression::AssignExpr(a) => a.op.location.clone(),
            Expression::Function(f) => f.location.clone(),
            Expression::Lambda(l) => l.location.clone(),
            Expression::List(l) => l.location.clone(),
            Expression::IndexExpr(l) => l.location.clone(),
            Expression::DottedLookup(d) => d.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseTree {
    pub statements: Vec<Statement>,
    max_arg: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct GlobalDecl {
    pub token: Token,
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
    pub location: Location,
    pub condition: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub location: Location,
    pub reference: Box<Expression>,
    pub items: Box<Expression>,
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
pub struct IndexExpr {
    pub location: Location,
    pub obj: Box<Expression>,
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
    pub lhs: Box<Expression>,
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
    pub num_args: usize,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct List {
    pub location: Location,
    pub exprs: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct DottedLookup {
    pub location: Location,
    pub lhs: Box<Expression>,
    pub rhs: String,
}

#[allow(dead_code)]
fn err_msg(token: &Token, reason: &str) -> String {
    format!("{}: {}", token.location, reason)
}

fn unexpected(token: &Token) -> String {
    panic!("{}: Unexpected `{}`", token.location, token.token_type)
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
    ( $lexer:ident, $token:path ) => {{
        let token = $lexer.peek().ok_or("Unexpected EOF")?;
        match &token.token_type {
            $token => $lexer.next().unwrap(),
            _ => return Err(unexpected(&token)),
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
            TokenType::Global => self.parse_global_decl(lexer)?,
            TokenType::Class => self.parse_class_decl(lexer)?,
            _ => Statement::Expr(self.parse_expr(lexer)?),
        });
        // eat all the semicolons
        while if_expect!(lexer, TokenType::Semicolon) {}
        rv
    }

    fn parse_global_decl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        expect!(lexer, TokenType::Global);
        let token = lexer.next().ok_or("Unexpected EOF")?;
        match &token.token_type {
            TokenType::Identifier(_) => (),
            _ => return Err(unexpected(&token)),
        }
        Ok(Statement::GlobalDecl(GlobalDecl { token }))
    }

    fn parse_class_decl<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Statement, String> {
        let location = expect!(lexer, TokenType::Class).location;
        let token = lexer.next().ok_or("Unexpected EOF")?;
        let name = match &token.token_type {
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
                    let fun = match self.parse_function(lexer, true)? {
                        Expression::Function(f) => f,
                        _ => unreachable!(),
                    };
                    if methods
                        .insert(fun.name.as_ref().unwrap().clone(), fun.clone())
                        .is_some()
                    {
                        return Err(format!(
                            "{}: redefinition of method `{}`",
                            fun.location,
                            fun.name.unwrap()
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

        Ok(Statement::ClassDecl(ClassDecl {
            location,
            name,
            fields,
            methods,
        }))
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

    fn parse_for<T: Iterator<Item = String>>(
        &mut self,
        lexer: &mut Peekable<Lexer<T>>,
    ) -> Result<Expression, String> {
        let location = expect!(lexer, TokenType::For).location;
        expect!(lexer, TokenType::OpenParen);
        let reference = Box::new(self.parse_ref(lexer)?);
        expect!(lexer, TokenType::In);
        let items = Box::new(self.parse_expr(lexer)?);
        expect!(lexer, TokenType::CloseParen);
        let body = Box::new(self.parse_expr(lexer)?);

        Ok(Expression::For(For {
            location,
            reference,
            items,
            body,
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

        let mut token = match lexer.peek() {
            Some(t) => t.clone(),
            None => return Ok(lhs),
        };
        let mut allow_decl = false;
        let reflhs = match &mut token.token_type {
            Equal => {
                lexer.next();
                allow_decl = true;
                match lhs {
                    Expression::RefExpr(_) => lhs,
                    Expression::IndexExpr(_) => lhs,
                    Expression::DottedLookup(_) => lhs,
                    _ => return Err(unexpected(&token)),
                }
            }
            AndEq | XorEq | OrEq | PlusEq | MinusEq | TimesEq | DivEq | ModEq => match lhs {
                Expression::RefExpr(_) => {
                    lexer.next();
                    lhs
                }
                Expression::IndexExpr(_) => {
                    lexer.next();
                    lhs
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
            let mut needs_ref = false;
            match token.token_type {
                TokenType::BoolNot => (),
                TokenType::BitNot => (),
                TokenType::Minus => (),
                TokenType::Plus => (),
                TokenType::Inc => {
                    needs_ref = true;
                }
                TokenType::Dec => {
                    needs_ref = true;
                }
                TokenType::Times => (),
                _ => return self.parse_post_unary(lexer),
            }
            let token = lexer.next().unwrap();
            let rhs = self.parse_pre_unary(lexer)?;
            if needs_ref {
                match rhs {
                    Expression::RefExpr(_) => (),
                    _ => return Err(unexpected(&token)),
                };
            }
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
                TokenType::OpenBracket => {
                    let location = expect!(lexer, TokenType::OpenBracket).location;
                    let args = self.parse_cse(lexer)?;
                    expect!(lexer, TokenType::CloseBracket).location;
                    Expression::IndexExpr(IndexExpr {
                        location: location,
                        obj: Box::new(lhs),
                        args,
                    })
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
        loop {
            let token = lexer.next().ok_or("Unexpected EOF")?;
            match &token.token_type {
                TokenType::Identifier(s) => args.push(s.clone()),
                TokenType::CloseParen => break,
                _ => return Err(unexpected(&token)),
            }
            let token = lexer.next().ok_or("Unexpected EOF")?;
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
        let num_args = self.max_arg.pop().unwrap();
        Ok(Expression::Lambda(Lambda {
            location,
            num_args,
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
            let mut rv = match &token.token_type {
                TokenType::Identifier(_) => Ok(Expression::RefExpr(RefExpr {
                    value: lexer.next().unwrap(),
                })),
                TokenType::LambdaArg(a) if self.max_arg.len() > 0 => {
                    if *a + 1 > *self.max_arg.last().unwrap() {
                        *self.max_arg.last_mut().unwrap() = *a + 1;
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
        Ok(Expression::DottedLookup(DottedLookup {
            location,
            lhs: Box::new(lhs),
            rhs,
        }))
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
        Ok(Expression::List(List {
            location: loc,
            exprs: exprs,
        }))
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
