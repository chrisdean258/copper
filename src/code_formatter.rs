use crate::lex::*;
use crate::parser::*;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};

thread_local!(static INDENT_DEPTH: RefCell<usize> = RefCell::new(0));

fn indent(f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
    for _ in 0..INDENT_DEPTH.with(|t| *t.borrow()) {
        f.write_str("\t")?;
    }
    Ok(())
}

fn indent_nl(f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
    f.write_str("\n")?;
    indent(f)
}

fn incindent() {
    INDENT_DEPTH.with(|t| *t.borrow_mut() += 1)
}

fn decindent() {
    INDENT_DEPTH.with(|t| *t.borrow_mut() -= 1)
}

impl Display for ParseTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        for stat in &self.statements {
            if !first {
                indent_nl(f)?;
                first = false
            }
            f.write_fmt(format_args!("{}", stat))?;
        }
        Ok(())
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Statement::Expr(e) => f.write_fmt(format_args!("{};", e)),
            Statement::GlobalDecl(g) => f.write_fmt(format_args!("{}", g)),
        }
    }
}

impl Display for GlobalDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let name = match &self.token.token_type {
            TokenType::Identifier(s) => s,
            _ => unreachable!(),
        };
        f.write_fmt(format_args!("global {};\n", name))
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Expression::While(x) => f.write_fmt(format_args!("{}", x)),
            Expression::If(x) => f.write_fmt(format_args!("{}", x)),
            Expression::CallExpr(x) => f.write_fmt(format_args!("{}", x)),
            Expression::RefExpr(x) => f.write_fmt(format_args!("{}", x)),
            Expression::Immediate(x) => f.write_fmt(format_args!("{}", x)),
            Expression::BlockExpr(x) => f.write_fmt(format_args!("{}", x)),
            Expression::BinOp(x) => f.write_fmt(format_args!("{}", x)),
            Expression::PreUnOp(x) => f.write_fmt(format_args!("{}", x)),
            Expression::PostUnOp(x) => f.write_fmt(format_args!("{}", x)),
            Expression::AssignExpr(x) => f.write_fmt(format_args!("{}", x)),
            Expression::Function(x) => f.write_fmt(format_args!("{}", x)),
            Expression::Lambda(x) => f.write_fmt(format_args!("{}", x)),
            Expression::List(x) => f.write_fmt(format_args!("{}", x)),
        }
    }
}

impl Display for List {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let exprs: Vec<String> = self.exprs.iter().map(|e| format!("{}", e)).collect();
        f.write_fmt(format_args!("[{}]", exprs.join(", ")))
    }
}

impl Display for While {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!(
            "while ({}) {}",
            self.condition.as_ref(),
            self.body.as_ref()
        ))
    }
}
impl Display for If {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!(
            "if ({}) {}",
            self.condition.as_ref(),
            self.body.as_ref()
        ))?;
        for ab in &self.and_bodies {
            indent_nl(f)?;
            f.write_fmt(format_args!("and {}", ab))?;
        }
        match &self.else_body {
            None => (),
            Some(i) => {
                indent_nl(f)?;
                f.write_fmt(format_args!("else {}", i))?;
            }
        }

        Ok(())
    }
}

impl Display for CallExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut first = true;
        f.write_fmt(format_args!("{}(", self.function.as_ref()))?;
        for arg in &self.args {
            if !first {
                f.write_str(", ")?;
                first = false;
            }
            f.write_fmt(format_args!("{}", arg))?;
        }
        f.write_str(")")?;
        Ok(())
    }
}

impl Display for RefExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.value.token_type))
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}", self.value.token_type))
    }
}
impl Display for BlockExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str("{")?;
        incindent();
        for stat in &self.statements {
            indent_nl(f)?;
            f.write_fmt(format_args!("{}", stat))?;
        }
        decindent();
        indent_nl(f)?;
        f.write_str("}")
    }
}
impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!(
            "{} {} {}",
            self.lhs.as_ref(),
            self.op.token_type,
            self.rhs.as_ref()
        ))
    }
}
impl Display for PreUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}{}", self.op.token_type, self.rhs.as_ref()))
    }
}
impl Display for PostUnOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!("{}{}", self.lhs.as_ref(), self.op.token_type,))
    }
}
impl Display for AssignExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_fmt(format_args!(
            "{} {} {}",
            self.lhs.as_ref(),
            self.op.token_type,
            self.rhs.as_ref()
        ))
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str("fn")?;
        match &self.name {
            Some(s) => f.write_fmt(format_args!(" {}(", s))?,
            None => f.write_str("(")?,
        }
        let arg_str = self.argnames.join(", ");
        f.write_fmt(format_args!("{}) ", arg_str))?;
        f.write_fmt(format_args!("{}", self.body.as_ref()))
    }
}

impl Display for Lambda {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let body = format!("{}", self.body.as_ref());
        if !body.starts_with('\\') {
            f.write_str("\\")?;
        }
        f.write_fmt(format_args!("{}", body))
    }
}
