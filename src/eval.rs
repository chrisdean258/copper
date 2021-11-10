use crate::builtins::*;
use crate::parser::*;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub enum Value {
    BuiltinFunc(fn(&mut Vec<Value>, Vec<usize>) -> Value),
    Str(String),
    Int(i64),
    Float(f64),
    Char(char),
    Null,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        f.write_str("Value::")?;
        match self {
            BuiltinFunc(_) => f.write_str("BuiltinFunc")?,
            Str(s) => f.write_fmt(format_args!("Str({:?})", s))?,
            Int(i) => f.write_fmt(format_args!("Int({:?})", i))?,
            Float(d) => f.write_fmt(format_args!("Float({:?})", d))?,
            Char(c) => f.write_fmt(format_args!("Char({:?})", c))?,
            Null => f.write_str("null")?,
        };
        Ok(())
    }
}

pub struct Evaluator {
    scopes: Vec<HashMap<String, usize>>,
    values: Vec<Value>,
    free: Vec<u64>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        let mut eval = Evaluator {
            scopes: Vec::new(),
            values: Vec::new(),
            free: Vec::new(),
        };

        let mut builtins = HashMap::new();
        builtins.insert(
            String::from("print"),
            eval.add_val(Value::BuiltinFunc(copper_print)),
        );

        eval.scopes.push(builtins);

        eval
    }

    pub fn alloc(&mut self) -> usize {
        let mut index: usize = 0;
        while index < self.free.len() {
            if self.free[index] != 0 {
                return index * 64 + self.free[index].leading_zeros() as usize;
            }
            index += 1
        }
        let mut new_vals: Vec<Value> = [0; 64].iter().map(|_| Value::Null).collect();
        self.values.append(&mut new_vals);
        self.free.push(!(1 as u64));
        index * 64
    }

    pub fn add_val(&mut self, val: Value) -> usize {
        let idx = self.alloc();
        self.values[idx] = val;
        idx
    }

    pub fn lookup(&self, key: &str) -> Option<usize> {
        for scope in self.scopes.iter().rev() {
            if let Some(&rv) = scope.get(key) {
                return Some(rv);
            }
        }
        None
    }

    pub fn eval(&mut self, tree: &ParseTree) -> Result<(), String> {
        for statement in tree.statements.iter() {
            self.eval_statement(&statement)?;
        }
        Ok(())
    }

    fn eval_statement(&mut self, statement: &Statement) -> Result<(), String> {
        use crate::parser::Statement::*;
        match statement {
            Expr(expr) => self.eval_expr(expr)?,
        };
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expression) -> Result<usize, String> {
        use crate::parser::Expression::*;
        Ok(match expr {
            CallExpr(callexpr) => self.eval_call_expr(&callexpr)?,
            RefExpr(refexpr) => self.eval_ref_expr(&refexpr)?,
            Immediate(immediate) => self.eval_immediate(&immediate)?,
            _ => todo!(),
        })
    }

    fn eval_call_expr(&mut self, expr: &CallExpr) -> Result<usize, String> {
        let funcidx = self.eval_expr(&expr.function)?;
        let func = self.values[funcidx].clone();
        let mut args = Vec::new();
        for arg in expr.args.iter() {
            args.push(self.eval_expr(arg)?);
        }
        let rtn = match func {
            Value::BuiltinFunc(f) => f(&mut self.values, args),
            _ => unreachable!(),
        };
        Ok(self.add_val(rtn))
    }

    fn eval_ref_expr(&mut self, expr: &RefExpr) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => match self.lookup(s) {
                Some(idx) => Ok(idx),
                _ => Err(format!("{}: No such name in scope...", s)),
            },
            _ => Err("Unexpected...".into()),
        }
    }

    fn eval_immediate(&mut self, immediate: &Immediate) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        Ok(self.add_val(match &immediate.value.token_type {
            Str(s) => Value::Str(s.clone()),
            Int(i) => Value::Int(i.clone()),
            Float(f) => Value::Float(f.clone()),
            Char(c) => Value::Char(c.clone()),
            _ => return Err("Unexpected...".into()),
        }))
    }
}
