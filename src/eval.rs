use crate::builtins::*;
use crate::parser::*;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub enum Value {
    BuiltinFunc(fn(&mut Evaluator, Vec<usize>) -> Value),
    Reference(usize),
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
            Reference(u) => f.write_fmt(format_args!("Reference({:?})", u))?,
            Null => f.write_str("null")?,
        };
        Ok(())
    }
}

pub struct Evaluator {
    scopes: Vec<HashMap<String, usize>>,
    pub values: Vec<Value>,
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

    fn openscope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn insert_scope(&mut self, key: String, val: usize) {
        self.scopes.last_mut().unwrap().insert(key, val);
    }

    // pub fn alloc(&mut self) -> usize {
    // let rv = self._alloc();
    // println!("first num {:x}", self.free[0]);
    // println!("allocing {}", rv);
    // rv
    // }

    pub fn alloc(&mut self) -> usize {
        let mut index: usize = 0;
        while index < self.free.len() {
            if self.free[index] != 0 {
                let rtnidx = self.free[index].trailing_zeros() as usize;
                self.free[index] &= !(1 << rtnidx);
                return index * 64 + rtnidx;
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
        self.openscope();
        for statement in tree.statements.iter() {
            self.eval_statement(&statement)?;
        }
        self.closescope();
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
            EqualExpr(equalexpr) => match &*equalexpr.lhs {
                RefExpr(r) => {
                    let idx = self.eval_ref_expr_or_alloc(&r)?;
                    let rhs = self.eval_expr(&equalexpr.rhs)?;
                    self.values[idx] = Value::Reference(rhs);
                    idx
                }
                _ => todo!(),
            },
            // e => {
            // println!("{:#?}", e);
            // todo!()
            // }
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
            Value::BuiltinFunc(f) => f(self, args),
            Value::Reference(u) => {
                let val = self.lookup_reference(u);
                println!("{:#?}", self.values[val]);
                todo!()
            }
            f => {
                println!("{:#?}", f);
                unreachable!();
            }
        };
        Ok(self.add_val(rtn))
    }

    fn lookup_reference(&self, idx: usize) -> usize {
        let mut i = idx;
        loop {
            match self.values[i] {
                Value::Reference(u) => {
                    i = u;
                }
                _ => return i,
            }
            if i == idx {
                panic!("Refernce loop")
            }
        }
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

    fn eval_ref_expr_or_alloc(&mut self, expr: &RefExpr) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        match self.eval_ref_expr(expr) {
            Ok(v) => return Ok(v),
            _ => (),
        };
        match &expr.value.token_type {
            Identifier(s) => {
                let new_val = self.alloc();
                self.insert_scope(s.clone(), new_val);
                Ok(new_val)
            }
            _ => todo!(),
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
