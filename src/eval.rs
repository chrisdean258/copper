use crate::builtins::*;
use crate::parser::*;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Clone)]
pub enum Value {
    BuiltinFunc(&'static str, fn(&mut Evaluator, Vec<Value>) -> Value),
    Reference(usize, bool),
    Str(Rc<String>),
    Int(i64),
    Float(f64),
    Char(char),
    Bool(u8),
    Function(Function),
    Null,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        f.write_str("Value::")?;
        match self {
            BuiltinFunc(name, _) => f.write_fmt(format_args!("BuiltinFunc({})", name))?,
            Str(s) => f.write_fmt(format_args!("Str({:?})", s))?,
            Int(i) => f.write_fmt(format_args!("Int({:?})", i))?,
            Float(d) => f.write_fmt(format_args!("Float({:?})", d))?,
            Char(c) => f.write_fmt(format_args!("Char({:?})", c))?,
            Reference(u, b) => f.write_fmt(format_args!(
                "Reference({:x}{})",
                u,
                if *b { "" } else { ", nonnullable" }
            ))?,
            Bool(b) => f.write_fmt(format_args!("Bool({})", b))?,
            Function(func) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", ")))?
            }
            Null => f.write_str("Null")?,
        };
        Ok(())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        match self {
            BuiltinFunc(name, _) => f.write_fmt(format_args!("BuiltinFunc({})", name))?,
            Str(s) => f.write_fmt(format_args!("{}", s))?,
            Int(i) => f.write_fmt(format_args!("{}", i))?,
            Float(d) => f.write_fmt(format_args!("{}", d))?,
            Char(c) => f.write_fmt(format_args!("{}", c))?,
            Reference(u, _) => f.write_fmt(format_args!("0x{:x}", u))?,
            Bool(b) => f.write_fmt(format_args!("{}", if *b != 0 { "true" } else { "false" }))?,
            Function(func) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", ")))?
            }
            Null => f.write_str("null")?,
        };
        Ok(())
    }
}

pub struct Evaluator {
    scopes: Vec<HashMap<String, usize>>,
    pub memory: Vec<Value>,
    // stack: Vec<Value>, // May use this later
}

impl Evaluator {
    pub fn new() -> Evaluator {
        let mut eval = Evaluator {
            scopes: Vec::new(),
            memory: Vec::new(),
        };

        let mut builtins = HashMap::new();

        let idx = eval.alloc(Value::BuiltinFunc("println", copper_print));
        builtins.insert(String::from("println"), idx);
        let idx = eval.alloc(Value::BuiltinFunc("print", copper_print_no_newline));
        builtins.insert(String::from("print"), idx);

        eval.scopes.push(builtins);

        eval
    }

    fn alloc(&mut self, val: Value) -> usize {
        self.memory.push(val);
        self.memory.len() - 1
    }

    fn openscope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_scope(&mut self, key: &str) -> Option<&mut usize> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get_mut(key) {
                return Some(val);
            }
        }
        None
    }

    fn insert_scope(&mut self, key: &str, val: usize) {
        match self.lookup_scope(key) {
            Some(v) => *v = val,
            None => {
                self.scopes.last_mut().unwrap().insert(key.to_string(), val);
            }
        }
    }

    fn insert_scope_local(&mut self, key: &str, val: usize) {
        self.scopes.last_mut().unwrap().insert(key.to_string(), val);
    }

    pub fn deref(&mut self, val: Value) -> Value {
        let mut rv = val;
        loop {
            rv = match rv {
                Value::Reference(u, _) => self.memory[u].clone(),
                _ => break rv,
            }
        }
    }

    pub fn eval(&mut self, tree: &ParseTree) -> Result<(), String> {
        self.openscope();
        for statement in tree.statements.iter() {
            self.eval_statement(&statement)?;
        }
        self.closescope();
        Ok(())
    }

    fn eval_statement(&mut self, statement: &Statement) -> Result<Value, String> {
        use crate::parser::Statement::*;
        Ok(match statement {
            Expr(expr) => self.eval_expr(expr)?,
        })
    }

    fn eval_if(&mut self, i: &If) -> Result<Value, String> {
        let mut ran_first = self.eval_if_internal(i)?;

        for ai in &i.and_bodies {
            ran_first |= self.eval_if_internal(&ai)?;
        }

        if !ran_first && i.else_body.is_some() {
            self.eval_expr(i.else_body.as_ref().unwrap())?;
        }

        Ok(Value::Null)
    }

    fn eval_if_internal(&mut self, i: &If) -> Result<bool, String> {
        let cond = self.eval_expr(&i.condition)?;
        let derefed = self.deref(cond);
        Ok(match derefed {
            Value::Bool(1) => {
                self.eval_expr(&*i.body)?;
                true
            }
            Value::Bool(0) => false,
            _ => {
                return Err(format!(
                    "If expressions must have boolean conditions not {:?}",
                    derefed
                ))
            }
        })
    }

    fn eval_while(&mut self, w: &While) -> Result<Value, String> {
        loop {
            let cond = self.eval_expr(&*w.condition)?;
            let derefed = self.deref(cond);
            match derefed {
                Value::Bool(1) => (),
                Value::Bool(0) => break,
                _ => {
                    return Err(format!(
                        "While loops must have boolean conditions not {:?}",
                        derefed
                    ))
                }
            }
            self.eval_expr(&w.body)?;
        }
        Ok(Value::Null)
    }

    fn eval_block(&mut self, b: &BlockExpr) -> Result<Value, String> {
        let mut rv = Value::Null;
        self.openscope();
        for statement in b.statements.iter() {
            rv = self.eval_statement(&statement)?;
        }
        self.closescope();
        Ok(rv)
    }

    fn eval_binop(&mut self, binop: &BinOp) -> Result<Value, String> {
        use crate::lex::TokenType::*;

        let mut lhs = self.eval_expr(&*binop.lhs)?;
        let mut rhs = self.eval_expr(&*binop.rhs)?;

        if let Value::Reference(u, _) = lhs {
            lhs = self.memory[u].clone();
        }
        if let Value::Reference(u, _) = rhs {
            rhs = self.memory[u].clone();
        }

        Ok(match binop.op.token_type {
            Plus => match (lhs, rhs) {
                (Value::Str(a), Value::Str(b)) => Value::Str(Rc::new(format!("{}{}", a, b))),
                (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a + b as i64),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 + b),
                (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 + b as u8) as char),
                (Value::Char(a), Value::Int(b)) => Value::Char((a as i64 + b) as u8 as char),
                (Value::Int(a), Value::Char(b)) => Value::Char((a + b as i64) as u8 as char),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot add two together. Not supported ({:?} + {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Minus => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a - b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 - b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a - b as i64),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 - b),
                (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 - b as u8) as char),
                (Value::Char(a), Value::Int(b)) => Value::Char((a as i64 - b) as u8 as char),
                (Value::Int(a), Value::Char(b)) => Value::Char((a - b as i64) as u8 as char),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot subtract these two. Not supported ({:?} - {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Times => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a * b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 * b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a * b as i64),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 * b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot multiply these two. Not supported ({:?} * {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Div => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a / b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a / b as f64),
                (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 / b),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 / b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot divide these two. Not supported ({:?} / {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            Mod => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a % b),
                (Value::Char(a), Value::Int(b)) => Value::Int(a as i64 % b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot divide these two. Not supported ({:?} % {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            CmpEq => match (lhs, rhs) {
                (Value::Str(a), Value::Str(b)) => Value::Bool((a == b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a == b) as u8),
                (Value::Bool(a), Value::Bool(b)) => Value::Bool((a == b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a == b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool((a as i64 == b) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool((a as f64 == b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a == b as f64) as u8),
                (Value::Float(a), Value::Float(b)) => Value::Bool((a == b) as u8),
                (a, b) => {
                    return Err(format!(
                    "{}: cannot compare equality between these two. Not supported ({:?} == {:?})",
                    binop.op.location, a, b
                ))
                }
            },
            CmpNotEq => match (lhs, rhs) {
                (Value::Str(a), Value::Str(b)) => Value::Bool((a != b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a != b) as u8),
                (Value::Bool(a), Value::Bool(b)) => Value::Bool((a != b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a != b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool((a as i64 != b) as u8),
                (Value::Int(a), Value::Char(b)) => Value::Bool((a != b as i64) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool((a as f64 != b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a != b as f64) as u8),
                (Value::Float(a), Value::Float(b)) => Value::Bool((a != b) as u8),
                (a, b) => {
                    return Err(format!(
                    "{}: cannot compare equality between these two. Not supported ({:?} != {:?})",
                    binop.op.location, a, b
                ))
                }
            },
            CmpGE => match (lhs, rhs) {
                (Value::Float(a), Value::Float(b)) => Value::Bool((a >= b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a >= b as f64) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool((a as f64 >= b) as u8),
                (Value::Str(a), Value::Str(b)) => Value::Bool((a >= b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a >= b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a >= b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool((a as i64 >= b) as u8),
                (Value::Int(a), Value::Char(b)) => Value::Bool((a >= b as i64) as u8),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} >= {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            CmpGT => match (lhs, rhs) {
                (Value::Float(a), Value::Float(b)) => Value::Bool((a > b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a > b as f64) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool((a as f64 > b) as u8),
                (Value::Str(a), Value::Str(b)) => Value::Bool((a > b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a > b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a > b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool((a as i64 > b) as u8),
                (Value::Int(a), Value::Char(b)) => Value::Bool((a > b as i64) as u8),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} > {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            CmpLE => match (lhs, rhs) {
                (Value::Float(a), Value::Float(b)) => Value::Bool((a <= b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a <= b as f64) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool((a as f64 <= b) as u8),
                (Value::Str(a), Value::Str(b)) => Value::Bool((a <= b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a <= b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a <= b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool((a as i64 <= b) as u8),
                (Value::Int(a), Value::Char(b)) => Value::Bool((a <= b as i64) as u8),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} <= {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            CmpLT => match (lhs, rhs) {
                (Value::Float(a), Value::Float(b)) => Value::Bool((a < b) as u8),
                (Value::Float(a), Value::Int(b)) => Value::Bool((a < b as f64) as u8),
                (Value::Int(a), Value::Float(b)) => Value::Bool(((a as f64) < b) as u8),
                (Value::Str(a), Value::Str(b)) => Value::Bool((a < b) as u8),
                (Value::Int(a), Value::Int(b)) => Value::Bool((a < b) as u8),
                (Value::Char(a), Value::Char(b)) => Value::Bool((a < b) as u8),
                (Value::Char(a), Value::Int(b)) => Value::Bool(((a as i64) < b) as u8),
                (Value::Int(a), Value::Char(b)) => Value::Bool((a < b as i64) as u8),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compare order between these two. Not supported ({:?} < {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BitOr => match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a | b),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 | b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a | b as i64),
                (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
                (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 | b as u8) as char),
                (Value::Char(a), Value::Int(b)) => Value::Int((a as i64) | b),
                (Value::Int(a), Value::Char(b)) => Value::Int(a | b as i64),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compute or between these two. Not supported ({:?} | {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BitAnd => match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a & b),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 & b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a & b as i64),
                (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
                (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 & b as u8) as char),
                (Value::Char(a), Value::Int(b)) => Value::Int((a as i64) & b),
                (Value::Int(a), Value::Char(b)) => Value::Int(a & b as i64),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot computer and between these two. Not supported ({:?} & {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BitXor => match (lhs, rhs) {
                (Value::Bool(a), Value::Bool(b)) => Value::Bool(a ^ b),
                (Value::Bool(a), Value::Int(b)) => Value::Int(a as i64 ^ b),
                (Value::Int(a), Value::Bool(b)) => Value::Int(a ^ b as i64),
                (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
                (Value::Char(a), Value::Char(b)) => Value::Char((a as u8 ^ b as u8) as char),
                (Value::Char(a), Value::Int(b)) => Value::Int((a as i64) ^ b),
                (Value::Int(a), Value::Char(b)) => Value::Int(a ^ b as i64),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot computer xor between these two. Not supported ({:?} ^ {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BoolOr => todo!("Need operator short circuiting. For now use nested ifs for short circuit stye behavior"),
            BoolAnd => todo!("Need operator short circuiting. For now use nested ifs for short circuit stye behavior"),
            BoolXor => todo!("Going to implement with the other two boolean functions"),
            BitShiftRight => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a >> b),
                (Value::Char(a), Value::Int(b)) => Value::Int((a as i64) >> b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compute bitshift between these two. Not supported ({:?} >> {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            BitShiftLeft => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a << b),
                (Value::Char(a), Value::Int(b)) => Value::Int((a as i64) << b),
                (a, b) => {
                    return Err(format!(
                        "{}: cannot compute bitshift between these two. Not supported ({:?} << {:?})",
                        binop.op.location, a, b
                    ))
                }
            },
            _ => {
                return Err(format!(
                    "{}: {:?}, unimplemented",
                    binop.op.location, binop.op.token_type
                ))
            }
        })
    }

    fn eval_expr(&mut self, expr: &Expression) -> Result<Value, String> {
        use crate::parser::Expression::*;
        Ok(match expr {
            CallExpr(callexpr) => self.eval_call_expr(&callexpr)?,
            RefExpr(refexpr) => self.eval_ref_expr(&refexpr, false)?,
            Immediate(immediate) => self.eval_immediate(&immediate)?,
            BlockExpr(blockexpr) => self.eval_block(blockexpr)?,
            BinOp(binop) => self.eval_binop(binop)?,
            AssignExpr(assignexpr) => self.eval_assign(assignexpr)?,
            While(w) => self.eval_while(w)?,
            If(i) => self.eval_if(i)?,
            Function(f) => self.eval_function_def(f)?,
            PreUnOp(_) => todo!(),
            PostUnOp(_) => todo!(),
        })
    }

    fn eval_function_def(&mut self, f: &Function) -> Result<Value, String> {
        Ok(Value::Function(f.clone()))
    }

    fn eval_assign(&mut self, expr: &AssignExpr) -> Result<Value, String> {
        use crate::lex::TokenType;
        use std::mem::discriminant as disc;
        let lhs_asref = self.eval_ref_expr(&*expr.lhs, expr.allow_decl)?;
        let rhs_pos_ref = self.eval_expr(&*expr.rhs)?;

        let rhs = match rhs_pos_ref {
            Value::Reference(u, _) => self.memory[u].clone(),
            _ => rhs_pos_ref,
        };

        let (mut lhs, nullable) = match lhs_asref {
            Value::Reference(u, b) => (self.memory.get_mut(u).unwrap(), b),
            _ => unreachable!(),
        };

        match &expr.op.token_type {
            TokenType::Equal => {
                // Force types to be the same or throw an error
                if disc(lhs) == disc(&rhs) {
                    *lhs = rhs.clone();
                } else if nullable && disc(&rhs) == disc(&Value::Null) {
                    *lhs = rhs.clone();
                } else if disc(lhs) == disc(&mut Value::Null) {
                    *lhs = rhs.clone();
                } else {
                    return Err(format!(
                        "{}: cannot assign {:?} <= {:?}",
                        expr.op.location, lhs, rhs
                    ));
                }
            }
            TokenType::AndEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a &= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 & *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a &= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a &= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 & *b as u8) as char,
                _ => todo!(),
            },
            TokenType::OrEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a |= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 | *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a |= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a |= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 | *b as u8) as char,
                _ => todo!(),
            },
            TokenType::XorEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a ^= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 ^ *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a ^= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a ^= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 ^ *b as u8) as char,
                _ => todo!(),
            },
            TokenType::PlusEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a += *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a += *b as i64,
                (Value::Int(a), Value::Char(b)) => *a += *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a += *b,
                (Value::Float(a), Value::Bool(b)) => *a += *b as f64,
                (Value::Float(a), Value::Char(b)) => *a += (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a + *b as f64,
                _ => todo!(),
            },
            TokenType::MinusEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a -= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a -= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a -= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a -= *b,
                (Value::Float(a), Value::Bool(b)) => *a -= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a -= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a - *b as f64,
                _ => todo!(),
            },
            TokenType::TimesEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a *= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a *= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a *= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a *= *b,
                (Value::Float(a), Value::Bool(b)) => *a *= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a *= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => todo!(),
            },
            TokenType::DivEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a /= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a /= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a /= *b,
                (Value::Float(a), Value::Char(b)) => *a /= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => todo!(),
            },
            TokenType::ModEq => match (&mut lhs, &rhs) {
                (Value::Int(a), Value::Int(b)) => *a %= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a %= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Float(a), Value::Char(b)) => *a %= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => todo!(),
            },
            TokenType::BitShiftRightEq => todo!(),
            TokenType::BitShiftLeftEq => todo!(),
            _ => todo!(),
        }
        Ok(lhs.clone())
    }

    fn eval_call_expr(&mut self, expr: &CallExpr) -> Result<Value, String> {
        let func = self.eval_expr(&expr.function)?;
        let func = match &func {
            Value::Reference(u, _) => self.memory[*u].clone(),
            _ => func,
        };
        let mut args = Vec::new();
        for arg in expr.args.iter() {
            args.push(self.eval_expr(arg)?);
        }
        Ok(match &func {
            Value::BuiltinFunc(_, f) => f(self, args),
            Value::Function(f) => {
                if args.len() != f.argnames.len() {
                    return Err(format!(
                        "Trying to call function with wrong number of args. wanted {} found {}",
                        args.len(),
                        f.argnames.len()
                    ));
                }
                self.openscope();
                for it in f.argnames.iter().zip(args.iter()) {
                    let (name, arg) = it;
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(name, idx);
                }
                let rv = self.eval_expr(&*f.body)?;
                rv
            }
            f => {
                println!("{:#?}", f);
                unreachable!();
            }
        })
    }

    fn eval_ref_expr(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<Value, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => {
                if let Some(idx) = self.lookup_scope(s) {
                    Ok(Value::Reference(*idx, false))
                } else if allow_insert {
                    let idx = self.alloc(Value::Null);
                    self.insert_scope(s, idx);
                    Ok(Value::Reference(idx, false))
                } else {
                    Err(format!("{}: No such name in scope...", s))
                }
            }
            _ => Err("Unexpected...".into()),
        }
    }

    fn eval_immediate(&mut self, immediate: &Immediate) -> Result<Value, String> {
        use crate::lex::TokenType::*;
        Ok(match &immediate.value.token_type {
            Str(s) => Value::Str(Rc::new(s.clone())),
            Int(i) => Value::Int(i.clone()),
            Float(f) => Value::Float(f.clone()),
            Char(c) => Value::Char(c.clone()),
            Bool(b) => Value::Bool(*b),
            Null => Value::Null,
            _ => return Err(format!("Unexpected immediate value {:?}", immediate.value)),
        })
    }
}
