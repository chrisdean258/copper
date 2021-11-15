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
    Lambda(Lambda),
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
            Lambda(lambda) => f.write_fmt(format_args!("lambda(args={})", lambda.max_arg))?,
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
            Reference(u, _) => f.write_fmt(format_args!("Reference(0x{:x})", u))?,
            Bool(b) => f.write_fmt(format_args!("{}", if *b != 0 { "true" } else { "false" }))?,
            Function(func) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", ")))?
            }
            Lambda(lambda) => f.write_fmt(format_args!("lambda(args={})", lambda.max_arg))?,
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

        let idx = eval.alloc(Value::BuiltinFunc("print", copper_print));
        builtins.insert(String::from("print"), idx);
        let idx = eval.alloc(Value::BuiltinFunc("prints", copper_print_no_newline));
        builtins.insert(String::from("prints"), idx);

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

    // fn insert_scope(&mut self, key: &str, val: usize) {
    // match self.lookup_scope(key) {
    // Some(v) => *v = val,
    // None => {
    // self.scopes.last_mut().unwrap().insert(key.to_string(), val);
    // }
    // }
    // }

    fn insert_scope_local(&mut self, key: &str, val: usize) {
        self.scopes.last_mut().unwrap().insert(key.to_string(), val);
    }

    pub fn deref(&mut self, val: Value) -> Value {
        let idx = match val {
            Value::Reference(u, _) => u,
            _ => return val,
        };
        let mut curidx = idx;
        loop {
            curidx = match self.memory[curidx] {
                Value::Reference(u, _) => u,
                _ => return self.memory[curidx].clone(),
            };
            if curidx == idx {
                panic!("reference loop")
            }
        }
    }

    pub fn deref_idx(&mut self, idx: usize) -> usize {
        let mut curidx = idx;
        loop {
            curidx = match self.memory[curidx] {
                Value::Reference(u, _) => u,
                _ => return curidx,
            };
            if curidx == idx {
                panic!("reference loop");
            }
        }
    }

    pub fn eval(&mut self, tree: &ParseTree) -> Result<Value, String> {
        let mut rv = Value::Null;
        for statement in tree.statements.iter() {
            rv = self.eval_statement(&statement)?;
        }
        Ok(self.deref(rv))
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
            if let Some(rv) = self.eval_if_internal(&ai)? {
                ran_first = Some(rv);
            }
        }

        if ran_first.is_none() && i.else_body.is_some() {
            ran_first = Some(self.eval_expr(i.else_body.as_ref().unwrap())?);
        }

        match ran_first {
            Some(a) => Ok(a),
            None => Ok(Value::Null),
        }
    }

    fn eval_if_internal(&mut self, i: &If) -> Result<Option<Value>, String> {
        let cond = self.eval_expr(&i.condition)?;
        let derefed = self.deref(cond);
        Ok(match derefed {
            Value::Bool(1) => Some(self.eval_expr(&*i.body)?),
            Value::Bool(0) => None,
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
        for statement in b.statements.iter() {
            rv = self.eval_statement(&statement)?;
        }
        Ok(rv)
    }

    fn eval_binop(&mut self, binop: &BinOp) -> Result<Value, String> {
        use crate::lex::TokenType::*;

        let mut lhs = self.eval_expr(&*binop.lhs)?;
        let mut rhs = self.eval_expr(&*binop.rhs)?;
        lhs = self.deref(lhs);
        rhs = self.deref(rhs);

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
        match expr {
            CallExpr(callexpr) => self.eval_call_expr(&callexpr),
            RefExpr(refexpr) => self.eval_ref_expr(&refexpr, false),
            Immediate(immediate) => self.eval_immediate(&immediate),
            BlockExpr(blockexpr) => self.eval_block(blockexpr),
            BinOp(binop) => self.eval_binop(binop),
            AssignExpr(assignexpr) => self.eval_assign(assignexpr),
            While(w) => self.eval_while(w),
            If(i) => self.eval_if(i),
            Function(f) => self.eval_function_def(f),
            Lambda(lambda) => self.eval_lambda_def(lambda),
            PreUnOp(u) => self.eval_unop_pre(u),
            PostUnOp(u) => self.eval_unop_post(u),
        }
    }

    fn eval_unop_pre(&mut self, u: &PreUnOp) -> Result<Value, String> {
        use crate::lex::TokenType;
        let rhs = self.eval_expr(&*u.rhs)?;
        let derefed = self.deref(rhs.clone());
        Ok(match u.op.token_type {
            TokenType::BoolNot => match derefed {
                Value::Bool(b) => Value::Bool(if b == 0 { 1 } else { 0 }),
                Value::Int(i) => Value::Bool(if i == 0 { 1 } else { 0 }),
                Value::Char(c) => Value::Bool(if c == '\0' { 1 } else { 0 }),
                _ => {
                    return Err(format!(
                        "{}: Cannot apply operator `!` to {:?}",
                        u.op.location, rhs
                    ));
                }
            },
            TokenType::BitNot => match derefed {
                Value::Int(i) => Value::Int(!i),
                Value::Char(c) => Value::Char(!(c as u8) as char),
                _ => {
                    return Err(format!(
                        "{}: Cannot apply operator `~` to {:?}",
                        u.op.location, rhs
                    ));
                }
            },
            TokenType::Minus => match derefed {
                Value::Int(i) => Value::Int(-i),
                _ => {
                    return Err(format!(
                        "{}: Cannot apply unary operator `!` to {:?}",
                        u.op.location, rhs
                    ));
                }
            },
            TokenType::Plus => match derefed {
                Value::Int(i) => Value::Int(i),
                Value::Char(c) => Value::Char(c),
                _ => {
                    return Err(format!(
                        "{}: Cannot apply unary operator `+` to {:?}",
                        u.op.location, rhs
                    ));
                }
            },
            TokenType::Inc => match (rhs, derefed) {
                (Value::Reference(u, _), Value::Int(i)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i + 1)
                }
                (Value::Reference(u, _), Value::Char(c)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(((c as u8) + 1) as char)
                }
                (_, d) => {
                    return Err(format!(
                        "{}: cannot apply prefix `++` to {:?}",
                        u.op.location, d
                    ))
                }
            },
            TokenType::Dec => match (rhs, derefed) {
                (Value::Reference(u, _), Value::Int(i)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i - 1)
                }
                (Value::Reference(u, _), Value::Char(c)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(((c as u8) - 1) as char)
                }
                (_, d) => {
                    return Err(format!(
                        "{}: cannot apply prefix `--` to {:?}",
                        u.op.location, d
                    ))
                }
            },
            _ => unreachable!(),
        })
    }

    fn eval_unop_post(&mut self, u: &PostUnOp) -> Result<Value, String> {
        use crate::lex::TokenType;
        let lhs = self.eval_expr(&*u.lhs)?;
        let derefed = self.deref(lhs.clone());
        Ok(match u.op.token_type {
            TokenType::Inc => match (lhs, derefed) {
                (Value::Reference(u, _), Value::Int(i)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i)
                }
                (Value::Reference(u, _), Value::Char(c)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(c)
                }
                (_, d) => {
                    return Err(format!(
                        "{}: cannot apply postfix `++` to {:?}",
                        u.op.location, d
                    ))
                }
            },
            TokenType::Dec => match (lhs, derefed) {
                (Value::Reference(u, _), Value::Int(i)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i)
                }
                (Value::Reference(u, _), Value::Char(c)) => {
                    let idx = self.deref_idx(u);
                    match &mut self.memory[idx] {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(c)
                }
                (_, d) => {
                    return Err(format!(
                        "{}: cannot apply postfix `--` to {:?}",
                        u.op.location, d
                    ))
                }
            },
            _ => unreachable!(),
        })
    }

    fn eval_function_def(&mut self, f: &Function) -> Result<Value, String> {
        if f.name.is_some() {
            let idx = self.alloc(Value::Function(f.clone()));
            self.insert_scope_local(f.name.as_ref().unwrap(), idx);
            Ok(Value::Reference(idx, false))
        } else {
            Ok(Value::Function(f.clone()))
        }
    }

    fn eval_lambda_def(&mut self, f: &Lambda) -> Result<Value, String> {
        Ok(Value::Lambda(f.clone()))
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
        let func = self.deref(func);
        let mut args = Vec::new();
        for arg in expr.args.iter() {
            args.push(self.eval_expr(arg)?);
        }

        self.openscope();
        let rv = Ok(match &func {
            Value::BuiltinFunc(_, f) => f(self, args),
            Value::Function(f) => {
                if args.len() != f.argnames.len() {
                    return Err(format!(
                        "Trying to call function with wrong number of args. wanted {} found {}",
                        args.len(),
                        f.argnames.len()
                    ));
                }
                for it in f.argnames.iter().zip(args.iter()) {
                    let (name, arg) = it;
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(name, idx);
                }
                let rv = self.eval_expr(&*f.body)?;
                rv
            }
            Value::Lambda(l) => {
                if args.len() != l.max_arg {
                    return Err(format!(
                        "Trying to call lambda with wrong number of args. wanted {} found {}",
                        args.len(),
                        l.max_arg
                    ));
                }
                for it in args.iter().enumerate() {
                    let (arg_num, arg) = it;
                    let idx = self.alloc(arg.clone());
                    let label = format!("\\{}", arg_num + 1);
                    self.insert_scope_local(&label, idx);
                }
                let rv = self.eval_expr(&*l.body)?;
                rv
            }
            f => return Err(format!("Tried to call {:#?}", f)),
        });
        self.closescope();
        rv
    }

    fn eval_ref_expr(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<Value, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => {
                if allow_insert {
                    let idx = self.alloc(Value::Null);
                    self.insert_scope_local(s, idx);
                    Ok(Value::Reference(idx, false))
                } else if let Some(idx) = self.lookup_scope(s) {
                    Ok(Value::Reference(*idx, false))
                } else {
                    Err(format!("{}: No such name in scope...", s))
                }
            }
            LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    let idx = self.alloc(Value::Null);
                    self.insert_scope_local(&s, idx);
                    Ok(Value::Reference(idx, false))
                } else if let Some(idx) = self.lookup_scope(&s) {
                    Ok(Value::Reference(*idx, false))
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
