use crate::builtins::*;
use crate::parser::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::mem::swap;
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Clone)]
pub enum Value {
    BuiltinFunc(&'static str, fn(&mut Evaluator, Vec<Value>) -> Value),
    Reference(usize, bool),
    Str(Rc<String>),
    List(Vec<Value>),
    Int(i64),
    Float(f64),
    Char(char),
    Bool(u8),
    Function(Function, Vec<Rc<RefCell<HashMap<String, usize>>>>),
    Lambda(Lambda, Vec<Rc<RefCell<HashMap<String, usize>>>>),
    Uninitialized,
    Null,
}

#[derive(Clone, Debug)]
pub struct Evaluator {
    scopes: Vec<Rc<RefCell<HashMap<String, usize>>>>,
    pub memory: Vec<Value>,
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Value::*;
        f.write_str("Value::")?;
        match self {
            BuiltinFunc(name, _) => f.write_fmt(format_args!("BuiltinFunc({})", name)),
            Str(s) => f.write_fmt(format_args!("Str({:?})", s)),
            List(l) => {
                f.write_str("List([")?;
                for expr in l.iter() {
                    f.write_fmt(format_args!("{:?}, ", expr))?;
                }
                f.write_str("])")
            }
            Int(i) => f.write_fmt(format_args!("Int({:?})", i)),
            Float(d) => f.write_fmt(format_args!("Float({:?})", d)),
            Char(c) => f.write_fmt(format_args!("Char({:?})", c)),
            Reference(u, b) => f.write_fmt(format_args!(
                "Reference({:x}{})",
                u,
                if *b { "" } else { ", nonnullable" }
            )),
            Bool(b) => f.write_fmt(format_args!("Bool({})", b)),
            Function(func, _) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", "),))
            }
            Lambda(lambda, _) => f.write_fmt(format_args!("lambda(args={})", lambda.num_args)),
            Null => f.write_str("Null"),
            Uninitialized => f.write_str("Uninitialized"),
        }
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
            List(l) => {
                f.write_str("[")?;
                for expr in l.iter() {
                    f.write_fmt(format_args!("{}, ", expr))?;
                }
                f.write_str("]")?;
            }
            Function(func, _) => {
                f.write_fmt(format_args!("function({})", func.argnames.join(", ")))?
            }
            Lambda(lambda, _) => f.write_fmt(format_args!("lambda(args={})", lambda.num_args))?,
            Null => f.write_str("null")?,
            Uninitialized => f.write_str("Uninitialized")?,
        };
        Ok(())
    }
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

        eval.scopes.push(Rc::new(RefCell::new(builtins)));

        eval
    }

    fn alloc(&mut self, val: Value) -> usize {
        self.memory.push(val);
        self.memory.len() - 1
    }

    fn openscope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_scope(&mut self, key: &str) -> Option<usize> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.borrow().get(key) {
                return Some(*val);
            }
        }
        None
    }

    fn lookup_scope_local(&self, key: &str) -> Option<usize> {
        self.scopes
            .last()
            .unwrap()
            .borrow()
            .get(key)
            .and_then(|u| Some(*u))
    }

    fn setdefault_scope_local(&mut self, key: &str, default: Value) -> usize {
        if let Some(_) = self.lookup_scope_local(key) {
        } else {
            let idx = self.alloc(default.clone());
            self.insert_scope_local(key, idx);
        }
        self.lookup_scope_local(key).unwrap()
    }

    fn insert_scope_local(&mut self, key: &str, idx: usize) {
        self.scopes
            .last_mut()
            .unwrap()
            .borrow_mut()
            .insert(key.to_string(), idx);
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

    pub fn eval(&mut self, tree: &mut ParseTree) -> Result<Value, String> {
        let mut rv = Value::Null;
        for statement in tree.statements.iter_mut() {
            rv = self.eval_statement(statement)?;
        }
        Ok(self.deref(rv))
    }

    fn eval_statement(&mut self, statement: &mut Statement) -> Result<Value, String> {
        use crate::parser::Statement::*;
        Ok(match statement {
            Expr(expr) => self.eval_expr(expr)?,
            GlobalDecl(gd) => self.eval_global_decl(gd)?,
        })
    }

    fn eval_global_decl(&mut self, gd: &GlobalDecl) -> Result<Value, String> {
        use crate::lex::TokenType;
        let id = match &gd.token.token_type {
            TokenType::Identifier(s) => s,
            _ => unreachable!(),
        };

        let idx = match self.lookup_scope(id) {
            Some(i) => i,
            None => unreachable!(),
        };
        let rv = Value::Reference(idx, false);
        self.insert_scope_local(id, idx);
        Ok(rv)
    }

    fn eval_if(&mut self, i: &mut If) -> Result<Value, String> {
        let mut ran_first = self.eval_if_internal(i)?;

        for ai in i.and_bodies.iter_mut() {
            if let Some(rv) = self.eval_if_internal(ai)? {
                ran_first = Some(rv);
            }
        }

        if ran_first.is_none() && i.else_body.is_some() {
            ran_first = Some(self.eval_expr(i.else_body.as_mut().unwrap())?);
        }

        match ran_first {
            Some(a) => Ok(a),
            None => Ok(Value::Null),
        }
    }

    fn eval_if_internal(&mut self, i: &mut If) -> Result<Option<Value>, String> {
        let cond = self.eval_expr(&mut i.condition)?;
        let derefed = self.deref(cond);
        Ok(match derefed {
            Value::Bool(1) => Some(self.eval_expr(i.body.as_mut())?),
            Value::Bool(0) => None,
            _ => {
                return Err(format!(
                    "If expressions must have boolean conditions not {:?}",
                    derefed
                ))
            }
        })
    }

    fn eval_while(&mut self, w: &mut While) -> Result<Value, String> {
        loop {
            let cond = self.eval_expr(w.condition.as_mut())?;
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
            self.eval_expr(&mut w.body)?;
        }
        Ok(Value::Null)
    }

    fn eval_block(&mut self, b: &mut BlockExpr) -> Result<Value, String> {
        let mut rv = Value::Null;
        for statement in b.statements.iter_mut() {
            rv = self.eval_statement(statement)?;
        }
        Ok(rv)
    }

    fn eval_binop(&mut self, binop: &mut BinOp) -> Result<Value, String> {
        use crate::lex::TokenType::*;

        let mut lhs = self.eval_expr(binop.lhs.as_mut())?;
        let mut rhs = self.eval_expr(binop.rhs.as_mut())?;
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
                (Value::Null, Value::Null) => Value::Bool(1),
                (Value::Null, _) => Value::Bool(0),
                (_, Value::Null) => Value::Bool(0),
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
                (Value::Null, Value::Null) => Value::Bool(0),
                (Value::Null, _) => Value::Bool(1),
                (_, Value::Null) => Value::Bool(1),
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

    fn eval_expr(&mut self, expr: &mut Expression) -> Result<Value, String> {
        use crate::parser::Expression::*;
        match expr {
            CallExpr(callexpr) => self.eval_call_expr(callexpr),
            RefExpr(refexpr) => self.eval_ref_expr(refexpr, false),
            Immediate(immediate) => self.eval_immediate(immediate),
            BlockExpr(blockexpr) => self.eval_block(blockexpr),
            BinOp(binop) => self.eval_binop(binop),
            AssignExpr(assignexpr) => self.eval_assign(assignexpr),
            While(w) => self.eval_while(w),
            If(i) => self.eval_if(i),
            Function(f) => self.eval_function_def(f),
            Lambda(lambda) => self.eval_lambda_def(lambda),
            PreUnOp(u) => self.eval_unop_pre(u),
            PostUnOp(u) => self.eval_unop_post(u),
            List(l) => self.eval_list(l),
        }
    }

    fn eval_list(&mut self, l: &mut List) -> Result<Value, String> {
        let mut rv: Vec<Value> = Vec::new();
        for expr in l.exprs.iter_mut() {
            rv.push(self.eval_expr(expr)?);
        }
        Ok(Value::List(rv))
    }

    fn eval_unop_pre(&mut self, u: &PreUnOp) -> Result<Value, String> {
        use crate::lex::TokenType;
        let rhsidx = self.eval_ref_expr_int(&*u.rhs, false)?;
        let derefedidx = self.deref_idx(rhsidx);
        let rhs = self.memory[derefedidx].clone();

        Ok(match u.op.token_type {
            TokenType::BoolNot => match rhs {
                Value::Bool(b) => Value::Bool(if b == 0 { 1 } else { 0 }),
                Value::Int(i) => Value::Bool(if i == 0 { 1 } else { 0 }),
                Value::Char(c) => Value::Bool(if c == '\0' { 1 } else { 0 }),
                _ => unreachable!(),
            },
            TokenType::BitNot => match rhs {
                Value::Int(i) => Value::Int(!i),
                Value::Char(c) => Value::Char(!(c as u8) as char),
                _ => unreachable!(),
            },
            TokenType::Minus => match rhs {
                Value::Int(i) => Value::Int(-i),
                _ => unreachable!(),
            },
            TokenType::Plus => match rhs {
                Value::Int(i) => Value::Int(i),
                Value::Char(c) => Value::Char(c),
                _ => unreachable!(),
            },
            TokenType::Times => match rhs {
                Value::Null => return Err(format!("{}: Null value encountered", u.op.location)),
                t => t.clone(),
            },
            TokenType::Inc => match rhs {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx] {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i + 1)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx] {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(((c as u8) + 1) as char)
                }
                _ => unreachable!(),
            },
            TokenType::Dec => match rhs {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx] {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i - 1)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx] {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(((c as u8) - 1) as char)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
    }

    fn eval_unop_post(&mut self, u: &PostUnOp) -> Result<Value, String> {
        use crate::lex::TokenType;

        let lhsidx = self.eval_ref_expr_int(&*u.lhs, false)?;
        let derefedidx = self.deref_idx(lhsidx);
        let lhs = self.memory[derefedidx].clone();
        Ok(match u.op.token_type {
            TokenType::Inc => match lhs {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx] {
                        Value::Int(ii) => *ii += 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx] {
                        Value::Char(cc) => *cc = ((*cc as u8) + 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(c)
                }
                _ => unreachable!(),
            },
            TokenType::Dec => match lhs {
                Value::Int(i) => {
                    match &mut self.memory[derefedidx] {
                        Value::Int(ii) => *ii -= 1,
                        _ => unreachable!(),
                    }
                    Value::Int(i)
                }
                Value::Char(c) => {
                    match &mut self.memory[derefedidx] {
                        Value::Char(cc) => *cc = ((*cc as u8) - 1) as char,
                        _ => unreachable!(),
                    }
                    Value::Char(c)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        })
    }

    fn eval_function_def(&mut self, f: &Function) -> Result<Value, String> {
        let idx = self.alloc(Value::Function(f.clone(), self.scopes.clone()));
        let rv = Value::Reference(idx, false);
        if f.name.is_some() {
            let idx = self.alloc(rv.clone());
            self.insert_scope_local(f.name.as_ref().unwrap(), idx);
        }
        Ok(rv)
    }

    fn eval_lambda_def(&mut self, f: &Lambda) -> Result<Value, String> {
        Ok(Value::Lambda(f.clone(), self.scopes.clone()))
    }

    fn eval_assign(&mut self, expr: &mut AssignExpr) -> Result<Value, String> {
        use crate::lex::TokenType;
        let rhs = self.eval_expr(expr.rhs.as_mut())?;
        let rhs = match rhs {
            Value::Reference(u, _) => self.memory[u].clone(),
            _ => rhs,
        };

        let lhsidx = self.eval_ref_expr_int(expr.lhs.as_ref(), expr.allow_decl)?;

        match &expr.op.token_type {
            TokenType::Equal => {
                self.memory[lhsidx] = rhs;
            }
            TokenType::AndEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a &= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 & *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a &= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a &= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 & *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::OrEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a |= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 | *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a |= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a |= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 | *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::XorEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a ^= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 ^ *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a ^= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a ^= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 ^ *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::PlusEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a += *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a += *b as i64,
                (Value::Int(a), Value::Char(b)) => *a += *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 + *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a += *b,
                (Value::Float(a), Value::Bool(b)) => *a += *b as f64,
                (Value::Float(a), Value::Char(b)) => *a += (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a + *b as f64,
                _ => unreachable!(),
            },
            TokenType::MinusEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a -= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a -= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a -= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 - *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a -= *b,
                (Value::Float(a), Value::Bool(b)) => *a -= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a -= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a - *b as f64,
                _ => unreachable!(),
            },
            TokenType::TimesEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a *= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Int(a), Value::Bool(b)) => *a *= *b as i64,
                (Value::Int(a), Value::Char(b)) => *a *= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 * *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a *= *b,
                (Value::Float(a), Value::Bool(b)) => *a *= *b as f64,
                (Value::Float(a), Value::Char(b)) => *a *= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::DivEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a /= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a /= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 / *b as u8) as char,
                (Value::Float(a), Value::Float(b)) => *a /= *b,
                (Value::Float(a), Value::Char(b)) => *a /= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::ModEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a %= *b,
                (Value::Char(a), Value::Char(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Int(a), Value::Char(b)) => *a %= *b as i64,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 % *b as u8) as char,
                (Value::Float(a), Value::Char(b)) => *a %= (*b as u8) as f64,
                (Value::Float(a), Value::Int(b)) => *a = *a * *b as f64,
                _ => unreachable!(),
            },
            TokenType::BitShiftRightEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a >>= *b,
                (Value::Char(a), Value::Int(b)) => *a = (*a as u8 >> *b as u8) as char,
                _ => unreachable!(),
            },
            TokenType::BitShiftLeftEq => match (&mut self.memory[lhsidx], &rhs) {
                (Value::Int(a), Value::Int(b)) => *a <<= *b,
                (Value::Char(a), Value::Int(b)) => *a = ((*a as u8) << *b as u8) as char,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
        Ok(self.memory[lhsidx].clone())
    }

    fn eval_call_expr(&mut self, expr: &mut CallExpr) -> Result<Value, String> {
        let func = self.eval_expr(expr.function.as_mut())?;
        let mut func = self.deref(func);
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.eval_expr(arg)?);
        }

        self.openscope();
        let rv = Ok(match &mut func {
            Value::BuiltinFunc(_, f) => f(self, args),
            Value::Function(f, scopes) => {
                assert_eq!(args.len(), f.argnames.len());
                let mut tmp = scopes.clone();
                swap(&mut self.scopes, &mut tmp);
                self.openscope();
                for it in f.argnames.iter().zip(args.iter()) {
                    let (name, arg) = it;
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(name, idx);
                }
                let rv = self.eval_expr(f.body.as_mut())?;
                swap(&mut self.scopes, &mut tmp);
                rv
            }
            Value::Lambda(l, scopes) => {
                assert_eq!(args.len(), l.num_args);
                let mut tmp = scopes.clone();
                swap(&mut self.scopes, &mut tmp);
                self.openscope();
                for it in args.iter().enumerate() {
                    let (arg_num, arg) = it;
                    let label = format!("\\{}", arg_num);
                    let idx = self.alloc(arg.clone());
                    self.insert_scope_local(&label, idx);
                }
                let rv = self.eval_expr(l.body.as_mut())?;
                self.closescope();
                swap(&mut self.scopes, &mut tmp);
                rv
            }
            _ => unreachable!(),
        });
        self.closescope();
        rv
    }

    fn eval_ref_expr(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<Value, String> {
        let idx = self.eval_ref_expr_int(expr, allow_insert)?;
        Ok(self.memory[idx].clone())
    }

    fn eval_ref_expr_int(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => {
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, Value::Uninitialized))
                } else if let Some(idx) = self.lookup_scope(s) {
                    Ok(idx)
                } else {
                    Err(format!("{}: No such name in scope...", s))
                }
            }
            LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, Value::Uninitialized))
                } else if let Some(idx) = self.lookup_scope(&s) {
                    Ok(idx)
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
