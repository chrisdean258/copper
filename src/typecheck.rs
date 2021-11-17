#![allow(dead_code)]
use crate::lex;
use crate::parser::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Type {
    BuiltinFunc,
    Str,
    Int,
    Float,
    Char,
    Bool,
    Reference(Box<Type>),
    Function(Function, Vec<Signature>), //Note all signatures must have the same number of args
    Lambda(Lambda, Vec<Signature>),
    Null,
    Nullable(Box<Type>),
    Class,
    Unininitialized,
}

#[derive(Clone, Debug)]
pub struct Signature {
    ins: Vec<Type>,
    out: Box<Type>,
}

impl Signature {
    fn inputs_match_same_len(&self, inputs: &Vec<Type>) -> bool {
        use std::mem::discriminant as disc;
        for tup in self.ins.iter().zip(inputs) {
            let (known, new) = tup;
            if disc(known) != disc(&new) {
                return false;
            }
        }
        true
    }

    fn input_match(&self, inputs: &Vec<Type>) -> bool {
        if self.ins.len() == inputs.len() {
            self.inputs_match_same_len(inputs)
        } else {
            false
        }
    }
}

pub struct TypeChecker {
    scopes: Vec<HashMap<String, usize>>,
    memory: Vec<Type>,
}

fn unop_err(operator: &lex::Token, typ: &Type) -> String {
    format!(
        "{}: Cannot apply operator `{}` to type {:?}",
        operator.location, operator.token_type, typ
    )
}

fn binop_err(operator: &lex::Token, ltyp: &Type, rtyp: &Type) -> String {
    format!(
        "{}: Cannot apply operator `{}` to types {:?} and {:?}",
        operator.location, operator.token_type, ltyp, rtyp
    )
}

fn sig_partial_match(s1: &Vec<Signature>, s2: &Vec<Signature>) -> bool {
    if s1.len() == 0 || s2.len() == 0 {
        return true;
    }
    if s1[0].ins.len() != s2[0].ins.len() {
        return false;
    }
    for sig1 in s1.iter() {
        for sig2 in s2.iter() {
            if sig1.inputs_match_same_len(&sig2.ins) {
                return true;
            }
        }
    }
    false
}

// fn same_type(t1: &Type, t2: &Type) -> bool {
// use std::mem::discriminant as disc;
// use Type::*;
// let t1 = match t1 {
// Reference(t) => t,
// _ => t1,
// };

// let t2 = match t2 {
// Reference(t) => t,
// _ => t2,
// };
// let (t1, t2) = match (t1, t2) {
// (Function(_, sigs1), Lambda(_, sigs2)) => return sig_partial_match(sigs1, sigs2),
// (Lambda(_, sigs1), Function(_, sigs2)) => return sig_partial_match(sigs1, sigs2),
// (Function(_, sigs1), Function(_, sigs2)) => return sig_partial_match(sigs1, sigs2),
// (Lambda(_, sigs1), Lambda(_, sigs2)) => return sig_partial_match(sigs1, sigs2),
// (Nullable(a), Nullable(b)) => a, &*b),
// (Reference(a), b) => (&*a, b),
// (a, Reference(b)) => (a, &*b),
// _ => (),
// };
// disc(t1) == disc(t2)
// }

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let mut tc = TypeChecker {
            scopes: Vec::new(),
            memory: Vec::new(),
        };
        let mut builtins = HashMap::new();
        builtins.insert(String::from("print"), tc.alloc(Type::BuiltinFunc));
        builtins.insert(String::from("prints"), tc.alloc(Type::BuiltinFunc));
        tc.scopes.push(builtins);
        tc
    }

    fn alloc(&mut self, typ: Type) -> usize {
        self.memory.push(typ);
        self.memory.len() - 1
    }

    fn openscope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_scope(&mut self, key: &str) -> Option<usize> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.get(key) {
                return Some(*val);
            }
        }
        None
    }

    fn lookup_scope_local(&self, key: &str) -> Option<usize> {
        self.scopes.last().unwrap().get(key).and_then(|u| Some(*u))
    }

    fn setdefault_scope_local(&mut self, key: &str, default: Type) -> usize {
        if let Some(_) = self.lookup_scope_local(key) {
        } else {
            self.insert_scope_local(key, default.clone());
        }
        self.lookup_scope_local(key).unwrap()
    }

    fn insert_scope_local(&mut self, key: &str, val: Type) {
        let idx = self.alloc(val);
        self.scopes.last_mut().unwrap().insert(key.to_string(), idx);
    }

    pub fn typecheck(&mut self, tree: &ParseTree) -> Result<Type, String> {
        let mut rv = Type::Null;
        for statement in tree.statements.iter() {
            rv = self.typecheck_statement(&statement)?;
        }
        Ok(rv)
    }

    fn typecheck_statement(&mut self, statement: &Statement) -> Result<Type, String> {
        Ok(match statement {
            Statement::Expr(expr) => self.typecheck_expr(expr)?,
        })
    }

    fn typecheck_if(&mut self, i: &If) -> Result<Type, String> {
        let mut ran_first = self.typecheck_if_internal(i)?;

        for ai in &i.and_bodies {
            if let Some(rv) = self.typecheck_if_internal(&ai)? {
                ran_first = Some(rv);
            }
        }

        if ran_first.is_none() && i.else_body.is_some() {
            ran_first = Some(self.typecheck_expr(i.else_body.as_ref().unwrap())?);
        }

        match ran_first {
            Some(a) => Ok(a),
            None => Ok(Type::Null),
        }
    }

    fn typecheck_if_internal(&mut self, i: &If) -> Result<Option<Type>, String> {
        let cond = self.typecheck_expr(&i.condition)?;
        Ok(match cond {
            Type::Bool => Some(self.typecheck_expr(&*i.body)?),
            _ => {
                return Err(format!(
                    "{}: If expressions must have boolean conditions not {:?}",
                    i.location, cond
                ))
            }
        })
    }

    fn typecheck_while(&mut self, w: &While) -> Result<Type, String> {
        let cond = self.typecheck_expr(&*w.condition)?;
        match cond {
            Type::Bool => (),
            _ => {
                return Err(format!(
                    "{}: While loops must have boolean conditions not {:?}",
                    w.location, cond
                ))
            }
        }
        self.typecheck_expr(&w.body)?;
        Ok(Type::Null)
    }

    fn typecheck_block(&mut self, b: &BlockExpr) -> Result<Type, String> {
        let mut rv = Type::Null;
        for statement in b.statements.iter() {
            rv = self.typecheck_statement(&statement)?;
        }
        Ok(rv)
    }

    fn typecheck_binop(&mut self, binop: &BinOp) -> Result<Type, String> {
        use crate::lex::TokenType::*;

        let lhs = self.typecheck_expr(&*binop.lhs)?;
        let rhs = self.typecheck_expr(&*binop.rhs)?;

        Ok(match binop.op.token_type {
            Plus => match (lhs, rhs) {
                (Type::Str, Type::Str) => Type::Str,
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (Type::Int, Type::Float) => Type::Float,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Bool, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Char, Type::Int) => Type::Char,
                (Type::Int, Type::Char) => Type::Char,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            Minus => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (Type::Int, Type::Float) => Type::Float,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Bool, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Char, Type::Int) => Type::Char,
                (Type::Int, Type::Char) => Type::Char,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            Times => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (Type::Int, Type::Float) => Type::Float,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Bool, Type::Int) => Type::Int,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            Div => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (Type::Int, Type::Float) => Type::Float,
                (Type::Bool, Type::Int) => Type::Int,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            Mod => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Int) => Type::Int,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            CmpEq | CmpNotEq | CmpGE | CmpGT | CmpLE | CmpLT => match (lhs, rhs) {
                (Type::Str, Type::Str) => Type::Bool,
                (Type::Int, Type::Int) => Type::Bool,
                (Type::Bool, Type::Bool) => Type::Bool,
                (Type::Char, Type::Char) => Type::Bool,
                (Type::Char, Type::Int) => Type::Bool,
                (Type::Int, Type::Float) => Type::Bool,
                (Type::Float, Type::Int) => Type::Bool,
                (Type::Float, Type::Float) => Type::Bool,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            BitOr | BitAnd | BitXor => match (lhs, rhs) {
                (Type::Bool, Type::Bool) => Type::Bool,
                (Type::Bool, Type::Int) => Type::Int,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Char, Type::Int) => Type::Int,
                (Type::Int, Type::Char) => Type::Int,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            BoolOr | BoolAnd | BoolXor => match (lhs, rhs) {
                (Type::Bool, Type::Bool) => Type::Bool,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            BitShiftRight | BitShiftLeft => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Int) => Type::Int,
                (a, b) => return Err(binop_err(&binop.op, &a, &b)),
            },
            _ => {
                return Err(format!(
                    "{}: {:?}, unimplemented",
                    binop.op.location, binop.op.token_type
                ))
            }
        })
    }

    fn typecheck_expr(&mut self, expr: &Expression) -> Result<Type, String> {
        use crate::parser::Expression::*;
        match expr {
            CallExpr(callexpr) => self.typecheck_call_expr(&callexpr),
            RefExpr(refexpr) => self.typecheck_ref_expr(&refexpr, false),
            Immediate(immediate) => self.typecheck_immediate(&immediate),
            BlockExpr(blockexpr) => self.typecheck_block(blockexpr),
            BinOp(binop) => self.typecheck_binop(binop),
            AssignExpr(assignexpr) => self.typecheck_assign(assignexpr),
            While(w) => self.typecheck_while(w),
            If(i) => self.typecheck_if(i),
            Function(f) => self.typecheck_function_def(f),
            Lambda(lambda) => self.typecheck_lambda_def(lambda),
            PreUnOp(u) => self.typecheck_unop_pre(u),
            PostUnOp(u) => self.typecheck_unop_post(u),
        }
    }

    fn typecheck_unop_pre(&mut self, u: &PreUnOp) -> Result<Type, String> {
        use crate::lex::TokenType;
        let rhsidx = self.typecheck_ref_expr_int(&*u.rhs, false)?;
        let rhs = self.memory[rhsidx].clone();
        Ok(match u.op.token_type {
            TokenType::BoolNot => match rhs {
                Type::Bool => Type::Bool,
                Type::Int => Type::Bool,
                Type::Char => Type::Bool,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            TokenType::BitNot => match rhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            TokenType::Minus => match rhs {
                Type::Int => Type::Int,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            TokenType::Plus => match rhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            TokenType::Inc => match rhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            TokenType::Dec => match rhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &rhs)),
            },
            _ => unreachable!(),
        })
    }

    fn typecheck_unop_post(&mut self, u: &PostUnOp) -> Result<Type, String> {
        use crate::lex::TokenType;
        let lhsidx = self.typecheck_ref_expr_int(&*u.lhs, false)?;
        let lhs = self.memory[lhsidx].clone();
        Ok(match u.op.token_type {
            TokenType::Inc => match lhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &lhs)),
            },
            TokenType::Dec => match lhs {
                Type::Int => Type::Int,
                Type::Char => Type::Char,
                _ => return Err(unop_err(&u.op, &lhs)),
            },
            _ => unreachable!(),
        })
    }

    fn typecheck_function_def(&mut self, f: &Function) -> Result<Type, String> {
        let rv = Type::Function(f.clone(), Vec::new());
        if f.name.is_some() {
            self.insert_scope_local(f.name.as_ref().unwrap(), rv.clone());
        }
        Ok(rv)
    }

    fn typecheck_lambda_def(&mut self, f: &Lambda) -> Result<Type, String> {
        Ok(Type::Lambda(f.clone(), Vec::new()))
    }

    fn typecheck_assignment(
        &mut self,
        expr: &AssignExpr,
        lhsidx: usize,
        rhs: Type,
    ) -> Result<Type, String> {
        use std::mem::discriminant as disc;
        let olhs = self.memory[lhsidx].clone();
        let lhs = match &olhs {
            Type::Unininitialized => {
                self.memory[lhsidx] = rhs.clone();
                return Ok(rhs);
            }
            Type::Null => match rhs {
                Type::Null => Type::Null,
                _ => {
                    self.memory[lhsidx] = Type::Nullable(Box::new(rhs.clone()));
                    return Ok(Type::Nullable(Box::new(rhs.clone())));
                }
            },
            Type::Nullable(t) => match rhs {
                Type::Null => return Ok(olhs),
                _ => *t.clone(),
            },
            _ => olhs.clone(),
        };
        if disc(&lhs) == disc(&rhs) {
            return Ok(olhs);
        }
        Err(format!(
            "{}: Cannot assign type {:?} = type {:?}",
            expr.op.location, olhs, rhs
        ))
    }

    fn typecheck_assign(&mut self, expr: &AssignExpr) -> Result<Type, String> {
        use crate::lex::TokenType::*;

        let rhs = self.typecheck_expr(&*expr.rhs)?;

        let rhs = match rhs {
            Type::Reference(t) => *t.clone(),
            _ => rhs,
        };

        let lhsidx = self.typecheck_ref_expr_int(&*expr.lhs, true)?;
        let lhs = self.memory[lhsidx].clone();

        Ok(match &expr.op.token_type {
            Equal => self.typecheck_assignment(expr, lhsidx, rhs)?,
            OrEq | XorEq | AndEq => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Int, Type::Char) => Type::Int,
                (Type::Char, Type::Int) => Type::Char,
                (a, b) => return Err(binop_err(&expr.op, &a, &b)),
            },
            PlusEq | MinusEq => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Int, Type::Bool) => Type::Bool,
                (Type::Int, Type::Char) => Type::Int,
                (Type::Char, Type::Int) => Type::Char,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Bool) => Type::Float,
                (Type::Float, Type::Char) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (a, b) => return Err(binop_err(&expr.op, &a, &b)),
            },
            TimesEq => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Int, Type::Bool) => Type::Int,
                (Type::Int, Type::Char) => Type::Int,
                (Type::Char, Type::Int) => Type::Char,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Bool) => Type::Float,
                (Type::Float, Type::Char) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (a, b) => return Err(binop_err(&expr.op, &a, &b)),
            },
            DivEq => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Char) => Type::Char,
                (Type::Int, Type::Char) => Type::Int,
                (Type::Char, Type::Int) => Type::Char,
                (Type::Float, Type::Float) => Type::Float,
                (Type::Float, Type::Char) => Type::Float,
                (Type::Float, Type::Int) => Type::Float,
                (a, b) => return Err(binop_err(&expr.op, &a, &b)),
            },
            ModEq | BitShiftRightEq | BitShiftLeftEq => match (lhs, rhs) {
                (Type::Int, Type::Int) => Type::Int,
                (Type::Char, Type::Int) => Type::Char,
                (a, b) => return Err(binop_err(&expr.op, &a, &b)),
            },
            _ => unreachable!(),
        })
    }

    fn typecheck_call_expr(&mut self, expr: &CallExpr) -> Result<Type, String> {
        let mut func = self.typecheck_expr(&expr.function)?;
        let mut args = Vec::new();
        for arg in expr.args.iter() {
            args.push(self.typecheck_expr(arg)?);
        }

        self.openscope();
        let rv = Ok(match &mut func {
            Type::BuiltinFunc => Type::Null,
            Type::Function(f, sigs) => {
                if args.len() != f.argnames.len() {
                    return Err(format!(
                        "{}: Trying to call function with wrong number of args. wanted {} found {}",
                        f.location,
                        args.len(),
                        f.argnames.len()
                    ));
                }
                // we have the types of all the args. We should check against known signatures
                for sig in sigs.iter() {
                    if sig.inputs_match_same_len(&args) {
                        return Ok(*sig.out.clone());
                    }
                }
                for it in f.argnames.iter().zip(args.iter()) {
                    let (name, arg) = it;
                    self.insert_scope_local(name, arg.clone());
                }
                let rv = self.typecheck_expr(&*f.body)?;
                sigs.push(Signature {
                    ins: args,
                    out: Box::new(rv.clone()),
                });
                rv
            }
            Type::Lambda(l, sigs) => {
                if args.len() != l.max_arg + 1 {
                    return Err(format!(
                        "{}: Trying to call lambda with wrong number of args. wanted {} found {}",
                        l.location,
                        args.len(),
                        l.max_arg
                    ));
                }
                for sig in sigs.iter() {
                    if sig.inputs_match_same_len(&args) {
                        return Ok(*sig.out.clone());
                    }
                }
                for it in args.iter().enumerate() {
                    let (arg_num, arg) = it;
                    let label = format!("\\{}", arg_num);
                    self.insert_scope_local(&label, arg.clone());
                }
                let rv = self.typecheck_expr(&*l.body)?;
                sigs.push(Signature {
                    ins: args,
                    out: Box::new(rv.clone()),
                });
                rv
            }
            f => return Err(format!("{}: Tried to call {:#?}", expr.location, f)),
        });
        self.closescope();
        rv
    }

    fn typecheck_ref_expr(&mut self, expr: &RefExpr, allow_insert: bool) -> Result<Type, String> {
        let idx = self.typecheck_ref_expr_int(expr, allow_insert)?;
        Ok(self.memory[idx].clone())
    }

    fn typecheck_ref_expr_int(
        &mut self,
        expr: &RefExpr,
        allow_insert: bool,
    ) -> Result<usize, String> {
        use crate::lex::TokenType::*;
        match &expr.value.token_type {
            Identifier(s) => {
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, Type::Unininitialized))
                } else if let Some(typ) = self.lookup_scope(s) {
                    Ok(typ)
                } else {
                    Err(format!(
                        "{}: No such name in scope `{}`",
                        expr.value.location, s
                    ))
                }
            }
            LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, Type::Unininitialized))
                } else if let Some(typ) = self.lookup_scope(&s) {
                    Ok(typ)
                } else {
                    Err(format!(
                        "{}: No such name in scope `{}`",
                        expr.value.location, s
                    ))
                }
            }
            _ => Err("Unexpected...".into()),
        }
    }

    fn typecheck_immediate(&mut self, immediate: &Immediate) -> Result<Type, String> {
        use crate::lex::TokenType;
        Ok(match immediate.value.token_type {
            TokenType::Str(_) => Type::Str,
            TokenType::Int(_) => Type::Int,
            TokenType::Float(_) => Type::Float,
            TokenType::Char(_) => Type::Char,
            TokenType::Bool(_) => Type::Bool,
            TokenType::Null => Type::Null,
            _ => {
                return Err(format!(
                    "{}: Unexpected immediate value {}",
                    immediate.value.location, immediate.value.token_type
                ))
            }
        })
    }
}
