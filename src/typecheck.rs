#![allow(dead_code)]
use crate::lex;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::{Signature, TypeEntry, TypeEntryType, TypeRef, TypeSystem};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct TypeChecker {
    scopes: Vec<Rc<RefCell<HashMap<String, usize>>>>,
    memory: Vec<TypeRef>,
    system: TypeSystem,
}

fn scope_error(id: &str, token: &lex::Token) -> String {
    format!("{}: No such name in scope `{}`", token.location, id)
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        let mut tc = TypeChecker {
            scopes: Vec::new(),
            memory: Vec::new(),
            system: TypeSystem::new(),
        };
        let mut builtins = HashMap::new();
        let pt = tc.system.builtinfunction("print", typesystem::Null);
        let pts = tc.system.builtinfunction("print", typesystem::Null);
        let rt = tc.system.optional(typesystem::Str);
        let getline = tc.system.builtinfunction("getline", rt);
        let len = tc.system.builtinfunction("len", typesystem::Int);
        builtins.insert(String::from("print"), tc.alloc(pt));
        builtins.insert(String::from("prints"), tc.alloc(pts));
        builtins.insert(String::from("getline"), tc.alloc(getline));
        builtins.insert(String::from("len"), tc.alloc(len));
        tc.scopes.push(Rc::new(RefCell::new(builtins)));
        tc
    }

    fn alloc(&mut self, typ: usize) -> usize {
        self.memory.push(typ);
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

    fn setdefault_scope_local(&mut self, key: &str, default: usize) -> usize {
        if let Some(_) = self.lookup_scope_local(key) {
        } else {
            self.insert_scope_local(key, default.clone());
        }
        self.lookup_scope_local(key).unwrap()
    }

    fn insert_scope_local(&mut self, key: &str, val: usize) {
        let idx = self.alloc(val);
        self.scopes
            .last_mut()
            .unwrap()
            .borrow_mut()
            .insert(key.to_string(), idx);
    }

    pub fn typecheck(&mut self, tree: &mut ParseTree) -> Result<TypeRef, String> {
        let mut rv = typesystem::Uninitialized;
        for statement in tree.statements.iter_mut() {
            rv = self.typecheck_statement(statement)?;
        }
        Ok(rv)
    }

    fn typecheck_statement(&mut self, statement: &mut Statement) -> Result<TypeRef, String> {
        Ok(match statement {
            Statement::Expr(expr) => self.typecheck_expr(expr)?,
            Statement::GlobalDecl(gd) => self.typecheck_global_decl(gd)?,
        })
    }

    fn typecheck_global_decl(&mut self, gd: &GlobalDecl) -> Result<TypeRef, String> {
        use crate::lex::TokenType;
        let id = match &gd.token.token_type {
            TokenType::Identifier(s) => s,
            _ => unreachable!(),
        };

        let idx = match self.lookup_scope(id) {
            Some(i) => i,
            None => return Err(scope_error(id, &gd.token)),
        };

        let rv = self.memory[idx].clone();

        self.insert_scope_local(id, rv.clone());
        Ok(rv)
    }

    // ned to augment this to prevent type switching
    fn typecheck_if(&mut self, i: &mut If) -> Result<TypeRef, String> {
        let mut ran_first = self.typecheck_if_internal(i)?;

        for ai in &mut i.and_bodies {
            if let Some(rv) = self.typecheck_if_internal(ai)? {
                ran_first = Some(rv);
            }
        }

        if ran_first.is_none() && i.else_body.is_some() {
            ran_first = Some(self.typecheck_expr(i.else_body.as_mut().unwrap())?);
        }

        match ran_first {
            Some(a) => Ok(a),
            None => Ok(typesystem::Null),
        }
    }

    fn typecheck_if_internal(&mut self, i: &mut If) -> Result<Option<TypeRef>, String> {
        let cond = self.typecheck_expr(i.condition.as_mut())?;
        Ok(match cond {
            typesystem::Bool => Some(self.typecheck_expr(i.body.as_mut())?),
            _ => {
                match &mut self.system.types[cond].typ {
                    TypeEntryType::GenericType(cons) => {
                        cons.push(typesystem::Constraint::Type(typesystem::Bool));
                        return Ok(Some(self.typecheck_expr(i.body.as_mut())?));
                    }
                    _ => (),
                }
                return Err(format!(
                    "{}: If expressions must have boolean conditions not {:?}",
                    i.location, self.system.types[cond].name
                ));
            }
        })
    }

    fn typecheck_while(&mut self, w: &mut While) -> Result<TypeRef, String> {
        let cond = self.typecheck_expr(w.condition.as_mut())?;
        match cond {
            typesystem::Bool => (),
            _ => {
                return Err(format!(
                    "{}: While loops must have boolean conditions not {:?}",
                    w.location, self.system.types[cond].name
                ))
            }
        }
        self.typecheck_expr(w.body.as_mut())?;
        Ok(typesystem::Null)
    }

    fn typecheck_block(&mut self, b: &mut BlockExpr) -> Result<TypeRef, String> {
        let mut rv = typesystem::Null;
        for statement in b.statements.iter_mut() {
            rv = self.typecheck_statement(statement)?;
        }
        Ok(rv)
    }

    fn typecheck_binop(&mut self, binop: &mut BinOp) -> Result<TypeRef, String> {
        use crate::lex::TokenType;
        let lhs = self.typecheck_expr(binop.lhs.as_mut())?;
        let rhs = self.typecheck_expr(binop.rhs.as_mut())?;

        let op = match binop.op.token_type {
            TokenType::Plus => typesystem::Plus,
            TokenType::Minus => typesystem::Minus,
            TokenType::Times => typesystem::Times,
            TokenType::Div => typesystem::Div,
            TokenType::Mod => typesystem::Mod,
            TokenType::CmpEq => typesystem::CmpEq,
            TokenType::CmpNotEq => typesystem::CmpNotEq,
            TokenType::CmpGE => typesystem::CmpGE,
            TokenType::CmpGT => typesystem::CmpGT,
            TokenType::CmpLE => typesystem::CmpLE,
            TokenType::CmpLT => typesystem::CmpLT,
            TokenType::BitOr => typesystem::BitOr,
            TokenType::BitAnd => typesystem::BitAnd,
            TokenType::BitXor => typesystem::BitXor,
            TokenType::BoolOr => typesystem::BoolOr,
            TokenType::BoolAnd => typesystem::BoolAnd,
            TokenType::BoolXor => typesystem::BoolXor,
            TokenType::BitShiftRight => typesystem::BitShiftRight,
            TokenType::BitShiftLeft => typesystem::BitShiftLeft,
            _ => {
                return Err(format!(
                    "{}: {:?}, unimplemented",
                    binop.op.location, binop.op.token_type
                ))
            }
        };
        match self.system.apply_operation(op, vec![lhs, rhs]) {
            Some(t) => Ok(t),
            None => Err(self.binop_err(&binop.op, &lhs, &rhs)),
        }
    }

    fn typecheck_expr(&mut self, expr: &mut Expression) -> Result<usize, String> {
        use crate::parser::Expression::*;
        match expr {
            CallExpr(callexpr) => self.typecheck_call_expr(callexpr),
            RefExpr(refexpr) => self.typecheck_ref_expr(refexpr, false),
            Immediate(immediate) => self.typecheck_immediate(immediate),
            BlockExpr(blockexpr) => self.typecheck_block(blockexpr),
            BinOp(binop) => self.typecheck_binop(binop),
            AssignExpr(assignexpr) => self.typecheck_assign(assignexpr),
            While(w) => self.typecheck_while(w),
            If(i) => self.typecheck_if(i),
            Function(f) => self.typecheck_function_def(f),
            Lambda(lambda) => self.typecheck_lambda_def(lambda),
            PreUnOp(u) => self.typecheck_unop_pre(u),
            PostUnOp(u) => self.typecheck_unop_post(u),
            List(l) => self.typecheck_list(l),
            IndexExpr(l) => self.typecheck_index_expr(l),
        }
    }

    fn typecheck_list(&mut self, l: &mut List) -> Result<TypeRef, String> {
        let mut typ = typesystem::Uninitialized;
        for expr in l.exprs.iter_mut() {
            let rt = self.typecheck_expr(expr)?;
            if typ == typesystem::Uninitialized || rt == typ {
                typ = rt;
            } else if let TypeEntryType::GenericType(cons) = &mut self.system.types[typ].typ {
                cons.push(typesystem::Constraint::Type(rt));
            } else {
                return Err(format!(
                    "{}: List expression has different type than established. Has `{}` needs `{}`",
                    expr.location(),
                    self.system.types[rt].name,
                    self.system.types[typ].name,
                ));
            }
        }
        Ok(self.system.list(typ))
    }

    fn typecheck_unop_pre(&mut self, u: &mut PreUnOp) -> Result<TypeRef, String> {
        use crate::lex::TokenType;
        let rhs = self.typecheck_expr(u.rhs.as_mut())?;
        let op = match u.op.token_type {
            TokenType::BoolNot => typesystem::BoolNot,
            TokenType::BitNot => typesystem::BitNot,
            TokenType::Minus => typesystem::Minus,
            TokenType::Plus => typesystem::Plus,
            TokenType::Inc => typesystem::Inc,
            TokenType::Dec => typesystem::Dec,
            TokenType::Times => typesystem::Times,
            _ => unreachable!(),
        };
        match self.system.apply_operation(op, vec![rhs]) {
            Some(t) => Ok(t),
            None => Err(self.unop_err(&u.op, &rhs)),
        }
    }

    fn typecheck_unop_post(&mut self, u: &PostUnOp) -> Result<usize, String> {
        use crate::lex::TokenType;
        let lhs = self.typecheck_ref_expr(&*u.lhs, false)?;
        let op = match u.op.token_type {
            TokenType::Inc => typesystem::Inc,
            TokenType::Dec => typesystem::Dec,
            _ => unreachable!(),
        };
        match self.system.apply_operation(op, vec![lhs]) {
            Some(t) => Ok(t),
            None => Err(self.unop_err(&u.op, &lhs)),
        }
    }

    fn typecheck_function_def(&mut self, f: &mut Function) -> Result<TypeRef, String> {
        let mut inputs = Vec::new();
        let name = match &f.name {
            Some(s) => format!("fn {}({})", s, f.argnames.join(", ")),
            None => format!("fn <anonymous>({})", f.argnames.join(", ")),
        };
        let placeholder = self.system.placeholder(&name);
        if f.name.is_some() {
            self.insert_scope_local(f.name.as_ref().unwrap(), placeholder);
        }

        self.openscope();
        for argname in f.argnames.iter() {
            let val = self.system.generic(argname);
            inputs.push(val);
            self.insert_scope_local(argname, val);
        }
        let output = self.typecheck_expr(f.body.as_mut())?;
        self.closescope();
        let sig = Signature { inputs, output };

        self.system.replace(
            placeholder,
            TypeEntry {
                name: name,
                typ: TypeEntryType::CallableType(sig, f.argnames.len()),
                fields: HashMap::new(),
            },
        );

        Ok(placeholder)
    }

    fn typecheck_lambda_def(&mut self, f: &mut Lambda) -> Result<usize, String> {
        let mut inputs = Vec::new();
        let name = format!("lambda({})", f.num_args);
        let nametmp = format!("lambda--({})", f.num_args);
        let placeholder = self.system.placeholder(&nametmp);
        self.openscope();
        for num in 0..f.num_args {
            let argname = format!("\\{}", num);
            let val = self.system.generic(&argname);
            inputs.push(val);
            self.insert_scope_local(&argname, val);
        }
        let output = self.typecheck_expr(f.body.as_mut())?;
        self.closescope();
        let sig = Signature { inputs, output };

        self.system.replace(
            placeholder,
            TypeEntry {
                name: name,
                typ: TypeEntryType::CallableType(sig, f.num_args),
                fields: HashMap::new(),
            },
        );
        Ok(placeholder)
    }

    fn typecheck_assignment(
        &mut self,
        expr: &AssignExpr,
        lhsidx: usize,
        rhs: TypeRef,
    ) -> Result<TypeRef, String> {
        let lhs = self.memory[lhsidx];

        if self.system.is_assignable(&lhs, &rhs) {
            return Ok(lhs);
        }
        if lhs == rhs {
            return Ok(rhs);
        }

        if lhs == typesystem::Uninitialized {
            self.memory[lhsidx] = rhs;
            return Ok(rhs);
        }

        if let TypeEntryType::GenericType(cons) = &mut self.system.types[rhs].typ {
            cons.push(typesystem::Constraint::Type(lhs));
            return Ok(lhs);
        }

        Err(format!(
            "{}: Cannot assign type `{}` = type `{}`",
            expr.op.location, self.system.types[lhs].name, self.system.types[rhs].name
        ))
    }

    fn typecheck_assign(&mut self, expr: &mut AssignExpr) -> Result<usize, String> {
        use crate::lex::TokenType;

        let lhsidx = self.typecheck_assignable(expr.lhs.as_mut(), true)?;
        let lhs = self.memory[lhsidx];
        let rhs = self.typecheck_expr(expr.rhs.as_mut())?;

        let op = match &expr.op.token_type {
            TokenType::Equal => return self.typecheck_assignment(expr, lhsidx, rhs),
            TokenType::OrEq => typesystem::OrEq,
            TokenType::XorEq => typesystem::XorEq,
            TokenType::AndEq => typesystem::AndEq,
            TokenType::PlusEq => typesystem::PlusEq,
            TokenType::MinusEq => typesystem::MinusEq,
            TokenType::TimesEq => typesystem::TimesEq,
            TokenType::DivEq => typesystem::DivEq,
            TokenType::ModEq => typesystem::ModEq,
            TokenType::BitShiftRightEq => typesystem::BitShiftRightEq,
            TokenType::BitShiftLeftEq => typesystem::BitShiftLeftEq,
            _ => unreachable!(),
        };

        match self.system.apply_operation(op, vec![rhs, lhs]) {
            Some(t) => Ok(t),
            None => Err(self.binop_err(&expr.op, &rhs, &lhs)),
        }
    }

    fn typecheck_call_expr(&mut self, expr: &mut CallExpr) -> Result<usize, String> {
        let func = self.typecheck_expr(expr.function.as_mut())?;
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.typecheck_expr(arg)?);
        }

        self.openscope();
        let typ = self.system.types[func].typ.clone();
        let rv = Ok(match &typ {
            TypeEntryType::BuiltinFunction(_, rt) => *rt,
            TypeEntryType::CallableType(sig, arglen) => {
                if args.len() != *arglen {
                    return Err(format!(
                        "{}: Trying to call function with wrong number of args. wanted {} found {}",
                        expr.location,
                        arglen,
                        args.len(),
                    ));
                }
                self.system.check_constraints(&sig.output, &args);
                sig.output
            }
            TypeEntryType::GenericType(_) => self.system.constrain_callable(func, args.len()),
            _ => {
                return Err(format!(
                    "{}: Trying to call `{:#?}`",
                    expr.location, self.system.types[func]
                ))
            }
        });
        self.closescope();
        rv
    }

    fn typecheck_index_expr(&mut self, expr: &mut IndexExpr) -> Result<usize, String> {
        let obj = self.typecheck_expr(expr.obj.as_mut())?;
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.typecheck_expr(arg)?);
        }

        match self.system.types[obj].typ {
            TypeEntryType::GenericType(_) => return Ok(self.system.constrain_idx(obj, args.len())),
            _ => (),
        }

        self.openscope();

        let ltyp = self.system.types[obj].clone();
        let callable = match self.system.types[obj].fields.get("__index__") {
            None => {
                return Err(format!(
                    "{}: cannot index type `{}`",
                    expr.location, ltyp.name
                ))
            }
            Some(t) => *t,
        };

        let typ = self.system.types[callable].clone();
        let rv = Ok(match &typ.typ {
            TypeEntryType::BuiltinFunction(_, rt) => self.alloc(*rt),
            TypeEntryType::CallableType(sig, arglen) => {
                if args.len() != *arglen {
                    return Err(format!(
                        "{}: Trying to index with wrong number of args. wanted {} found {}",
                        expr.location,
                        arglen,
                        args.len(),
                    ));
                }
                self.system.check_constraints(&sig.output, &args);
                sig.output
            }
            TypeEntryType::GenericType(_) => self.system.constrain_callable(obj, args.len()),
            _ => {
                return Err(format!(
                    "{}: Trying to index `{:#?}`",
                    expr.location, self.system.types[obj]
                ))
            }
        });

        self.closescope();
        rv
    }

    fn typecheck_ref_expr(
        &mut self,
        expr: &RefExpr,
        allow_insert: bool,
    ) -> Result<TypeRef, String> {
        let idx = self.typecheck_ref_expr_int(expr, allow_insert)?;
        Ok(self.memory[idx].clone())
    }

    fn typecheck_assignable(
        &mut self,
        expr: &mut Expression,
        allow_insert: bool,
    ) -> Result<usize, String> {
        match expr {
            Expression::RefExpr(re) => self.typecheck_ref_expr_int(re, allow_insert),
            Expression::IndexExpr(i) => self.typecheck_index_expr(i),
            _ => todo!(),
        }
    }

    fn typecheck_ref_expr_int(
        &mut self,
        expr: &RefExpr,
        allow_insert: bool,
    ) -> Result<usize, String> {
        use crate::lex::TokenType;
        match &expr.value.token_type {
            TokenType::Identifier(s) => {
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, typesystem::Uninitialized))
                } else if let Some(typ) = self.lookup_scope(s) {
                    Ok(typ)
                } else {
                    Err(scope_error(&s, &expr.value))
                }
            }
            TokenType::LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, typesystem::Uninitialized))
                } else if let Some(typ) = self.lookup_scope(&s) {
                    Ok(typ)
                } else {
                    Err(scope_error(&s, &expr.value))
                }
            }
            _ => Err("Unexpected...".into()),
        }
    }

    fn typecheck_immediate(&mut self, immediate: &Immediate) -> Result<usize, String> {
        use crate::lex::TokenType;
        Ok(match immediate.value.token_type {
            TokenType::Str(_) => typesystem::Str,
            TokenType::Int(_) => typesystem::Int,
            TokenType::Float(_) => typesystem::Float,
            TokenType::Char(_) => typesystem::Char,
            TokenType::Bool(_) => typesystem::Bool,
            TokenType::Null => typesystem::Null,
            _ => {
                return Err(format!(
                    "{}: Unexpected immediate value {}",
                    immediate.value.location, immediate.value.token_type
                ))
            }
        })
    }

    fn binop_err(&self, operator: &lex::Token, ltyp: &TypeRef, rtyp: &TypeRef) -> String {
        format!(
            "{}: Cannot apply operator `{}` to types `{}` and `{}`",
            operator.location,
            operator.token_type,
            self.system.types[*ltyp].name,
            self.system.types[*rtyp].name,
        )
    }

    fn unop_err(&self, operator: &lex::Token, typ: &TypeRef) -> String {
        format!(
            "{}: Cannot apply operator `{}` to type `{}`",
            operator.location, operator.token_type, self.system.types[*typ].name
        )
    }
}
