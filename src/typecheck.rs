#![allow(dead_code)]
use crate::lex;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::{TypeEntryType, TypeRef, TypeSystem};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
struct MemRef {
    pub idx: usize,
}

impl MemRef {
    fn new(idx: usize) -> MemRef {
        MemRef { idx }
    }
}

#[derive(Clone, Debug)]
struct FunctionCall {
    scopes: Vec<Rc<RefCell<HashMap<String, MemRef>>>>,
    arg_names: Vec<String>,
    body: Expression,
}

#[derive(Clone, Debug)]
pub struct TypeChecker {
    scopes: Vec<Rc<RefCell<HashMap<String, MemRef>>>>,
    memory: Vec<TypeRef>,
    system: TypeSystem<FunctionCall>,
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

    fn alloc(&mut self, typ: TypeRef) -> MemRef {
        self.memory.push(typ);
        MemRef::new(self.memory.len() - 1)
    }

    fn openscope(&mut self) {
        self.scopes.push(Rc::new(RefCell::new(HashMap::new())));
    }

    fn closescope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_scope(&mut self, key: &str) -> Option<MemRef> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(val) = scope.borrow().get(key) {
                return Some(*val);
            }
        }
        None
    }

    fn lookup_scope_local(&self, key: &str) -> Option<MemRef> {
        self.scopes
            .last()
            .unwrap()
            .borrow()
            .get(key)
            .and_then(|u| Some(*u))
    }

    fn setdefault_scope_local(&mut self, key: &str, default: TypeRef) -> MemRef {
        if let Some(_) = self.lookup_scope_local(key) {
        } else {
            self.insert_scope_local(key, default.clone());
        }
        self.lookup_scope_local(key).unwrap()
    }

    fn insert_scope_local(&mut self, key: &str, val: TypeRef) {
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

        let idx: MemRef = match self.lookup_scope(id) {
            Some(i) => i,
            None => return Err(scope_error(id, &gd.token)),
        };

        let rv = self.memory[idx.idx].clone();

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
                return Err(format!(
                    "{}: If expressions must have boolean conditions not {:?}",
                    i.location, self.system.types[cond.idx].name
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
                    w.location, self.system.types[cond.idx].name
                ));
            }
        }
        self.typecheck_expr(w.body.as_mut())?;
        Ok(typesystem::Null)
    }

    fn typecheck_for(&mut self, f: &mut For) -> Result<TypeRef, String> {
        let reftypidx: MemRef = self.typecheck_assignable(f.reference.as_mut(), true)?;
        let iterable = self.typecheck_expr(f.items.as_mut())?;
        let mut reftyp = self.memory[reftypidx.idx];

        let itertype = match &mut self.system.types[iterable.idx].typ.clone() {
            TypeEntryType::Iterable(t) => *t,
            TypeEntryType::Callable(0, fc) => {
                let typ = match self.typecheck_function_call(fc, Vec::new()) {
                    Ok(r) => r,
                    Err(s) => Err(format!(
                        "{}\n{}: Originating at this index expression",
                        s, f.location
                    ))?,
                };
                match self.system.types[typ.idx].typ {
                    TypeEntryType::Container(t) => t,
                    _ => typ,
                }
            }

            TypeEntryType::BuiltinFunction(_, t) => match &mut self.system.types[t.idx].typ {
                TypeEntryType::Container(a) => *a,
                _ => *t,
            },

            _ => {
                return Err(format!(
                    "{}: Must be a container type or function of 0 arguments to iterate over",
                    f.items.location()
                ))
            }
        };

        if reftyp == typesystem::Uninitialized {
            self.memory[reftypidx.idx] = itertype;
            reftyp = self.memory[reftypidx.idx];
        }

        if !self.system.is_assignable(&reftyp, &itertype) {
            return Err(format!(
                "{}: Cannot assign `{}` = `{}`",
                f.reference.location(),
                self.system.types[reftyp.idx].name,
                self.system.types[itertype.idx].name,
            ));
        }

        self.typecheck_expr(f.body.as_mut())?;
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

    fn typecheck_expr(&mut self, expr: &mut Expression) -> Result<TypeRef, String> {
        use crate::parser::Expression::*;
        match expr {
            CallExpr(callexpr) => self.typecheck_call_expr(callexpr),
            RefExpr(refexpr) => self.typecheck_ref_expr(refexpr, false),
            Immediate(immediate) => self.typecheck_immediate(immediate),
            BlockExpr(blockexpr) => self.typecheck_block(blockexpr),
            BinOp(binop) => self.typecheck_binop(binop),
            AssignExpr(assignexpr) => self.typecheck_assign(assignexpr),
            While(w) => self.typecheck_while(w),
            For(f) => self.typecheck_for(f),
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
            } else {
                return Err(format!(
                    "{}: List expression has different type than established. Has `{}` needs `{}`",
                    expr.location(),
                    self.system.types[rt.idx].name,
                    self.system.types[typ.idx].name,
                ));
            }
        }
        if typ == typesystem::Uninitialized {
            Ok(typesystem::EmptyList)
        } else {
            Ok(self.system.list(typ))
        }
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

    fn typecheck_unop_post(&mut self, u: &PostUnOp) -> Result<TypeRef, String> {
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

    fn typecheck_function_call(
        &mut self,
        f: &mut FunctionCall,
        args: Vec<TypeRef>,
    ) -> Result<TypeRef, String> {
        let save = self.scopes.clone();
        self.scopes = f.scopes.clone();
        self.openscope();
        for it in args.iter().zip(f.arg_names.iter()) {
            let (arg, name) = it;
            self.insert_scope_local(name, *arg);
        }
        let rv = self.typecheck_expr(&mut f.body)?;
        self.scopes = save;
        Ok(rv)
    }

    fn typecheck_function_def(&mut self, f: &mut Function) -> Result<TypeRef, String> {
        let name = match &f.name {
            Some(s) => format!("fn {}({})", s, f.argnames.join(", ")),
            None => format!("fn <anonymous>({})", f.argnames.join(", ")),
        };

        let rv = self.system.function(
            &name,
            f.argnames.len(),
            FunctionCall {
                scopes: self.scopes.clone(),
                arg_names: f.argnames.clone(),
                body: f.body.as_ref().clone(),
            },
        );

        if f.name.is_some() {
            self.insert_scope_local(f.name.as_ref().unwrap(), rv);
        }

        Ok(rv)
    }

    fn typecheck_lambda_def(&mut self, f: &mut Lambda) -> Result<TypeRef, String> {
        let name = format!("lambda({})", f.num_args);
        let arg_names: Vec<String> = (0..f.num_args).map(|a| format!("\\{}", a)).collect();
        Ok(self.system.function(
            &name,
            f.num_args,
            FunctionCall {
                scopes: self.scopes.clone(),
                arg_names,
                body: f.body.as_ref().clone(),
            },
        ))
    }

    fn typecheck_assignment(
        &mut self,
        expr: &AssignExpr,
        lhsidx: MemRef,
        rhs: TypeRef,
    ) -> Result<TypeRef, String> {
        let lhs = self.memory[lhsidx.idx];

        if self.system.is_assignable(&lhs, &rhs) {
            return Ok(lhs);
        }
        if lhs == rhs {
            return Ok(rhs);
        }

        if lhs == typesystem::Uninitialized {
            self.memory[lhsidx.idx] = rhs;
            return Ok(rhs);
        }

        Err(format!(
            "{}: Cannot assign type `{}` = type `{}`",
            expr.op.location, self.system.types[lhs.idx].name, self.system.types[rhs.idx].name
        ))
    }

    fn typecheck_assign(&mut self, expr: &mut AssignExpr) -> Result<TypeRef, String> {
        use crate::lex::TokenType;

        let lhsidx: MemRef = self.typecheck_assignable(expr.lhs.as_mut(), true)?;
        let lhs = self.memory[lhsidx.idx];
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

        let rv = match self.system.apply_operation(op, vec![rhs, lhs]) {
            Some(t) => t,
            None => return Err(self.binop_err(&expr.op, &lhs, &rhs)),
        };
        self.memory[lhsidx.idx] = rv;
        Ok(rv)
    }

    fn typecheck_call_expr(&mut self, expr: &mut CallExpr) -> Result<TypeRef, String> {
        let func = self.typecheck_expr(expr.function.as_mut())?;
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.typecheck_expr(arg)?);
        }

        self.openscope();
        let mut typ = self.system.types[func.idx].typ.clone();
        let rv = Ok(match &mut typ {
            TypeEntryType::BuiltinFunction(_, rt) => *rt,
            TypeEntryType::Callable(_, fc) => match self.typecheck_function_call(fc, args) {
                Ok(r) => r,
                Err(s) => Err(format!(
                    "{}\n{}: Originating at this index expression",
                    s, expr.location
                ))?,
            },
            _ => {
                return Err(format!(
                    "{}: Trying to call `{:#?}`",
                    expr.location, self.system.types[func.idx]
                ))
            }
        });
        self.closescope();
        rv
    }

    fn typecheck_index_expr(&mut self, expr: &mut IndexExpr) -> Result<TypeRef, String> {
        let idx: MemRef = self.typecheck_index_expr_int(expr)?;
        Ok(self.memory[idx.idx].clone())
    }

    fn typecheck_index_expr_int(&mut self, expr: &mut IndexExpr) -> Result<MemRef, String> {
        let obj = self.typecheck_expr(expr.obj.as_mut())?;
        let mut args = Vec::new();
        for arg in expr.args.iter_mut() {
            args.push(self.typecheck_expr(arg)?);
        }

        self.openscope();

        let ltyp = self.system.types[obj.idx].clone();
        let callable = match self.system.types[obj.idx].fields.get("__index__") {
            None => {
                return Err(format!(
                    "{}: cannot index type `{:#?}`",
                    expr.location, ltyp
                ))
            }
            Some(t) => *t,
        };

        let mut typ = self.system.types[callable.idx].clone();
        let rv = Ok(match &mut typ.typ {
            TypeEntryType::BuiltinFunction(_, rt) => self.alloc(*rt),
            TypeEntryType::Callable(_, fc) => {
                let rv = match self.typecheck_function_call(fc, args) {
                    Ok(r) => r,
                    Err(s) => Err(format!(
                        "{}\n{}: Originating at this index expression",
                        s, expr.location
                    ))?,
                };
                self.alloc(rv)
            }
            _ => {
                return Err(format!(
                    "{}: Trying to index `{:#?}`",
                    expr.location, self.system.types[obj.idx]
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
        let idx: MemRef = self.typecheck_ref_expr_int(expr, allow_insert)?;
        Ok(self.memory[idx.idx].clone())
    }

    fn typecheck_assignable(
        &mut self,
        expr: &mut Expression,
        allow_insert: bool,
    ) -> Result<MemRef, String> {
        match expr {
            Expression::RefExpr(re) => self.typecheck_ref_expr_int(re, allow_insert),
            Expression::IndexExpr(i) => self.typecheck_index_expr_int(i),
            _ => todo!(),
        }
    }

    fn typecheck_ref_expr_int(
        &mut self,
        expr: &RefExpr,
        allow_insert: bool,
    ) -> Result<MemRef, String> {
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

    fn typecheck_immediate(&mut self, immediate: &Immediate) -> Result<TypeRef, String> {
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
            self.system.types[ltyp.idx].name,
            self.system.types[rtyp.idx].name,
        )
    }

    fn unop_err(&self, operator: &lex::Token, typ: &TypeRef) -> String {
        format!(
            "{}: Cannot apply operator `{}` to type `{}`",
            operator.location, operator.token_type, self.system.types[typ.idx].name
        )
    }
}
