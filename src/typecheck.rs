#![allow(dead_code)]
use crate::lex;
use crate::location::Location;
use crate::parser::*;
use crate::typesystem;
use crate::typesystem::{TypeEntryType, TypeRef, TypeSystem};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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
    preset_args: Vec<TypeRef>,
    default_args: Vec<Expression>,
    body: Expression,
    location: Location,
    being_evaluated: Rc<RefCell<HashMap<Vec<TypeRef>, TypeRef>>>,
}

#[derive(Clone, Debug)]
pub struct TypeChecker {
    scopes: Vec<Rc<RefCell<HashMap<String, MemRef>>>>,
    memory: Vec<TypeRef>,
    system: TypeSystem<FunctionCall, ClassDecl>,
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
        let mut rv = typesystem::Undefined;
        for statement in tree.statements.iter_mut() {
            rv = self.typecheck_statement(statement)?;
        }
        Ok(rv)
    }

    fn typecheck_statement(&mut self, statement: &mut Statement) -> Result<TypeRef, String> {
        match statement {
            Statement::Expr(expr) => self.typecheck_expr(expr),
            Statement::GlobalDecl(gd) => self.typecheck_global_decl(gd),
            Statement::ClassDecl(cd) => self.typecheck_class_decl(cd),
            Statement::Import(i) => self.typecheck_import(i),
            Statement::FromImport(f) => self.typecheck_from_import(f),

            Statement::Break(b) => self.typecheck_break(b),
            Statement::Continue(c) => self.typecheck_continue(c),
            Statement::Return(r) => self.typecheck_return(r),
        }
    }

    fn check_consistency(&mut self, type1: TypeRef, type2: TypeRef) -> Option<TypeRef> {
        if self.system.convertable_to(&type1, &type2) {
            Some(type2)
        } else if self.system.convertable_to(&type2, &type1) {
            Some(type2)
        } else {
            None
        }
    }

    fn typecheck_return(&mut self, r: &mut Return) -> Result<TypeRef, String> {
        eprintln!(
            "{}: Warning: typechecking for return statements is not fully implemented",
            r.location
        );
        if let Some(expr) = &mut r.body {
            self.typecheck_expr(expr.as_mut())
        } else {
            Ok(typesystem::Undefined)
        }
    }

    fn typecheck_break(&mut self, b: &mut Break) -> Result<TypeRef, String> {
        eprintln!(
            "{}: Warning: typechecking for break statements is not fully implemented",
            b.location
        );
        if let Some(expr) = &mut b.body {
            self.typecheck_expr(expr.as_mut())
        } else {
            Ok(typesystem::Undefined)
        }
    }

    fn typecheck_continue(&mut self, _: &mut Continue) -> Result<TypeRef, String> {
        Ok(typesystem::Undefined)
    }

    fn typecheck_import(&mut self, _i: &mut Import) -> Result<TypeRef, String> {
        todo!()
    }

    fn typecheck_from_import(&mut self, _f: &mut FromImport) -> Result<TypeRef, String> {
        todo!()
    }

    fn typecheck_class_decl(&mut self, cd: &mut ClassDecl) -> Result<TypeRef, String> {
        let mut methods = HashMap::new();
        for (key, func) in cd.methods.iter_mut() {
            methods.insert(key.clone(), self.typecheck_function_def(func)?);
        }

        let rv = self
            .system
            .class(&cd.name, cd.fields.clone(), methods, cd.clone());

        self.insert_scope_local(&cd.name, rv);
        Ok(rv)
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

    fn typecheck_if(&mut self, i: &mut If) -> Result<TypeRef, String> {
        let mut types = HashSet::new();
        types.insert(self.typecheck_if_internal(i)?);

        for ai in &mut i.and_bodies {
            types.insert(self.typecheck_if_internal(ai)?);
        }

        if i.else_body.is_some() {
            types.insert(self.typecheck_expr(i.else_body.as_mut().unwrap())?);
        } else {
            types.insert(typesystem::Undefined);
        }

        Ok(self.system.union(types))
    }

    fn typecheck_if_internal(&mut self, i: &mut If) -> Result<TypeRef, String> {
        let cond = self.typecheck_expr(i.condition.as_mut())?;
        match cond {
            typesystem::Bool => self.typecheck_expr(i.body.as_mut()),
            _ => Err(format!(
                "{}: If expressions must have boolean conditions not {:?}",
                i.location, self.system.types[cond.idx].name
            )),
        }
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
            TypeEntryType::List(t) => *t,
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
                    "{}: Must be a container type or function of 0 arguments to iterate over. Found `{}`",
                    f.items.location(), self.system.types[iterable.idx].name
                ))
            }
        };

        if reftyp == typesystem::Undefined {
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
        Ok(typesystem::Undefined)
    }

    fn typecheck_block(&mut self, b: &mut BlockExpr) -> Result<TypeRef, String> {
        let mut rv = typesystem::Undefined;
        for statement in b.statements.iter_mut() {
            rv = self.typecheck_statement(statement)?
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
            DottedLookup(d) => self.typecheck_dotted_lookup(d),
        }
    }

    fn typecheck_list(&mut self, l: &mut List) -> Result<TypeRef, String> {
        let mut typ = typesystem::Undefined;
        for expr in l.exprs.iter_mut() {
            let rt = self.typecheck_expr(expr)?;
            if typ == typesystem::Undefined || rt == typ {
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
        if typ == typesystem::Undefined {
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
        mut args: Vec<TypeRef>,
    ) -> Result<TypeRef, String> {
        if let Some(typ) = f.being_evaluated.borrow().get(&args) {
            return Ok(*typ);
        }

        let orig_args = args.clone();

        let name = format!("<current function return type>");
        let typref = self.system.placeholder(&name);
        f.being_evaluated.borrow_mut().insert(args.clone(), typref);

        let save = self.scopes.clone();
        self.scopes = f.scopes.clone();
        self.openscope();
        let mut allargs = f.preset_args.clone();
        allargs.append(&mut args);
        // trying to apply default args

        if f.arg_names.len() > allargs.len() {
            let num_needed = f.arg_names.len() - allargs.len();
            if num_needed > f.default_args.len() {
                return Err(format!("{}: Not enough args", f.location));
            }
            let start = f.default_args.len() - num_needed;
            for i in start..f.default_args.len() {
                allargs.push(self.typecheck_expr(&mut f.default_args[i])?)
            }
        }

        if allargs.len() != f.arg_names.len() {
            let names: Vec<String> = allargs
                .iter()
                .map(|a| self.system.types[a.idx].name.clone())
                .collect();
            return Err(format!(
                "{}: Trying to call function with wrong number of arguments. Needed {}  found {} {}",
                f.location,
                f.arg_names.len(),
                allargs.len(),
                names.join(", ")
            ));
        }
        for (arg, name) in allargs.iter().zip(f.arg_names.iter()) {
            self.insert_scope_local(name, *arg);
        }

        f.being_evaluated
            .borrow_mut()
            .insert(allargs.clone(), typref);
        let rv = self.typecheck_expr(&mut f.body)?;
        self.scopes = save;
        f.being_evaluated.borrow_mut().insert(orig_args, rv);
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
                location: f.location.clone(),
                scopes: self.scopes.clone(),
                arg_names: f.argnames.clone(),
                preset_args: Vec::new(),
                default_args: f.default_args.clone(),
                body: f.body.as_ref().clone(),
                being_evaluated: Rc::new(RefCell::new(HashMap::new())),
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
                location: f.location.clone(),
                scopes: self.scopes.clone(),
                arg_names,
                preset_args: Vec::new(),
                default_args: Vec::new(),
                body: f.body.as_ref().clone(),
                being_evaluated: Rc::new(RefCell::new(HashMap::new())),
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

        if lhs == typesystem::Undefined {
            self.memory[lhsidx.idx] = rhs;
            return Ok(rhs);
        }

        if lhs == typesystem::EmptyList {
            match self.system.types[rhs.idx].typ {
                TypeEntryType::List(_) => {
                    self.memory[lhsidx.idx] = rhs;
                    return Ok(rhs);
                }
                _ => (),
            }
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

        let rv = self
            .system
            .apply_operation(op, vec![rhs, lhs])
            .ok_or(self.binop_err(&expr.op, &lhs, &rhs))?;
        self.typecheck_assignment(expr, lhsidx, rv)
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
            TypeEntryType::Class(class) => {
                let init = self.system.types[func.idx]
                    .fields
                    .get("__init__")
                    .ok_or(format!(
                        "{}: `<class {}>` lacks a __init__ method\n{}: Originating here",
                        class.location, class.name, expr.location
                    ))?;
                let mut init = match self.system.types[init.idx].typ.clone() {
                    TypeEntryType::Callable(_, fc) => fc,
                    _ => {
                        return Err(format!(
                            "{}: `<class {}>.__init__` must be a function\n{}: Originating here",
                            class.location, class.name, expr.location
                        ))
                    }
                };
                let inst = self.system.class_instance(func);
                args.insert(0, inst);
                match self.typecheck_function_call(&mut init, args) {
                    Ok(r) => r,
                    Err(s) => Err(format!(
                        "{}\n{}: Originating at this index expression",
                        s, expr.location
                    ))?,
                };
                for (field, typ) in self.system.types[inst.idx].fields.clone() {
                    if typ == typesystem::Undefined {
                        return Err(format!(
                            "{}: `__init__` method must initialize all fields. `.{}` was left uninitialized\n{}: Originating here",
                            init.location, field, expr.location
                        ));
                    } else if let TypeEntryType::UninitializedLocation(u) =
                        self.system.types[typ.idx].typ
                    {
                        let tr = self.memory[u];
                        self.system.types[typ.idx] = self.system.types[tr.idx].clone();
                    } else if let TypeEntryType::Callable(_, fc) =
                        &mut self.system.types[typ.idx].typ
                    {
                        if field != "__init__" {
                            fc.preset_args.push(inst);
                        }
                    }
                }
                inst
            }
            _ => {
                return Err(format!(
                    "{}: Trying to call `{}`",
                    expr.location, self.system.types[func.idx].name
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
                    "{}: cannot index type `{}`",
                    expr.location, ltyp.name
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
            Expression::DottedLookup(d) => self.typecheck_dotted_lookup_int(d),
            _ => unreachable!("{}", expr),
        }
    }

    fn typecheck_dotted_lookup(&mut self, d: &mut DottedLookup) -> Result<TypeRef, String> {
        let idx: MemRef = self.typecheck_dotted_lookup_int(d)?;
        Ok(self.memory[idx.idx].clone())
    }

    fn typecheck_dotted_lookup_int(&mut self, d: &mut DottedLookup) -> Result<MemRef, String> {
        let objidx = self.typecheck_expr(d.lhs.as_mut())?;
        let obj = &self.system.types[objidx.idx];
        let typ = *obj.fields.get(&d.rhs).ok_or(format!(
            "{}: `.{}` no such field on <class `{}`>",
            d.location, d.rhs, obj.name,
        ))?;

        let rv = self.alloc(typ);
        if typ == typesystem::Undefined {
            // we must be in a __init__ otherwise all fields should be initialized
            let name = format!("<Undefined `.{}`>", d.rhs);
            *self.system.types[objidx.idx]
                .fields
                .get_mut(&d.rhs)
                .unwrap() = self
                .system
                .entry(&name, TypeEntryType::UninitializedLocation(rv.idx));
        }

        Ok(rv)
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
                    Ok(self.setdefault_scope_local(&s, typesystem::Undefined))
                } else if let Some(typ) = self.lookup_scope(s) {
                    Ok(typ)
                } else {
                    Err(scope_error(&s, &expr.value))
                }
            }
            TokenType::LambdaArg(u) => {
                let s = format!("\\{}", u);
                if allow_insert {
                    Ok(self.setdefault_scope_local(&s, typesystem::Undefined))
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
