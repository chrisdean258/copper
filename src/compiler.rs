#![allow(dead_code)]
use crate::{
    builtins::BuiltinFunction,
    code_builder::CodeBuilder,
    memory,
    operation::{MachineOperation, Operation},
    typecheck::*,
    typesystem::{Type, TypeSystem, NULL, STR},
    value::Value,
};
use std::{
    collections::HashMap,
    mem::{replace, swap, take},
};

#[derive(Debug, Clone, Copy)]
enum MemoryLocation {
    BuiltinFunction(usize),
    CodeLocation(usize),
    GlobalVariable(usize),
    LocalVariable(usize),
    CurrentFunction,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    pub code: CodeBuilder,
    need_ref: bool,
    types: Option<TypeSystem>,
    scopes: Vec<HashMap<String, MemoryLocation>>,
    func_scopes: Vec<HashMap<String, TypedFunction>>,
    class_scopes: Vec<HashMap<String, TypedClassDecl>>,
    num_args: usize,
    arg_types: Option<Vec<Type>>,
    strings: HashMap<String, usize>,
    num_locals: Vec<usize>,
    recursive_calls: Vec<usize>,
    breaks: Vec<usize>,
    continues: Vec<usize>,
    current_null: Option<usize>,
    extra_args: usize,
    cur_sig_idx: Option<usize>,
}

impl Compiler {
    pub fn new(types: &mut TypeSystem) -> Self {
        Self {
            code: CodeBuilder::new(),
            need_ref: false,
            scopes: vec![
                {
                    // Builtins
                    let mut b = HashMap::new();
                    for (i, func) in BuiltinFunction::get_table(types).iter().enumerate() {
                        b.insert(func.name.clone(), MemoryLocation::BuiltinFunction(i));
                    }
                    b
                },
                HashMap::new(), //globals
            ],
            func_scopes: vec![HashMap::new(), HashMap::new()],
            class_scopes: vec![HashMap::new(), HashMap::new()],
            types: None,
            num_args: 0,
            arg_types: None,
            strings: HashMap::new(),
            num_locals: vec![0, 0],
            recursive_calls: Vec::new(),
            breaks: Vec::new(),
            continues: Vec::new(),
            current_null: None,
            extra_args: 0,
            cur_sig_idx: None,
        }
    }

    pub fn compile(
        &mut self,
        name: String,
        p: &TypedParseTree,
        types: &TypeSystem,
    ) -> (Vec<MachineOperation>, Vec<String>, usize) {
        self.types = Some(types.clone());
        self.code.open_function(name);
        if p.globals > 0 {
            self.code.reserve(p.globals - self.scopes[1].len());
        }
        for (i, statement) in p.statements.iter().enumerate() {
            self.statement(statement, true, i == p.statements.len() - 1);
        }
        let entry = self.code.close_function();
        self.types = None;
        let mut strings = vec![String::new(); self.strings.len()];
        for (s, p) in self.strings.iter() {
            strings[*p] = s.clone();
        }

        (self.code.code(), strings, entry)
    }

    fn open_scope(&mut self) {
        self.num_locals.push(0);
        self.scopes.push(HashMap::new());
        self.func_scopes.push(HashMap::new());
        self.class_scopes.push(HashMap::new());
    }

    fn close_scope(&mut self) {
        debug_assert!(!self.scopes.is_empty());
        self.num_locals.pop();
        self.scopes.pop();
        self.func_scopes.pop();
        self.class_scopes.pop();
    }

    fn insert_scope(&mut self, name: String, what: MemoryLocation) {
        debug_assert!(self.scopes.len() >= 2);
        let rv = self.scopes.last_mut().unwrap().insert(name, what);
        debug_assert!(rv.is_none());
    }

    fn replace_scope(&mut self, name: String, what: MemoryLocation) {
        debug_assert!(self.scopes.len() >= 2);
        let scope = self.scopes.last_mut().unwrap();
        let rv = scope.insert(name, what);
        debug_assert!(rv.is_some());
    }

    fn next_local(&mut self) -> MemoryLocation {
        // Actually local == global so we will write out a global
        *self.num_locals.last_mut().unwrap() += 1;
        if self.scopes.len() == 2 {
            MemoryLocation::GlobalVariable(*self.num_locals.last().unwrap() - 1)
        } else {
            MemoryLocation::LocalVariable(*self.num_locals.last().unwrap() - 1)
        }
    }

    fn lookup_scope_local_or_global_can_fail(&mut self, name: &str) -> Option<MemoryLocation> {
        debug_assert!(self.scopes.len() >= 2);
        let scope = self.scopes.last().unwrap();
        if let Some(a) = scope.get(name) {
            return Some(*a);
        }
        if let Some(a) = self.scopes[1].get(name) {
            return Some(*a);
        }
        if let Some(a) = self.scopes[0].get(name) {
            return Some(*a);
        }
        None
    }

    fn lookup_scope_local_or_global(&mut self, name: &str) -> MemoryLocation {
        if let Some(a) = self.lookup_scope_local_or_global_can_fail(name) {
            a
        } else {
            unreachable!(
                // self.arg_types.is_some(),
                "Looking up `{}` failed with scopes = {:?}",
                name, self.scopes
            )
        }
    }

    fn entomb_string(&mut self, string: String) -> usize {
        let len = self.strings.len();
        *self.strings.entry(string).or_insert(len)
    }

    fn statement(&mut self, s: &TypedStatement, pop: bool, save: bool) {
        match s {
            TypedStatement::Expr(e) => {
                self.get_value(e);
                if pop {
                    if save {
                        self.code.emit(MachineOperation::Save);
                    }
                    self.code.emit(MachineOperation::Pop);
                }
            }
            TypedStatement::ClassDecl(c) => self.classdecl(c),
            TypedStatement::Import(i) => todo!("{:?}", i),
            TypedStatement::FromImport(f) => todo!("{:?}", f),
            TypedStatement::Continue => self.continue_(),
            TypedStatement::Break => self.break_(),
            TypedStatement::Return(r) => self.return_(r),
        }
    }

    fn classdecl(&mut self, c: &TypedClassDecl) {
        self.class_scopes
            .last_mut()
            .unwrap()
            .insert(c.borrow().classdecl.name.clone(), c.clone());
        for (_name, (f, _l)) in c.borrow().methods.iter() {
            self.function(f);
        }
    }

    fn return_(&mut self, r: &TypedReturn) {
        match &r.body {
            Some(e) => self.get_value(e),
            None => {
                self.code.push(Value::Uninitialized);
            }
        }
        if r.from_function {
            self.code.return_();
        } else {
            self.code.exit_with();
        }
    }

    fn break_(&mut self) {
        self.breaks.push(self.code.jump_relative(0));
    }

    fn continue_(&mut self) {
        self.continues.push(self.code.jump_relative(0));
    }

    fn expr_dont_call_without_handling_needs_ref(&mut self, e: &TypedExpression) {
        match &e.etype {
            TypedExpressionType::While(w) => self.whileexpr(w),
            TypedExpressionType::For(f) => todo!("{f:?}"), //self.forexpr(f),
            TypedExpressionType::If(i) => self.ifexpr(i),
            TypedExpressionType::CallExpr(c) => self.call(c),
            TypedExpressionType::FuncRefExpr(r) => self.funcrefexpr(r),
            TypedExpressionType::BuiltinFuncRefExpr(b) => self.builtinfuncrefexpr(b),
            TypedExpressionType::Immediate(i) => self.immediate(i),
            TypedExpressionType::BlockExpr(b) => self.block(b),
            TypedExpressionType::BinOp(b) => self.binop(b),
            TypedExpressionType::PreUnOp(p) => self.preunop(p),
            TypedExpressionType::PostUnOp(p) => self.postunop(p),
            TypedExpressionType::AssignExpr(a) => self.assignment(a),
            TypedExpressionType::List(l) => self.list(l),
            TypedExpressionType::IndexExpr(i) => self.index(i),
            TypedExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            TypedExpressionType::LambdaArg(l) => self.lambdaarg(l),
            TypedExpressionType::Str(s) => self.string(s),
            TypedExpressionType::RepeatedArg => self.repeated_arg(),
            TypedExpressionType::Null => self.null(e.typ),
            TypedExpressionType::PossibleMethodCall(m) => todo!("{:?}", m),
            TypedExpressionType::VarRefExpr(v) => self.varrefexpr(v),
            TypedExpressionType::Function(f) => self.function(f),
            TypedExpressionType::Lambda(l) => self.lambda(l),
            TypedExpressionType::InitExpr(i) => self.initexpr(i),
            TypedExpressionType::ClassRefExpr(r) => self.classrefexpr(r),
            TypedExpressionType::DirectFuncRef(d, idx) => {
                let addr = self.single_function(d, *idx, false);
                self.code.push(Value::Ptr(addr));
            }
            e => todo!("{:?}", e),
        }
    }

    fn binop(&mut self, b: &TypedBinOp) {
        self.get_value(b.lhs.as_ref());
        //TODO option types
        if b.op == Operation::BoolOr {
            self.code.dup();
            if self.types.as_ref().unwrap().is_option(b.lhs.typ) {
                self.code.push(Value::None(b.lhs.typ));
                self.code.emit(MachineOperation::CmpNotEq);
            }
            let from = self.code.jump_relative_if(0);
            self.code.pop();
            self.get_value(b.rhs.as_ref());
            let to = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(from, to as isize);
        } else if b.op == Operation::BoolAnd {
            self.code.dup();
            if self.types.as_ref().unwrap().is_option(b.lhs.typ) {
                self.code.push(Value::None(b.lhs.typ));
                self.code.emit(MachineOperation::CmpNotEq);
            }
            self.code.emit(MachineOperation::BoolNot);
            let from = self.code.jump_relative_if(0);
            self.code.pop();
            self.get_value(b.rhs.as_ref());
            let to = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(from, to as isize);
        } else if b.op == Operation::BoolXor && self.types.as_ref().unwrap().is_option(b.lhs.typ) {
            self.get_value(b.rhs.as_ref());
            self.code.dup(); // lhs rhs rhs
            self.code.push(Value::None(b.lhs.typ)); // lhs rhs rhs None
            self.code.emit(MachineOperation::CmpEq); // lhs rhs rhs_is_None
            let from1 = self.code.jump_relative_if(0); // go to the end
            self.code.swap(); // Nonnull_rhs lhs
            self.code.dup(); // Nonnull_rhs lhs lhs
            self.code.push(Value::None(b.lhs.typ)); // Nonnull_rhs lhs lhs None
            self.code.emit(MachineOperation::CmpEq); // Nonnull_rhs lhs lhs_is_None
            let from2 = self.code.jump_relative_if(0); // go to the end
            self.code.pop(); // Nonnull_rhs
            self.code.push(Value::None(b.lhs.typ)); // Nonnull_rhs None
            self.code.swap(); // Nonnull_rhs None
            let end = self.code.pop();
            self.code.backpatch_jump_rel(from1, end as isize);
            self.code.backpatch_jump_rel(from2, end as isize);
        } else {
            self.get_value(b.rhs.as_ref());
            self.code.emit(b.op.as_machine_op());
        }
    }

    fn preunop(&mut self, p: &TypedPreUnOp) {
        match p.op {
            Operation::PreInc => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(MachineOperation::Plus);
                self.code.store();
            }
            Operation::PreDec => {
                self.get_ref(p.rhs.as_ref());
                self.code.dup();
                self.code.load();
                self.code.push(Value::Int(1));
                self.code.emit(MachineOperation::Minus);
                self.code.store();
            }
            t if t.is_preunop() => {
                self.get_value(p.rhs.as_ref());
                if p.op != Operation::Deref {
                    self.code.emit(p.op.as_machine_op());
                }
            }
            _ => unreachable!(),
        }
    }

    fn repeated_arg(&mut self) {
        let num_repeated = self.arg_types.as_ref().unwrap().len() - self.num_args;
        // eprintln!( "num_args: {:?}\nnum_repeated: {:?}\narg_types: {:?}\nnum_locals: {:?}", self.num_args, self.num_repeated, self.arg_types, self.num_locals,);
        if num_repeated == 0 {
            return;
        }
        // first_arg
        // eprintln!( "repeated arg! {}", self.code .local_ref(self.num_args as isize - num_repeated as isize));
        self.code.local_ref(self.num_args as isize);
        self.code.load_n(num_repeated);
        self.extra_args = num_repeated - 1;
    }

    fn null(&mut self, typ: Type) {
        if let Some(t) = self.current_null {
            if typ == t || typ == NULL {
                self.code.push(Value::None(t));
            } else {
                let types = self.types.as_ref().unwrap();
                panic!(
                    "Mismatched null types {} and {}",
                    types.typename(t),
                    types.typename(typ)
                );
            }
        } else {
            self.code.push(Value::None(typ));
        }
    }

    fn postunop(&mut self, p: &TypedPostUnOp) {
        self.get_ref(p.lhs.as_ref());
        self.code.dup();
        self.code.load();
        self.code.dup();
        self.code.rotate(3);
        self.code.push(Value::Int(1));
        self.code.emit(match p.op {
            Operation::PostInc => MachineOperation::Plus,
            Operation::PostDec => MachineOperation::Minus,
            _ => unreachable!(),
        });
        self.code.store();
        self.code.pop();
    }

    fn get_ref(&mut self, e: &TypedExpression) {
        let save = self.need_ref;
        self.need_ref = true;
        self.expr_dont_call_without_handling_needs_ref(e);
        self.need_ref = save;
    }

    fn get_value(&mut self, e: &TypedExpression) {
        let save = self.need_ref;
        self.need_ref = false;
        self.expr_dont_call_without_handling_needs_ref(e);
        self.need_ref = save;
    }

    fn varrefexpr(&mut self, r: &TypedVarRefExpr) {
        let mem: MemoryLocation = if r.is_decl {
            let new_var = self.next_local();
            self.insert_scope(r.name.clone(), new_var);
            new_var
        } else {
            self.lookup_scope_local_or_global(&r.name)
        };
        match mem {
            MemoryLocation::BuiltinFunction(u) => {
                panic!("Should have been a funcrefexpr {u:?}\n{r:?}")
            }
            // MemoryLocation::CodeLocation(u) if !self.need_ref => self.code.code_ref(u),
            MemoryLocation::CodeLocation(u) => panic!("Should have been a funcrefexpr {}", u),
            MemoryLocation::GlobalVariable(u) => self.code.global_ref(u),
            MemoryLocation::LocalVariable(u) => self.code.local_ref(u as isize),
            MemoryLocation::CurrentFunction => panic!("UNKNOWN"),
        };
        if !self.need_ref {
            self.code.load();
        }
    }

    fn scope_lookup<T: Clone>(&self, key: &str, table: &[HashMap<String, T>]) -> Option<T> {
        for scope in table.iter().rev() {
            if let Some(f) = scope.get(key) {
                return Some(f.clone());
            }
        }
        None
    }

    fn builtinfuncrefexpr(&mut self, b: &TypedBuiltinFuncRefExpr) {
        let Some(val) = self.lookup_scope_local_or_global_can_fail(&b.name) else {
            unreachable!("Cannot find builtin function `{}`", b.name);
        };
        let MemoryLocation::BuiltinFunction(num) = val else {
            unreachable!("Expected CodeLocation found {val:?}");
        };
        self.code.push(Value::Ptr(num + memory::BUILTIN_CODE));
    }

    fn funcrefexpr(&mut self, r: &TypedFuncRefExpr) {
        let Some(sig_idx) = self.cur_sig_idx else {
            panic!("No signature index");
        };
        let signature = &r.func.borrow().signatures[sig_idx];
        let mangled_name = self.types.as_ref().unwrap().mangle(&r.name, signature);
        if let Some(val) = self.lookup_scope_local_or_global_can_fail(&mangled_name) {
            match val {
                MemoryLocation::CodeLocation(a) => {
                    self.code.push(Value::Ptr(a));
                    return;
                }
                MemoryLocation::CurrentFunction => {
                    self.recursive_calls
                        .push(self.code.push(Value::Uninitialized));
                    return;
                }
                t => unreachable!("Expected CodeLocation found {:?}", t),
            }
        }
        let is_init = false;
        let func = match self.scope_lookup(&r.name, &self.func_scopes) {
            Some(f) => f,
            None => match self.scope_lookup(&r.name, &self.class_scopes) {
                None => panic!(
                    "Could not find a func `{}` in func_scope or class_scope",
                    r.name
                ),
                Some(_c) => {
                    todo!();
                    // is_init = true;
                    // match &c.borrow().methods.get("__init__").unwrap().etype {
                    // ExpressionType::Function(f) => f.clone(),
                    // _ => unreachable!(),
                    // }
                }
            },
        };

        self.insert_scope(mangled_name.clone(), MemoryLocation::CurrentFunction);

        let save_bp_list = take(&mut self.recursive_calls);
        let addr = self.single_function(&func, sig_idx, is_init);
        self.recursive_calls = save_bp_list;
        self.replace_scope(mangled_name, MemoryLocation::CodeLocation(addr));
        self.code.push(Value::Ptr(addr));
    }

    fn classrefexpr(&mut self, _r: &TypedClassRefExpr) {}

    fn get_value_with_null_as(&mut self, e: &TypedExpression, null_type: Type) {
        let save = self.current_null;
        if self.types.as_ref().unwrap().is_option(null_type) {
            self.current_null = Some(null_type);
        }
        self.get_value(e);
        self.current_null = save;
    }

    fn assignment(&mut self, a: &TypedAssignExpr) {
        self.get_ref(a.lhs.as_ref());
        if a.op == Operation::Equal {
            self.get_value_with_null_as(a.rhs.as_ref(), a.lhs.typ);
            self.code.store();
        } else if a.op == Operation::Extract {
            self.get_value_with_null_as(a.rhs.as_ref(), a.lhs.typ);
            self.code.dup();
            self.code.rotate(3);
            self.code.store();
            self.code.pop();
            self.code.push(Value::None(a.rhs.typ));
            self.code.emit(MachineOperation::CmpNotEq);
        } else {
            self.code.dup();
            self.code.load();
            self.get_value(a.rhs.as_ref());
            self.code.emit(a.op.underlying_binop().as_machine_op());
            self.code.store();
        }
    }

    fn immediate(&mut self, i: &TypedImmediate) {
        self.code.push(i.value);
    }

    fn block(&mut self, b: &TypedBlockExpr) {
        let mut first = true;
        for statement in b.statements.iter() {
            if !first {
                first = false;
                self.code.pop();
            }
            self.statement(statement, false, false);
        }
    }

    fn whileexpr(&mut self, w: &TypedWhile) {
        let skip_body = self.code.jump_relative(0);
        let cond_addr = self.code.next_function_relative_addr();
        self.get_value(w.condition.as_ref());
        let cond = self.code.split_off_from(cond_addr);

        let body_addr = self.code.next_function_relative_addr();
        debug_assert!(body_addr == cond_addr);
        let mut breaks = take(&mut self.breaks);
        let mut continues = take(&mut self.continues);
        self.get_value(w.body.as_ref());
        swap(&mut self.breaks, &mut breaks);
        swap(&mut self.continues, &mut continues);

        let new_cond_addr = self.code.next_function_relative_addr();
        self.code.append(cond);
        self.code
            .backpatch_jump_rel(skip_body, new_cond_addr as isize);
        self.code.jump_relative_if(body_addr as isize);

        let end = self.code.push(Value::Uninitialized);

        for addr in breaks {
            self.code.backpatch_jump_rel(addr, end as isize);
        }
        for addr in continues {
            self.code.backpatch_jump_rel(addr, new_cond_addr as isize);
        }
    }

    // fn forexpr(&mut self, _f: &For) {}

    fn ifexpr(&mut self, i: &TypedIf) {
        let save = self.current_null;
        if let Some(t) = i.makes_option {
            self.current_null = Some(t);
        }
        if !i.and_bodies.is_empty() {
            self.code.push(Value::Uninitialized); // If return value
            self.get_value(i.condition.as_ref());
            self.code.dup();
            self.code.emit(MachineOperation::BoolNot);
            let mut next_branch = self.code.jump_relative_if(0);
            self.get_value(i.body.as_ref());
            self.code.rotate(3);
            self.code.swap();
            self.code.pop();

            for ((body, _typ), _location) in i.and_bodies.iter() {
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
                self.get_value(body.condition.as_ref());
                self.code.dup();
                self.code.rotate(3);
                self.code.emit(MachineOperation::BoolOr);
                self.code.swap();
                self.code.emit(MachineOperation::BoolNot);
                next_branch = self.code.jump_relative_if(0);
                self.get_value(body.body.as_ref());
                self.code.rotate(3);
                self.code.swap();
                self.code.pop();
            }
            let loc = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(next_branch, loc as isize);
            if let Some(else_body) = &i.else_body {
                next_branch = self.code.jump_relative_if(0);
                self.get_value(else_body);
                self.code.swap();
                self.code.pop();
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
            }
        } else {
            self.get_value(i.condition.as_ref());
            if let Some(else_body) = &i.else_body {
                let true_branch = self.code.jump_relative_if(0);
                self.get_value(else_body);
                let jump_to_end = self.code.jump_relative(0);
                let true_loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(true_branch, true_loc as isize);
                self.get_value(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            } else {
                self.code.emit(MachineOperation::BoolNot);
                let jump_to_end = self.code.jump_relative_if(0);
                self.get_value(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            }
        }
        self.current_null = save;
    }

    fn string(&mut self, s: &TypedStr) {
        let str_idx = self.entomb_string(s.string.clone());
        self.code.push(Value::Str(str_idx));
    }

    fn new_list_with_len_as_ptr(&mut self, len: usize) {
        self.code.alloc(1 + len); // list struct
        self.code.dup();
        self.code.push(Value::PtrOffset(1));
        self.code.emit(MachineOperation::Plus);
        self.code.swap();
        self.code.push(Value::Int(len as i64)); // length
        self.code.store();
        self.code.pop();
    }

    fn list(&mut self, l: &TypedList) {
        self.new_list_with_len_as_ptr(l.exprs.len());
        self.code.dup();
        for expr in l.exprs.iter() {
            self.get_value(expr);
        }
        self.code.store_n(l.exprs.len());
        self.code.cast(Value::List(0));
    }

    fn index(&mut self, i: &TypedIndexExpr) {
        if i.obj.typ == STR {
            self.get_value(i.obj.as_ref());
            self.get_value(&i.args[0]);
            self.code.emit(MachineOperation::Plus);
        } else {
            self.get_value(i.obj.as_ref()); // list
            debug_assert!(i.args.len() == 1);
            self.get_value(&i.args[0]); // list list_len idx
            self.code.emit(MachineOperation::Plus); // list idx
        }
        if !self.need_ref {
            self.code.load();
        }
    }

    fn function(&mut self, f: &TypedFunction) {
        // names functions are rendered in accordance with FuncRefExprs so that more instances can be rendered when needed
        if let Some(name) = f.borrow().function.name.clone() {
            self.func_scopes.last_mut().unwrap().insert(name, f.clone());
            self.code.push(Value::Uninitialized);
        } else {
            // I think this is broken but it _might_ work
            debug_assert_eq!(f.borrow().signatures.len(), 1);
            // for sig in f.borrow().signatures.iter() {
            let addr = self.single_function(f, 0, false);
            self.code.push(Value::Ptr(addr));
            // }
        }
    }

    fn single_function(
        &mut self,
        f: &TypedFunction,
        sig_idx: usize,
        _alloc_before_call: bool,
    ) -> usize {
        let f = f.borrow();
        let addr = f.code_locations.borrow()[sig_idx];
        if addr != 0 {
            return 0;
        }
        let sig = &f.signatures[sig_idx];
        debug_assert!(
            sig.repeated_inputs.is_some() || sig.inputs.len() == f.function.argnames.len(),
            "{:?} {} ?== {}, {:?}",
            sig.repeated_inputs,
            sig.inputs.len(),
            f.function.argnames.len(),
            sig
        );
        let name = if f.is_method {
            //has to have name because its a signature
            self.types
                .as_ref()
                .unwrap()
                .format_method_sig(sig, f.function.name.clone().unwrap())
        } else {
            format!(
                "{}{}",
                f.function
                    .name
                    .clone()
                    .unwrap_or_else(|| "<anonymous>".to_string()),
                self.types.as_ref().unwrap().format_signature(sig)
            )
        };
        self.open_scope();
        self.code.open_function(name);
        for argname in f.function.argnames.iter() {
            let var = self.next_local();
            self.insert_scope(argname.clone(), var);
        }
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if f.locals > f.function.argnames.len() {
            self.code.reserve(f.locals - f.function.argnames.len());
        }
        if let Some(size) = f.alloc_before_call {
            self.code.local_ref(0);
            self.code.alloc(size);
            self.code.store_fast();
        }
        self.get_value(&f.typed_bodies[sig_idx]);
        if f.alloc_before_call.is_some() {
            self.code.local_ref(0);
            self.code.load();
        }

        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        let addr = self.code.close_function_and_patch(&self.recursive_calls);
        f.code_locations.borrow_mut()[sig_idx] = addr;
        addr
    }

    fn lambda(&mut self, l: &TypedLambda) {
        debug_assert_eq!(l.borrow().signatures.len(), 1);
        let addr = self.single_lambda(l, 0);
        self.code.push(Value::Ptr(addr));
    }

    fn single_lambda(&mut self, l: &TypedLambda, sig_idx: usize) -> usize {
        let l = l.borrow();
        let sig = &l.signatures[sig_idx];
        let name = format!(
            "<lambda>{}",
            self.types.as_ref().unwrap().format_signature(sig)
        );
        self.open_scope();
        self.code.open_function(name);
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if l.locals > sig.inputs.len() {
            self.code.reserve(l.locals - sig.inputs.len());
        }
        self.get_value(&l.typed_bodies[sig_idx]);
        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        self.code.close_function()
    }

    fn lambdaarg(&mut self, l: &TypedLambdaArg) {
        self.code.local_ref(l.number as isize);
        self.code.load();
    }

    fn initexpr(&mut self, i: &TypedInitExpr) {
        self.code.alloc(i.alloc_before_call);
        self.code.dup();
        let Some(tce) = &i.callexpr else {
            return;
        };
        self.call(tce);
        self.code.pop();
    }

    fn call(&mut self, c: &TypedCallExpr) {
        let save_extra = self.extra_args;
        self.extra_args = 0;
        let alloc_before_call_num = 0;
        for arg in c.args.iter() {
            self.get_value(arg);
        }
        self.code.push(Value::Count(
            c.args.len() + self.extra_args + alloc_before_call_num,
        ));
        self.extra_args = save_extra;

        let mut arg_types = Some(c.args.iter().map(|a| a.typ).collect());
        swap(&mut arg_types, &mut self.arg_types);

        if let Some(sig_idx) = c.sig_idx {
            let save_sig_idx = replace(&mut self.cur_sig_idx, Some(sig_idx));
            self.get_value(c.function.as_ref());
            self.cur_sig_idx = save_sig_idx;
            self.arg_types = arg_types;
            self.code.call();
        } else {
            // Builtin function
            self.get_value(c.function.as_ref());
            self.arg_types = arg_types;
            self.code.call();
            // panic!("Cannot calculate call expr without signature: {c:?}");
        }
    }

    fn dotted_lookup(&mut self, d: &TypedDottedLookup) {
        self.get_value(d.lhs.as_ref());
        self.code.push(Value::PtrOffset(d.index as isize));
        self.code.emit(MachineOperation::Plus);
        if !self.need_ref {
            self.code.load();
        }
    }
}
