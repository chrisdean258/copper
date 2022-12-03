use crate::{
    builtins::BuiltinFunction,
    code_builder::CodeBuilder,
    operation::{MachineOperation, Operation},
    parser::*,
    typesystem::{Signature, Type, TypeSystem, NULL},
    value::Value,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    mem::{swap, take},
    rc::Rc,
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
    func_scopes: Vec<HashMap<String, Rc<RefCell<Function>>>>,
    class_scopes: Vec<HashMap<String, Rc<RefCell<ClassDecl>>>>,
    num_args: usize,
    arg_types: Option<Vec<Type>>,
    strings: HashMap<String, usize>,
    num_locals: Vec<usize>,
    recursive_calls: Vec<usize>,
    breaks: Vec<usize>,
    continues: Vec<usize>,
    current_null: Option<usize>,
    extra_args: usize,
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
        }
    }

    pub fn compile(
        &mut self,
        name: String,
        p: &ParseTree,
        types: &TypeSystem,
    ) -> (Vec<MachineOperation>, Vec<String>, usize) {
        self.types = Some(types.clone());
        self.code.open_function(name);
        if p.globals.unwrap() > 0 {
            self.code.reserve(p.globals.unwrap() - self.scopes[1].len());
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

    fn statement(&mut self, s: &Statement, pop: bool, save: bool) {
        match s {
            Statement::Expr(e) => {
                self.expr(e);
                if pop {
                    if save {
                        self.code.emit(MachineOperation::Save);
                    }
                    self.code.emit(MachineOperation::Pop);
                }
            }
            Statement::ClassDecl(c) => self.classdecl(c),
            Statement::Import(i) => todo!("{:?}", i),
            Statement::FromImport(f) => todo!("{:?}", f),
            Statement::Continue(c) => self.continue_(c),
            Statement::Break(b) => self.break_(b),
            Statement::Return(r) => self.return_(r),
        }
    }

    fn classdecl(&mut self, c: &Rc<RefCell<ClassDecl>>) {
        self.class_scopes
            .last_mut()
            .unwrap()
            .insert(c.borrow().name.clone(), c.clone());
    }

    fn return_(&mut self, r: &Return) {
        match &r.body {
            Some(e) => self.expr(e.as_ref()),
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

    fn break_(&mut self, _b: &Break) {
        self.breaks.push(self.code.jump_relative(0));
    }

    fn continue_(&mut self, _c: &Continue) {
        self.continues.push(self.code.jump_relative(0));
    }

    fn expr(&mut self, e: &Expression) {
        match &e.etype {
            ExpressionType::While(w) => self.whileexpr(w),
            ExpressionType::For(f) => self.forexpr(f),
            ExpressionType::If(i) => self.ifexpr(i),
            ExpressionType::CallExpr(c) => self.call(c),
            ExpressionType::RefExpr(r) => self.refexpr(r),
            ExpressionType::FuncRefExpr(r) => self.funcrefexpr(r),
            ExpressionType::Immediate(i) => self.immediate(i),
            ExpressionType::BlockExpr(b) => self.block(b),
            ExpressionType::BinOp(b) => self.binop(b),
            ExpressionType::PreUnOp(p) => self.preunop(p),
            ExpressionType::PostUnOp(p) => self.postunop(p),
            ExpressionType::AssignExpr(a) => self.assignment(a),
            ExpressionType::Function(f) => self.function(
                f.clone(),
                self.types
                    .as_ref()
                    .unwrap()
                    .get_signatures_for_func(e.derived_type.unwrap()),
            ),
            ExpressionType::Lambda(l) => self.lambda(
                l.clone(),
                self.types
                    .as_ref()
                    .unwrap()
                    .get_signatures_for_func(e.derived_type.unwrap()),
            ),
            ExpressionType::List(l) => self.list(l),
            ExpressionType::IndexExpr(i) => self.index(i),
            ExpressionType::DottedLookup(d) => self.dotted_lookup(d),
            ExpressionType::LambdaArg(l) => self.lambdaarg(l),
            ExpressionType::Str(s) => self.string(s),
            ExpressionType::RepeatedArg => self.repeated_arg(),
            ExpressionType::Null => self.null(e.derived_type.unwrap()),
            ExpressionType::PossibleMethodCall(m) => todo!("{:?}", m),
        }
    }

    fn binop(&mut self, b: &BinOp) {
        self.expr(b.lhs.as_ref());
        //TODO option types
        if b.op == Operation::BoolOr {
            self.code.dup();
            let from = self.code.jump_relative_if(0);
            self.code.pop();
            self.expr(b.rhs.as_ref());
            let to = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(from, to as isize);
        } else if b.op == Operation::BoolAnd {
            self.code.dup();
            self.code.emit(MachineOperation::BoolNot);
            let from = self.code.jump_relative_if(0);
            self.code.pop();
            self.expr(b.rhs.as_ref());
            let to = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(from, to as isize);
        } else {
            self.expr(b.rhs.as_ref());
            self.code.emit(b.op.as_machine_op());
        }
    }

    fn preunop(&mut self, p: &PreUnOp) {
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
                self.expr(p.rhs.as_ref());
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

    fn postunop(&mut self, p: &PostUnOp) {
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

    fn get_ref(&mut self, e: &Expression) {
        let save = self.need_ref;
        self.need_ref = true;
        self.expr(e);
        self.need_ref = save;
    }

    fn get_no_ref(&mut self, e: &Expression) {
        let save = self.need_ref;
        self.need_ref = false;
        self.expr(e);
        self.need_ref = save;
    }

    fn refexpr(&mut self, r: &RefExpr) {
        let mem: MemoryLocation = if r.is_decl {
            let new_var = self.next_local();
            self.insert_scope(r.name.clone(), new_var);
            new_var
        } else {
            self.lookup_scope_local_or_global(&r.name)
        };
        match mem {
            MemoryLocation::BuiltinFunction(u) if !self.need_ref => {
                self.code.builtin_ref(u);
                return;
            }
            MemoryLocation::BuiltinFunction(u) => panic!("Cannot write to builtin function {}", u),
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

    fn funcrefexpr(&mut self, r: &FuncRefExpr) {
        let mangled_name = self.types.as_ref().unwrap().mangle(&r.name, &r.sig);
        if let Some(val) = self.lookup_scope_local_or_global_can_fail(&mangled_name) {
            match val {
                MemoryLocation::CodeLocation(a) => {
                    self.code.push(Value::Ptr(a));
                    return;
                }
                MemoryLocation::CurrentFunction => {
                    self.recursive_calls.push(self.code.push(Value::Null));
                    return;
                }
                t => unreachable!("Expected CodeLocation found {:?}", t),
            }
        }
        let mut is_init = false;
        let func = match self.scope_lookup(&r.name, &self.func_scopes) {
            Some(f) => f,
            None => match self.scope_lookup(&r.name, &self.class_scopes) {
                None => panic!(
                    "Could not find a func `{}` in func_scope or class_scope",
                    r.name
                ),
                Some(c) => {
                    is_init = true;
                    match &c.borrow().methods.get("__init__").unwrap().etype {
                        ExpressionType::Function(f) => f.clone(),
                        _ => unreachable!(),
                    }
                }
            },
        };

        self.insert_scope(mangled_name.clone(), MemoryLocation::CurrentFunction);

        let save_bp_list = take(&mut self.recursive_calls);
        let addr = self.single_function(&func.borrow(), &r.sig, is_init);
        self.recursive_calls = save_bp_list;
        self.replace_scope(mangled_name, MemoryLocation::CodeLocation(addr));
        self.code.push(Value::Ptr(addr));
    }

    fn assignment(&mut self, a: &AssignExpr) {
        self.get_ref(a.lhs.as_ref());
        if a.op != Operation::Equal {
            self.code.dup();
            self.code.load();
            self.expr(a.rhs.as_ref());
            self.code.emit(a.op.underlying_binop().as_machine_op());
        } else {
            let types = self.types.as_ref().unwrap();
            let save = self.current_null;
            if types.is_option(a.lhs.derived_type.unwrap()) {
                self.current_null = a.lhs.derived_type;
            }
            self.expr(a.rhs.as_ref());
            self.current_null = save;
        }
        self.code.store();
    }

    fn immediate(&mut self, i: &Immediate) {
        self.code.push(i.value);
    }

    fn block(&mut self, b: &BlockExpr) {
        let mut first = true;
        for statement in b.statements.iter() {
            if !first {
                first = false;
                self.code.pop();
            }
            self.statement(statement, false, false);
        }
    }

    fn whileexpr(&mut self, w: &While) {
        let condition = self.code.next_function_relative_addr();
        self.expr(w.condition.as_ref());
        self.code.emit(MachineOperation::BoolNot);
        let skip_body = self.code.jump_relative_if(0);

        let mut breaks = take(&mut self.breaks);
        let mut continues = take(&mut self.continues);
        self.expr(w.body.as_ref());
        swap(&mut self.breaks, &mut breaks);
        swap(&mut self.continues, &mut continues);

        self.code.jump_relative(condition as isize);
        let end = self.code.push(Value::Uninitialized);

        self.code.backpatch_jump_rel(skip_body, end as isize);
        for addr in breaks {
            self.code.backpatch_jump_rel(addr, end as isize);
        }
        for addr in continues {
            self.code.backpatch_jump_rel(addr, condition as isize);
        }
    }

    fn forexpr(&mut self, _f: &For) {}

    fn ifexpr(&mut self, i: &If) {
        let save = self.current_null;
        if let Some(t) = i.makes_option {
            self.current_null = Some(t);
        }
        if !i.and_bodies.is_empty() {
            self.code.push(Value::Uninitialized); // If return value
            self.expr(i.condition.as_ref());
            self.code.dup();
            self.code.emit(MachineOperation::BoolNot);
            let mut next_branch = self.code.jump_relative_if(0);
            self.expr(i.body.as_ref());
            self.code.rotate(3);
            self.code.swap();
            self.code.pop();

            for (body, _) in i.and_bodies.iter() {
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
                self.expr(body.condition.as_ref());
                self.code.dup();
                self.code.rotate(3);
                self.code.emit(MachineOperation::BoolOr);
                self.code.swap();
                self.code.emit(MachineOperation::BoolNot);
                next_branch = self.code.jump_relative_if(0);
                self.expr(body.body.as_ref());
                self.code.rotate(3);
                self.code.swap();
                self.code.pop();
            }
            let loc = self.code.next_function_relative_addr();
            self.code.backpatch_jump_rel(next_branch, loc as isize);
            if let Some(else_body) = &i.else_body {
                next_branch = self.code.jump_relative_if(0);
                self.expr(else_body);
                self.code.swap();
                self.code.pop();
                let loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(next_branch, loc as isize);
            }
        } else {
            self.expr(i.condition.as_ref());
            if let Some(else_body) = &i.else_body {
                let true_branch = self.code.jump_relative_if(0);
                self.expr(else_body);
                let jump_to_end = self.code.jump_relative(0);
                let true_loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(true_branch, true_loc as isize);
                self.expr(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            } else {
                self.code.emit(MachineOperation::BoolNot);
                let jump_to_end = self.code.jump_relative_if(0);
                self.expr(i.body.as_ref());
                let end = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(jump_to_end, end as isize);
            }
        }
        self.current_null = save;
    }

    fn string(&mut self, s: &Str) {
        let str_idx = self.entomb_string(s.string.clone());
        self.code.push(Value::Str(str_idx));
    }

    fn new_list_with_len(&mut self, len: usize) {
        self.code.alloc(3); // list struct
        self.code.dup();
        self.code.alloc(len);
        self.code.push(Value::Int(len as i64));
        self.code.push(Value::Int(len as i64));
        self.code.store_n(3);
    }

    fn list(&mut self, l: &List) {
        self.new_list_with_len(l.exprs.len());
        self.code.dup();
        self.code.load();
        for expr in l.exprs.iter() {
            self.expr(expr);
        }
        self.code.store_n(l.exprs.len());
    }

    fn index(&mut self, i: &IndexExpr) {
        self.get_no_ref(i.obj.as_ref());
        self.code.dup();
        self.code.push(Value::PtrOffset(1));
        self.code.emit(MachineOperation::Plus);
        self.code.load();

        debug_assert!(i.args.len() == 1);
        self.expr(&i.args[0]);
        self.code.dup();
        self.code.rotate(3);
        self.code.emit(MachineOperation::CmpLE);
        self.code.conditional_fail();
        self.code.swap();
        self.code.load();
        self.code.emit(MachineOperation::Plus);
        if !self.need_ref {
            self.code.load();
        }
    }

    fn function(&mut self, f: Rc<RefCell<Function>>, sigs: Vec<Signature>) {
        // names functions are rendered in accordance with FuncRefExprs so that more instances can be rendered when needed
        if let Some(name) = &f.borrow().name {
            self.func_scopes
                .last_mut()
                .unwrap()
                .insert(name.clone(), f.clone());
            self.code.push(Value::Uninitialized);
        } else {
            // I think this is broken but it _might_ work
            for sig in sigs.iter() {
                let addr = self.single_function(&f.borrow(), sig, false);
                self.code.push(Value::Ptr(addr));
            }
        }
    }

    fn single_function(
        &mut self,
        f: &Function,
        sig: &Signature,
        _alloc_before_call: bool,
    ) -> usize {
        debug_assert!(
            sig.repeated_inputs.is_some() || sig.inputs.len() == f.argnames.len(),
            "{:#?}\n-------------------\n{:#?}",
            f,
            sig
        );
        let name = format!(
            "{}{}",
            f.name.clone().unwrap_or_else(|| "<anonymous>".to_string()),
            self.types.as_ref().unwrap().format_signature(sig)
        );
        self.open_scope();
        self.code.open_function(name);
        for argname in f.argnames.iter() {
            let var = self.next_local();
            self.insert_scope(argname.clone(), var);
        }
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if f.locals.unwrap() > f.argnames.len() {
            self.code.reserve(f.locals.unwrap() - f.argnames.len());
        }
        if let Some(size) = f.alloc_before_call {
            self.code.local_ref(0);
            self.code.alloc(size);
            self.code.store_fast();
        }
        self.expr(f.body.as_ref());
        if f.alloc_before_call.is_some() {
            self.code.local_ref(0);
            self.code.load();
        }

        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        self.code.close_function_and_patch(&self.recursive_calls)
    }

    fn lambda(&mut self, l: Rc<RefCell<Lambda>>, sigs: Vec<Signature>) {
        let sig = &sigs[0];
        // for sig in sigs.iter() {
        let addr = self.single_lambda(&l.borrow(), sig);
        self.code.push(Value::Ptr(addr));
        // break;
        // }
    }

    fn single_lambda(&mut self, l: &Lambda, sig: &Signature) -> usize {
        let name = format!(
            "<lambda>{}",
            self.types.as_ref().unwrap().format_signature(sig)
        );
        self.open_scope();
        self.code.open_function(name);
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if l.locals.unwrap() > sig.inputs.len() {
            self.code.reserve(l.locals.unwrap() - sig.inputs.len());
        }
        self.expr(l.body.as_ref());
        self.code.return_();
        self.num_args = old_num_args;
        self.close_scope();
        self.code.close_function()
    }

    fn lambdaarg(&mut self, l: &LambdaArg) {
        self.code.local_ref(l.number as isize);
        self.code.load();
    }

    fn call(&mut self, c: &CallExpr) {
        let save_extra = self.extra_args;
        self.extra_args = 0;
        let mut alloc_before_call_num = 0;
        if c.alloc_before_call.is_some() {
            self.code.push(Value::Uninitialized);
            alloc_before_call_num = 1;
        }
        for arg in c.args.iter() {
            self.expr(arg);
        }
        self.code.push(Value::Count(
            c.args.len() + self.extra_args + alloc_before_call_num,
        ));
        self.extra_args = save_extra;

        let mut arg_types = Some(c.args.iter().map(|a| a.derived_type.unwrap()).collect());
        swap(&mut arg_types, &mut self.arg_types);
        self.expr(c.function.as_ref());
        self.arg_types = arg_types;

        self.code.call();
    }

    fn dotted_lookup(&mut self, d: &DottedLookup) {
        self.get_no_ref(d.lhs.as_ref());
        self.code.push(Value::PtrOffset(d.index.unwrap() as isize));
        self.code.emit(MachineOperation::Plus);
        if !self.need_ref {
            self.code.load();
        }
    }
}
