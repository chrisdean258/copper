use crate::{
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

#[derive(Debug, Clone)]
pub struct Compiler {
    pub code: CodeBuilder,
    need_ref: bool,
    types: Option<TypeSystem>,
    num_args: usize,
    arg_types: Option<Vec<Type>>,
    strings: HashMap<String, usize>,
    recursive_calls: Vec<usize>,
    breaks: Vec<usize>,
    continues: Vec<usize>,
    current_null: Option<usize>,
    extra_args: usize,
    cur_sig_idx: Option<usize>,
}

impl Compiler {
    pub fn new(_types: &mut TypeSystem) -> Self {
        Self {
            code: CodeBuilder::new(),
            need_ref: false,
            types: None,
            num_args: 0,
            arg_types: None,
            strings: HashMap::new(),
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
            self.code.reserve(p.globals);
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
            TypedStatement::ClassDecl(_) => (),
            TypedStatement::Import(i) => todo!("{:?}", i),
            TypedStatement::FromImport(f) => todo!("{:?}", f),
        }
    }

    fn return_(&mut self, r: &TypedReturn) {
        match r.body.as_ref() {
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
            TypedExpressionType::ForList(f) => self.forlist(f),
            TypedExpressionType::ForClass(f) => self.forclass(f),
            TypedExpressionType::If(i) => self.ifexpr(i),
            TypedExpressionType::CallExpr(c) => self.call(c),
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
            TypedExpressionType::VarRefExpr(v) => self.varrefexpr(v),
            TypedExpressionType::Function(f) => self.function(f),
            TypedExpressionType::Lambda(l) => self.lambda(l),
            TypedExpressionType::InitExpr(i) => self.initexpr(i),
            TypedExpressionType::ClassRefExpr(r) => self.classrefexpr(r),
            TypedExpressionType::DirectFuncRef(d) => self.directfuncref(d),
            TypedExpressionType::Continue => self.continue_(),
            TypedExpressionType::Break => self.break_(),
            TypedExpressionType::Return(r) => self.return_(r),
            TypedExpressionType::Unreachable => unreachable!(),
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
            } else if b.lhs.typ == NULL {
                // TODO: Figure out what this is actually supposed to be value wise
                self.code.push(Value::None(NULL));
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
        if num_repeated == 0 {
            return;
        }
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
        match r.place {
            VarLoc::Global(u) => self.code.global_ref(u),
            VarLoc::Local(u) => self.code.local_ref(u as isize),
        };
        if !self.need_ref {
            self.code.load();
        }
    }

    fn builtinfuncrefexpr(&mut self, b: &TypedBuiltinFuncRefExpr) {
        self.code.push(Value::Ptr(b.idx + memory::BUILTIN_CODE));
    }

    fn directfuncref(&mut self, fr: &TypedFunction) {
        let Some(sig_idx) = self.cur_sig_idx else {
            panic!("No signature index");
        };
        let save_bp_list = take(&mut self.recursive_calls);
        let addr = self.single_function(fr, sig_idx);
        self.recursive_calls = save_bp_list;
        if addr == 1 {
            self.recursive_calls
                .push(self.code.push(Value::Uninitialized));
        } else {
            self.code.push(Value::Ptr(addr));
        }
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

    fn forclass(&mut self, f: &TypedForClass) {
        self.get_value(&f.items);
        self.code.push(Value::Count(1));
        let arg_types = replace(&mut self.arg_types, Some(vec![f.items.typ]));
        let save_sig_idx = replace(&mut self.cur_sig_idx, Some(f.iter_func_sig_idx));
        let addr = self.single_function(&f.iter_func, f.iter_func_sig_idx);
        self.code.push(Value::Ptr(addr));
        self.cur_sig_idx = save_sig_idx;
        self.arg_types = arg_types;
        self.code.call(); // iter

        let top = self.code.dup(); // iter iter
        self.get_ref(&f.reference); // iter iter ref
        self.code.swap(); // iter ref iter
        self.code.push(Value::Count(1)); // iter ref iter 1
        let arg_types = replace(&mut self.arg_types, Some(vec![f.mid_typ]));
        let save_sig_idx = replace(&mut self.cur_sig_idx, Some(f.next_func_sig_idx));
        let addr = self.single_function(&f.next_func, f.next_func_sig_idx);
        self.code.push(Value::Ptr(addr));
        self.cur_sig_idx = save_sig_idx;
        self.arg_types = arg_types;
        self.code.call(); // iter ref item
        let mut goto_end = None;
        if self.types.as_ref().unwrap().is_option(f.none_type) {
            self.code.dup(); // iter ref item item
            self.code.push(Value::None(f.none_type)); // iter ref item item None
            self.code.emit(MachineOperation::CmpEq); // iter ref item is_null
            goto_end = Some(self.code.jump_relative_if(0));
        }

        self.code.store(); // iter item
        self.code.pop(); // iter
        let mut breaks = take(&mut self.breaks);
        let mut continues = take(&mut self.continues);
        self.get_value(&f.body); // iter junk
        swap(&mut self.breaks, &mut breaks);
        swap(&mut self.continues, &mut continues);
        self.code.pop(); // iter
        self.code.jump_relative(top as isize);

        let end = self.code.next_function_relative_addr();

        if let Some(ge) = goto_end {
            self.code.backpatch_jump_rel(ge, end as isize);
        }
        for addr in breaks {
            self.code.backpatch_jump_rel(addr, end as isize);
        }
        for addr in continues {
            self.code.backpatch_jump_rel(addr, top as isize);
        }
    }

    fn forlist(&mut self, f: &TypedForList) {
        self.get_value(&f.items);
        self.code.cast(Value::Ptr(0)); // L
        self.code.dup(); // L L
        self.code.dup(); // L L L
        self.code.push(Value::PtrOffset(-1)); // L L L -1
        self.code.emit(MachineOperation::Plus); // L L L-1
        self.code.load(); // L L len
        self.code.emit(MachineOperation::Plus); // L E
        let start = self.code.next_function_relative_addr(); // P E
        self.code.dup(); // P E E
        self.code.rotate(3); // E P E
        self.code.swap(); // E E P
        self.code.dup(); // E E P P
        self.code.push(Value::PtrOffset(1)); // E E P P 1
        self.code.emit(MachineOperation::Plus); // E E P N
        self.code.rotate(4); // N E E P
        self.code.dup(); // N E E P P
        self.code.rotate(3); // N E P E P
        self.code.emit(MachineOperation::CmpEq); // N E P at_end
        let skip = self.code.jump_relative_if(0);
        self.code.load(); // N E V
        self.get_ref(&f.reference); // N E V R
        self.code.swap(); // N E V R
        self.code.store(); // N E V
        self.code.pop(); // N E
        self.get_value(&f.body);
        self.code.pop();
        self.code.jump_relative(start as isize);
        let end = self.code.pop(); // E N
        self.code.backpatch_jump_rel(skip, end as isize);
        self.code.pop(); // E
        self.code.pop(); //
    }

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
                let true_branch = self.code.jump_relative_if(0);
                self.code.push(Value::Uninitialized);
                let jump_to_end = self.code.jump_relative(0);
                let true_loc = self.code.next_function_relative_addr();
                self.code.backpatch_jump_rel(true_branch, true_loc as isize);
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
        if f.borrow().function.name.clone().is_some() {
            self.code.push(Value::Uninitialized);
        } else {
            // I think this is broken but it _might_ work
            // debug_assert_eq!(f.borrow().signatures.len(), 1);
            // for sig in f.borrow().signatures.iter() {
            // TODO
            let idx = self.cur_sig_idx.unwrap_or(0);
            let addr = self.single_function(f, idx);
            self.code.push(Value::Ptr(addr));
            // }
        }
    }

    fn single_function(&mut self, f: &TypedFunction, sig_idx: usize) -> usize {
        let f = f.borrow();
        let addr = f.code_locations.borrow()[sig_idx];
        if addr != 0 {
            return addr;
        }
        f.code_locations.borrow_mut()[sig_idx] = 1;

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
        self.code.open_function(name);
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
        let addr = self.code.close_function_and_patch(&self.recursive_calls);
        f.code_locations.borrow_mut()[sig_idx] = addr;
        addr
    }

    fn lambda(&mut self, l: &TypedLambda) {
        debug_assert_eq!(l.borrow().signatures.len(), 1, "{:?}", &l.borrow());
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
        self.code.open_function(name);
        let old_num_args = self.num_args;
        self.num_args = sig.inputs.len();
        if l.locals > sig.inputs.len() {
            self.code.reserve(l.locals - sig.inputs.len());
        }
        self.get_value(&l.typed_bodies[sig_idx]);
        self.code.return_();
        self.num_args = old_num_args;
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
        for arg in c.args.iter() {
            self.get_value(arg);
        }
        self.code.push(Value::Count(c.args.len() + self.extra_args));
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
        if d.index != 0 {
            self.code.push(Value::PtrOffset(d.index as isize));
            self.code.emit(MachineOperation::Plus);
        }
        if !self.need_ref {
            self.code.load();
        }
    }
}
