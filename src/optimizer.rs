use crate::{memory, operation::MachineOperation, value::Value};
use std::collections::{BTreeMap, BTreeSet};

struct BasicBlock {
    // original_addr: usize,
    new_addr: usize,
    orig_len: usize,
}

pub fn optimize(mut code: Vec<MachineOperation>) -> Vec<MachineOperation> {
    use MachineOperation::*;
    let mut basic_block_begins = BTreeSet::new();
    basic_block_begins.insert(0);
    basic_block_begins.insert(code.len());
    for (i, op) in code.iter_mut().enumerate() {
        match op {
            JumpRel(ref mut o) | JumpRelIf(ref mut o) => {
                let to = i as isize + *o;
                basic_block_begins.insert(to as usize);
                basic_block_begins.insert(i + 1);
                *o = to;
            }
            _ => (),
        }
    }

    let basic_block_begins: Vec<usize> = basic_block_begins.into_iter().collect();
    let mut basic_blocks = BTreeMap::new();
    for i in 0..basic_block_begins.len() - 1 {
        basic_blocks.insert(
            basic_block_begins[i],
            BasicBlock {
                // original_addr: basic_block_begins[i],
                new_addr: 0,
                orig_len: basic_block_begins[i + 1] - basic_block_begins[i],
            },
        );
    }
    let mut new_code = Vec::new();

    for (addr, block) in basic_blocks.iter_mut() {
        block.new_addr = new_code.len();
        new_code.append(&mut optimize_basic_block(
            &code[*addr..*addr + block.orig_len],
        ));
    }

    for (i, inst) in new_code.iter_mut().enumerate() {
        match inst {
            JumpRel(ref mut o) | JumpRelIf(ref mut o) => {
                let to = basic_blocks.get(&(*o as usize)).unwrap().new_addr;
                *o = to as isize - i as isize;
            }
            _ => (),
        }
    }

    new_code
}

#[allow(clippy::single_match)]
fn optimize_basic_block(code: &[MachineOperation]) -> Vec<MachineOperation> {
    use MachineOperation::*;
    let mut i = 0;
    let mut code = code.to_vec();
    while i < code.len() - 1 {
        if i < code.len() - 2 {
            match (code[i], code[i + 1], code[i + 2]) {
                (Push(Value::Count(c)), Push(Value::Ptr(ip)), Call) => {
                    code[i] = Nop;
                    code[i + 1] = Nop;
                    code[i + 2] = if ip < memory::CODE {
                        CallBuiltinSize(ip, c)
                    } else {
                        CallKnownSize(ip, c)
                    }
                }
                _ => (),
            }
        }
        match (code[i], code[i + 1]) {
            (Push(_), Pop) => {
                code[i] = Nop;
                code[i + 1] = Nop;
            }
            (Pop, Push(v)) => {
                code[i] = Nop;
                code[i + 1] = Inplace(v);
            }
            (Store, Pop) => {
                code[i] = Nop;
                code[i + 1] = FastStore;
            }
            (RefFrame(o), Load) => {
                code[i] = Nop;
                code[i + 1] = LoadLocal(o);
            }
            (Reserve(a), Push(Value::Uninitialized)) => {
                code[i] = Nop;
                code[i + 1] = Reserve(a + 1);
            }
            (Push(Value::Ptr(ip)), Call) => {
                code[i] = Nop;
                code[i + 1] = if ip < memory::CODE {
                    CallBuiltin(ip)
                } else {
                    CallKnown(ip)
                }
            }
            (Push(Value::Bool(1)), JumpRelIf(o)) => {
                code[i] = Nop;
                code[i + 1] = JumpRel(o);
            }
            (Push(Value::Bool(0)), JumpRelIf(_)) => {
                code[i] = Nop;
                code[i + 1] = Nop;
            }
            // out.push(JumpRel(o));
            // i += 1;
            // }
            // (Push(Value::Bool(0)), JumpRelIf(_)) => {
            // i += 1;
            // }
            _ => (),
        }
        i += 1;
    }
    code.into_iter().filter(|&x| !matches!(x, Nop)).collect()
}
