use crate::operation::MachineOperation;
use crate::value::Value;
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
        optimize_basic_block(&code[*addr..*addr + block.orig_len], &mut new_code);
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

fn optimize_basic_block(code: &[MachineOperation], out: &mut Vec<MachineOperation>) {
    use MachineOperation::*;
    let mut i = 0;
    while i < code.len() - 1 {
        match (code[i], code[i + 1]) {
            (Push(_), Pop) => {
                i += 1;
            }
            (Pop, Push(v)) => {
                out.push(Inplace(v));
                i += 1;
            }
            (Store, Pop) => {
                out.push(FastStore);
                i += 1;
            }
            // (Push(Value::Bool(1)), JumpRelIf(o)) => {
            // out.push(JumpRel(o));
            // i += 1;
            // }
            // (Push(Value::Bool(0)), JumpRelIf(_)) => {
            // i += 1;
            // }
            (t, _) => out.push(t),
        }
        i += 1;
    }
    if i < code.len() {
        out.push(code[i]);
    }
}
