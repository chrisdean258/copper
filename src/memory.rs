use crate::allocator::Allocator;
use crate::value::Value;

use std::ops::{Index, IndexMut};

pub const HEAP: usize = 0x10000000;
pub const STACK: usize = 0x6000001;
pub const CODE: usize = 0x100000;
pub const BUILTIN_CODE: usize = 0x10000;

#[derive(Clone, Debug)]
pub struct Memory {
    pub stack: Vec<Value>,
    pub heap: Vec<(usize, Allocator)>,
    pub strings: Vec<String>,
    oob: Value,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            stack: vec![Value::Sentinel],
            heap: vec![
                (HEAP, Allocator::new(1 << 0)),
                (HEAP * 2, Allocator::new(1 << 1)),
                (HEAP * 3, Allocator::new(1 << 2)),
                (HEAP * 4, Allocator::new(1 << 3)),
                (HEAP * 5, Allocator::new(1 << 4)),
                (HEAP * 6, Allocator::new(1 << 5)),
                (HEAP * 7, Allocator::new(1 << 6)),
                (HEAP * 8, Allocator::new(1 << 7)),
                (HEAP * 9, Allocator::new(1 << 8)),
                (HEAP * 10, Allocator::new(1 << 9)),
            ],
            strings: Vec::new(),
            oob: Value::Sentinel,
        }
    }

    pub fn add_strings(&mut self, strings: &mut Vec<String>) {
        self.strings.append(strings);
    }

    pub fn alloc_string(&mut self, string: String) -> usize {
        self.strings.push(string);
        self.strings.len() - 1
    }

    pub fn strcat(&mut self, idx1: usize, idx2: usize) -> usize {
        self.alloc_string(format!("{}{}", self.strings[idx1], self.strings[idx2]))
    }

    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    pub fn last_mut(&mut self) -> &mut Value {
        self.stack.last_mut().unwrap()
    }

    pub fn reserve(&mut self, count: usize) {
        self.stack
            .resize(self.stack.len() + count, Value::Uninitialized)
    }

    pub fn rotate(&mut self, count: usize) {
        debug_assert!(self.stack.len() >= count);
        debug_assert!(count >= 2);
        let val = *self.stack.last().unwrap();
        let idx = self.stack.len() - count;
        for i in (idx..(self.stack.len() - 1)).rev() {
            self.stack[i + 1] = self.stack[i];
        }
        self.stack[idx] = val;
    }

    pub fn truncate_stack(&mut self, ptr: usize) {
        self.stack.truncate(ptr - STACK);
    }

    pub fn stack_top(&self) -> usize {
        STACK + self.stack.len()
    }

    pub fn memcpy(&mut self, dst: usize, src: usize, len: usize) {
        for i in 0..len {
            self[dst + i] = self[src + i];
        }
    }

    pub fn malloc(&mut self, mut size: usize) -> usize {
        if size == 0 {
            return 0;
        }
        size = size.checked_next_power_of_two().unwrap_or(63);
        let idx = (size.trailing_zeros()) as usize;
        if idx >= self.heap.len() {
            panic!("cannot allocate {} values yet", size);
        }
        self.heap[idx].1.alloc() + self.heap[idx].0
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, addr: usize) -> &mut Self::Output {
        self.stack
            .get_mut(addr - STACK)
            .or_else(|| self.heap[addr / HEAP - 1].1.get_mut(addr % HEAP))
            .unwrap_or(&mut self.oob)
    }
}

impl Index<usize> for Memory {
    type Output = Value;
    fn index(&self, addr: usize) -> &Self::Output {
        self.stack
            .get(addr - STACK)
            .or_else(|| self.heap[addr / HEAP - 1].1.get(addr % HEAP))
            .unwrap_or(&self.oob)
    }
}
