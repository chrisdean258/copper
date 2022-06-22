use crate::allocator::Allocator;
use crate::value::Value;

use std::ops::{Index, IndexMut};

pub const HEAP: usize = 0x10000000;
pub const STACK: usize = 0x6000000;
pub const CODE: usize = 0x100000;
pub const BUILTIN_CODE: usize = 0x10000;

#[derive(Clone, Debug)]
pub struct Memory {
    pub stack: Vec<Value>,
    pub heap: Vec<(usize, Allocator)>,
    pub strings: Vec<String>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
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
        }
    }

    // #[inline(always)]
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

    #[inline(always)]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val);
    }

    #[inline(always)]
    pub fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    #[inline(always)]
    pub fn last_mut(&mut self) -> &mut Value {
        self.stack.last_mut().unwrap()
    }

    #[inline(always)]
    pub fn reserve(&mut self, count: usize) {
        self.stack
            .resize(self.stack.len() + count, Value::Uninitialized)
    }

    pub fn rotate(&mut self, count: usize) {
        debug_assert!(self.stack.len() >= count);
        debug_assert!(count >= 2);
        let val = *self.stack.last().unwrap();
        let idx = self.stack.len() - count as usize;
        for i in (idx..(self.stack.len() - 1)).rev() {
            self.stack[i + 1] = self.stack[i];
        }
        self.stack[idx] = val;
    }

    // #[inline(always)]
    pub fn truncate_stack(&mut self, ptr: usize) {
        self.stack.truncate(ptr - STACK);
    }

    // #[inline(always)]
    pub fn stack_top(&self) -> usize {
        STACK + self.stack.len()
    }

    // #[inline(always)]
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

    pub fn get(&self, addr: usize) -> Option<&Value> {
        self.stack
            .get(addr - STACK)
            .or_else(|| self.heap[addr / HEAP - 1].1.get(addr % HEAP))
    }

    pub fn get_mut(&mut self, addr: usize) -> Option<&mut Value> {
        self.stack
            .get_mut(addr - STACK)
            .or_else(|| self.heap[addr / HEAP - 1].1.get_mut(addr % HEAP))
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, addr: usize) -> &mut Self::Output {
        self.stack
            .get_mut(addr - STACK)
            .unwrap_or_else(|| &mut self.heap[addr / HEAP - 1].1[addr % HEAP])
        // if addr >= STACK && addr - STACK < self.stack.len() {
        // &mut self.stack[addr - STACK]
        // } else if addr >= HEAP {
        // &mut self.heap[addr / HEAP - 1].1[addr % HEAP]
        // } else {
        // panic!("0x{:x} not mapped", addr)
        // }
    }
}

impl Index<usize> for Memory {
    type Output = Value;
    fn index(&self, addr: usize) -> &Self::Output {
        // dbg!(addr - STACK, self.stack.get(addr - STACK), &self.stack);
        self.stack
            .get(addr - STACK)
            .unwrap_or_else(|| &self.heap[addr / HEAP - 1].1[addr % HEAP])
        // if addr >= STACK && addr - STACK < self.stack.len() {
        // &self.stack[addr - STACK]
        // } else if addr >= HEAP {
        // &self.heap[addr / HEAP - 1].1[addr % HEAP]
        // } else {
        // panic!("0x{:x} not mapped", addr)
        // }
    }
}
