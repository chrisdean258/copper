use crate::value::Value;
use std::ops::{Index, IndexMut};

pub const HEAP: usize = 0x10000000;
pub const STACK: usize = 0x1000000;
pub const CODE: usize = 0x100000;
pub const BUILTIN_CODE: usize = 0x10000;

#[derive(Clone, Debug)]
pub struct Memory {
    pub stack: Vec<u64>,
    pub heap: Vec<u64>,
    pub code: Vec<u64>,
}

impl Memory {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            heap: Vec::new(),
            code: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, val: Value) {
        self.stack.push(val.encode_full());
    }

    #[inline]
    pub fn push_enc(&mut self, val: u64) {
        self.stack.push(val);
    }

    #[inline]
    pub fn pop_as(&mut self, what: Value) -> Value {
        assert!(self.stack.len() >= 1);
        let val = self.stack.pop().unwrap();
        Value::decode_full(val, what)
    }

    #[inline]
    pub fn pop(&mut self) -> u64 {
        assert!(self.stack.len() >= 1);
        self.stack.pop().unwrap()
    }

    #[inline]
    pub fn reserve(&mut self, count: usize) {
        self.stack.resize(self.stack.len() + count, 0)
    }

    #[inline]
    pub fn rotate(&mut self, count: usize) {
        assert!(self.stack.len() >= count && count >= 2);
        let val = *self.stack.last().unwrap();
        let idx = self.stack.len() - count as usize;
        for i in (idx..(self.stack.len() - 1)).rev() {
            self.stack[i + 1] = self.stack[i];
        }
        self.stack[idx] = val;
    }

    #[inline]
    pub fn dup(&mut self) {
        assert!(self.stack.len() >= 1);
        self.stack.push(*self.stack.last().unwrap());
    }

    #[inline]
    pub fn swap(&mut self) {
        assert!(self.stack.len() >= 2);
        let a = self.pop();
        let b = self.pop();
        self.push_enc(a);
        self.push_enc(b);
    }

    #[inline]
    pub fn truncate_stack(&mut self, ptr: usize) {
        self.stack.truncate(ptr - STACK);
    }

    #[inline]
    pub fn stack_top(&self) -> usize {
        STACK + self.stack.len()
    }
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, addr: usize) -> &mut Self::Output {
        if addr >= HEAP && addr - HEAP < self.heap.len() {
            &mut self.heap[addr - HEAP]
        } else if addr >= STACK && addr - STACK < self.stack.len() {
            &mut self.stack[addr - STACK]
        } else if addr >= CODE && addr - CODE < self.code.len() {
            &mut self.code[addr - CODE]
        } else if addr >= BUILTIN_CODE {
            panic!("Cannot write builtin code as addr 0x{:x}", addr)
        } else {
            panic!("Below builtin code is not mapped at 0x{:x}", addr)
        }
    }
}

impl Index<usize> for Memory {
    type Output = u64;
    fn index(&self, addr: usize) -> &Self::Output {
        if addr >= HEAP {
            &self.heap[addr - HEAP]
        } else if addr >= STACK {
            &self.stack[addr - STACK]
        } else if addr >= CODE {
            &self.code[addr - CODE]
        } else if addr >= BUILTIN_CODE {
            panic!("Cannot read builtin code as addr 0x{:x}", addr)
        } else {
            panic!("Below builtin code is not mapped at 0x{:x}", addr)
        }
    }
}
