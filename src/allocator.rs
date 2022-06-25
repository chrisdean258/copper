#![allow(dead_code)]
use crate::value::Value;
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug)]
pub struct Allocator {
    block_size: usize,
    pub memory: Vec<Value>,
    free: Vec<usize>,
    oob: Value,
}

impl Allocator {
    pub fn new(block_size: usize) -> Self {
        Self {
            block_size,
            memory: Vec::new(),
            free: Vec::new(),
            oob: Value::Uninitialized,
        }
    }

    pub fn alloc(&mut self) -> usize {
        self.free.pop().unwrap_or_else(|| {
            self.memory
                .append(&mut vec![Value::Uninitialized; self.block_size]);
            self.memory.len() - self.block_size
        })
    }

    pub fn free(&mut self, idx: usize) {
        debug_assert!(idx % self.block_size == 0, "Trying to free unaligned block");
        self.free.push(idx);
    }

    #[inline]
    pub fn get(&self, idx: usize) -> Option<&Value> {
        self.memory.get(idx)
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut Value> {
        self.memory.get_mut(idx)
    }
}

impl Index<usize> for Allocator {
    type Output = Value;

    fn index(&self, idx: usize) -> &Self::Output {
        self.memory.get(idx).unwrap_or(&self.oob)
    }
}

impl IndexMut<usize> for Allocator {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        self.memory.get_mut(idx).unwrap_or(&mut self.oob)
    }
}
