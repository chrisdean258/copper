#![allow(dead_code)]
use crate::value::Value;
use std::ops::{Index, IndexMut};

#[derive(Clone, Debug)]
pub struct Allocator {
    block_size: usize,
    pub memory: Vec<Value>,
    free: Vec<usize>,
}

impl Allocator {
    pub fn new(block_size: usize) -> Self {
        Self {
            block_size,
            memory: Vec::new(),
            free: Vec::new(),
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
        assert!(idx % self.block_size == 0, "Trying to free unaligned block");
        self.free.push(idx);
    }
}

impl Index<usize> for Allocator {
    type Output = Value;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.memory[idx]
    }
}

impl IndexMut<usize> for Allocator {
    fn index_mut(&mut self, idx: usize) -> &mut Self::Output {
        &mut self.memory[idx]
    }
}
