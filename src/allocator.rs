#![allow(dead_code)]
use std::collections::HashSet;

pub struct Allocator<T> {
    memory: Vec<T>,
    free: HashSet<usize>,
}

impl<T> Allocator<T> {
    pub fn new() -> Self {
        Self {
            memory: Vec::new(),
            free: HashSet::new(),
        }
    }

    fn free_slot(&mut self) -> Option<usize> {
        for element in self.free.iter() {
            return Some(*element);
        }
        None
    }

    pub fn alloc(&mut self, val: T) -> usize {
        match self.free_slot() {
            Some(s) => {
                self.free.remove(&s);
                self.memory[s] = val;
                s
            }
            None => {
                self.memory.push(val);
                self.memory.len() - 1
            }
        }
    }

    pub fn free(&mut self, addr: usize) {
        self.free.insert(addr);
    }
}
