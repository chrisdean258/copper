#![allow(dead_code)]
use std::collections::HashSet;
use std::ops::{Index, IndexMut};

pub struct Allocator<T> {
    base: usize,
    memory: Vec<T>,
    free: HashSet<usize>,
}

impl<T> Allocator<T> {
    pub fn new(base: usize) -> Self {
        Self {
            base,
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

impl<T> Index<usize> for Allocator<T> {
    type Output = T;

    fn index(&self, addr: usize) -> &Self::Output {
        &self.memory[addr - self.base]
    }
}

impl<T> IndexMut<usize> for Allocator<T> {
    fn index_mut(&mut self, addr: usize) -> &mut Self::Output {
        &mut self.memory[addr - self.base]
    }
}

impl<T> IntoIterator for Allocator<T> {
    type Item = T;

    fn iter(&mut self) -> Vec<T>::IntoIter {
        self.memory
            .iter()
            .enumerate()
            .filter(|(i, v)| !self.free.contains(&i))
            .collect()
            .into_iter()
    }
}
