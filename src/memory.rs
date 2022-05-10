use std::ops::{Index, IndexMut};

pub const HEAP: usize = 0x10000000;
pub const STACK: usize = 0x1000000;
pub const CODE: usize = 0x100000;
pub const BUILTIN_CODE: usize = 0x10000;

pub struct Memory {
    pub stack: Vec<u64>,
    pub heap: Vec<u64>,
    pub code: Vec<u64>,
}

impl IndexMut<usize> for Memory {
    fn index_mut(&mut self, addr: usize) -> &mut Self::Output {
        if addr >= HEAP {
            &mut self.heap[addr - HEAP]
        } else if addr >= STACK {
            &mut self.stack[addr - STACK]
        } else if addr >= CODE {
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
