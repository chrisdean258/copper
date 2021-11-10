use std::borrow::{Borrow, BorrowMut};

#[derive(Clone)]
pub struct ReIterable<T: Iterator> {
    iterable: T,
    record_: Option<Vec<T::Item>>,
    offsets: Vec<usize>,
    goff: usize,
}

pub trait ReIter<T>
where
    T: Iterator,
    T::Item: Clone,
{
    fn reiter(self) -> ReIterable<T>;
}

impl<T> ReIter<T> for T
where
    T: Iterator,
    T::Item: Clone,
{
    fn reiter(self) -> ReIterable<T> {
        ReIterable {
            iterable: self,
            record_: None,
            offsets: vec![],
            goff: 0,
        }
    }
}

impl<T> ReIterable<T>
where
    T: Iterator,
    T::Item: Clone,
{
    pub fn record(&mut self) {
        if self.record_.is_none() {
            self.record_ = Some(vec![]);
            self.offsets = vec![];
            self.goff = 0;
        }
        self.offsets.push(self.goff);
    }

    pub fn commit(&mut self) -> usize {
        let off = self.offsets.pop().unwrap();
        if self.offsets.len() == 0 {
            self.record_ = None;
            off
        } else {
            off - self.offsets[self.offsets.len() - 1]
        }
    }

    // pub fn commit_all(&mut self) {
        // self.record_ = Some(vec![]);
        // self.offsets = vec![];
        // self.goff = 0;
    // }

    pub fn rollback(&mut self) -> usize {
        let off = self.offsets.pop().unwrap();
        let rv = self.goff - off;
        self.goff = off;
        rv
    }

    pub fn peek(&mut self) -> Option<T::Item> {
        self.record();
        let rv = self.next();
        self.rollback();
        rv
    }

    pub fn next_if<F>(&mut self, f: F) -> Option<T::Item>
    where
        F: FnOnce(&T::Item) -> bool,
    {
        if f(&self.peek()?) {
            self.next()
        } else {
            None
        }
    }
}

impl<T> Iterator for ReIterable<T>
where
    T: Iterator,
    T::Item: Clone,
{
    type Item = T::Item;
    fn next(&mut self) -> Option<<T as Iterator>::Item> {
        match &mut self.record_ {
            None => self.iterable.next(),
            Some(v) => {
                self.goff += 1;
                if self.goff > v.len() {
                    let rv = self.iterable.next()?;
                    v.push(rv.clone());
                    Some(rv)
                } else {
                    Some(v[self.goff - 1].clone())
                }
            }
        }
    }
}

impl<T> Borrow<T> for ReIterable<T>
where
    T: Iterator,
    T::Item: Clone,
{
    fn borrow(&self) -> &T {
        &self.iterable
    }
}

impl<T> BorrowMut<T> for ReIterable<T>
where
    T: Iterator,
    T::Item: Clone,
{
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.iterable
    }
}
