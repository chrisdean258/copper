use std::{
    default::Default,
    error::Error,
    fmt::{Display, Formatter, Result},
};

#[derive(Clone, Debug)]
pub struct ErrorCollection<T: Error> {
    errs: Vec<T>,
}

impl<T: Error> Error for ErrorCollection<T> {}

impl<T: Error> Display for ErrorCollection<T> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for error in self.errs.iter() {
            writeln!(f, "{error}")?;
        }
        Ok(())
    }
}

impl<T: Error> Default for ErrorCollection<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Error> ErrorCollection<T> {
    pub fn new() -> Self {
        Self { errs: Vec::new() }
    }

    pub fn push(&mut self, err: T) {
        self.errs.push(err);
    }
}
