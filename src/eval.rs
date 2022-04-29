use crate::parser::*;

#[derive(Clone, Debug)]
pub struct Object {}

#[derive(Clone, Debug)]
pub struct Evaluator {}

impl Evaluator {
    pub fn new() -> Self {
        Self {}
    }

    pub fn eval(&mut self, _tree: &mut ParseTree) -> Result<Object, String> {
        panic!()
    }
}
