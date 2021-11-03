use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Location {
    pub label: Rc<String>,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(label: Rc<String>, line: usize, column: usize) -> Location {
        Location {
            label,
            line,
            column,
        }
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.label, self.line, self.column)
    }
}
