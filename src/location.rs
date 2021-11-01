use std::fmt;

#[derive(Debug, Clone)]
pub struct Location {
    pub label: String,
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(label: &str, line: usize, column: usize) -> Location {
        Location {
            label: label.into(),
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
