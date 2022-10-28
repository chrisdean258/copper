use std::fmt;
use std::fmt::Arguments;
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

    #[allow(dead_code)]
    pub fn err(&self, errmsg: &str) -> String {
        format!("{}: {}", self, errmsg)
    }

    pub fn errfmt(&self, args: Arguments) -> String {
        format!("{}: {}", self, args)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}:{}", self.label, self.line, self.column)
    }
}
