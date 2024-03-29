use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Location {
    pub label: Rc<String>,
    pub line: usize,
    pub column: usize,
    pub is_eof: bool,
}

#[derive(Debug, Clone)]
pub struct Span {
    pub label: Rc<String>,
    pub startline: usize,
    pub endline: usize,
    pub startcolumn: usize,
    pub endcolumn: usize,
}

impl Location {
    pub fn new(label: Rc<String>, line: usize, column: usize) -> Location {
        Location {
            label,
            line,
            column,
            is_eof: false,
        }
    }

    pub fn new_eof(label: Rc<String>) -> Location {
        Location {
            label,
            line: 0,
            column: 0,
            is_eof: true,
        }
    }
    #[allow(dead_code)]
    pub fn err(&self, errmsg: &str) -> String {
        format!("{}: {}", self, errmsg)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_eof {
            write!(f, "{}", self.label)
        } else {
            write!(f, "{}:{}:{}", self.label, self.line, self.column)
        }
    }
}

#[allow(dead_code)]
impl Span {
    pub fn from_locations(start: &Location, end: &Location) -> Option<Self> {
        if start.label == end.label {
            Some(Self {
                label: start.label.clone(),
                startline: start.line,
                endline: end.line,
                startcolumn: start.column,
                endcolumn: end.column,
            })
        } else {
            None
        }
    }

    pub fn from_spans(start: &Span, end: &Span) -> Option<Self> {
        if start.label == end.label {
            Some(Self {
                label: start.label.clone(),
                startline: start.startline,
                endline: end.endline,
                startcolumn: start.startcolumn,
                endcolumn: end.endcolumn,
            })
        } else {
            None
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: flesh this out to give better indication of spans
        write!(f, "{}:{}:{}", self.label, self.startline, self.startcolumn)
    }
}
