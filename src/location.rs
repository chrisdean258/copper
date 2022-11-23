use std::fmt;
use std::fmt::Arguments;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Location {
    pub label: Rc<String>,
    pub line: usize,
    pub column: usize,
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
