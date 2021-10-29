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
    #[allow(dead_code)]
    pub fn into_string(&self) -> String {
        format!("{}:{}:{}", self.label, self.line, self.column)
    }
}
