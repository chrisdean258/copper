pub struct CharIter {
    chars: Vec<char>,
    line_no: usize,
    col_no: usize,
    idx: usize,
}

impl Iterator for CharIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let rv = self.chars.get(self.idx).cloned()?;
        self.idx += 1;
        if rv == '\n' {
            self.line_no += 1;
            self.col_no = 0;
        }
        self.col_no += 1;
        Some(rv)
    }
}

impl CharIter {
    pub fn from_lines<T: Iterator<Item = String>>(lines: T) -> Self {
        Self::from_lines_with_lineno(lines, 1)
    }

    pub fn from_lines_with_lineno<T: Iterator<Item = String>>(lines: T, lineno: usize) -> Self {
        let string = lines
            .flat_map(|s| {
                let mut c = s.chars().collect::<Vec<char>>();
                if c.is_empty() || c.last() != Some(&'\n') {
                    c.push('\n');
                }
                c
            })
            .collect::<String>();
        Self::new_with_lineno(string, lineno)
    }

    pub fn new(string: String) -> Self {
        Self::new_with_lineno(string, 1)
    }

    pub fn new_with_lineno(string: String, lineno: usize) -> Self {
        CharIter {
            chars: string.chars().collect(),
            line_no: lineno,
            col_no: 1,
            idx: 0,
        }
    }

    pub fn location(&self) -> (usize, usize) {
        (self.line_no, self.col_no)
    }

    pub fn peek(&mut self) -> Option<char> {
        self.chars.get(self.idx).cloned()
    }

    pub fn next_if<F>(&mut self, func: F) -> Option<char>
    where
        F: FnOnce(char) -> bool,
    {
        let c = self.peek()?;
        if func(c) {
            self.next();
            Some(c)
        } else {
            None
        }
    }

    pub fn takewhile<F>(&mut self, func: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut rv = String::new();
        while let Some(c) = self.next_if(&func) {
            rv.push(c);
        }
        rv
    }
}
