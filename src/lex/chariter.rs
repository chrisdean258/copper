pub struct CharIter {
    chars: Vec<char>,
    line_no: usize,
    col_no: usize,
    prev_line_no: usize,
    prev_col_no: usize,
    idx: usize,
}

impl Iterator for CharIter {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let rv = self.chars.get(self.idx).cloned();
        self.idx += 1;
        self.prev_line_no = self.line_no;
        self.prev_col_no = self.col_no;
        if let Some('\n') = self.peek() {
            self.line_no += 1;
            self.col_no = 0;
        }
        self.col_no += 1;
        rv
    }
}

impl CharIter {
    pub fn new<T: Iterator<Item = String>>(lines: T) -> Self {
        CharIter {
            chars: lines
                .flat_map(|s| {
                    let mut c = s.chars().collect::<Vec<char>>();
                    if c.is_empty() || c.last() != Some(&'\n') {
                        c.push('\n');
                    }
                    c
                })
                .collect(),
            line_no: 1,
            col_no: 1,
            prev_col_no: 1,
            prev_line_no: 1,
            idx: 0,
        }
    }

    pub fn new_with_lineno<T: Iterator<Item = String>>(lines: T, lineno: usize) -> Self {
        CharIter {
            chars: lines
                .flat_map(|s| s.chars().collect::<Vec<char>>())
                .collect(),
            line_no: lineno,
            col_no: 0,
            prev_col_no: 0,
            prev_line_no: lineno,
            idx: 0,
        }
    }

    pub fn location(&self) -> (usize, usize) {
        (self.prev_line_no, self.prev_col_no)
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
