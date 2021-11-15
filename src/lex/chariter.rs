pub struct CharIter<T: Iterator<Item = String>> {
    lines: T,
    cur_line: Option<Vec<char>>,
    line_no: usize,
    col_no: usize,
}

impl<T: Iterator<Item = String>> Iterator for CharIter<T> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cur_line.is_none() || self.col_no >= self.cur_line.as_ref()?.len() {
            let line = self.lines.next();
            self.cur_line = Some(line?.chars().collect());
            let len = self.cur_line.as_ref()?.len();
            if len == 0 || self.cur_line.as_ref()?[len - 1] != '\n' {
                self.cur_line.as_mut()?.push('\n');
            }
            self.line_no += 1;
            self.col_no = 0;
        }
        self.col_no += 1;
        Some(self.cur_line.as_ref()?[self.col_no - 1])
    }
}

impl<T: Iterator<Item = String>> CharIter<T> {
    pub fn new(lines: T) -> Self {
        CharIter {
            lines,
            cur_line: None,
            line_no: 0,
            col_no: 0,
        }
    }

    pub fn new_with_lineno(lines: T, lineno: usize) -> Self {
        CharIter {
            lines,
            cur_line: None,
            line_no: lineno - 1,
            col_no: 0,
        }
    }

    pub fn location(&self) -> (usize, usize) {
        (self.line_no, self.col_no)
    }
}
