pub struct CharIter<T: Iterator<Item = String>> {
    lines: T,
    cur_line: Option<Vec<char>>,
    line_no: usize,
    col_no: usize,
}

// impl<T> IntoIterator for CharIter<T>
// where
// T: Iterator<Item = String>,
// {
// type Item = char;
// type IntoIter = FlatMap<T, std::vec::IntoIter<char>, fn(T::Item) -> std::vec::IntoIter<char>>;

// fn into_iter(self) -> Self::IntoIter {
// self.lines.flat_map(|s| {
// let mut v: Vec<char> = s.chars().collect();
// if v.last() != Some(&'\n') {
// v.push('\n');
// }
// v.into_iter()
// })
// }
// }

impl<T: Iterator<Item = String>> Iterator for CharIter<T> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        while self.cur_line.is_none() || self.col_no >= self.cur_line.as_ref()?.len() {
            let line = self.lines.next();
            self.cur_line = Some(line?.chars().collect());
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

    pub fn location(&self) -> (usize, usize) {
        (self.line_no, self.col_no)
    }
}
