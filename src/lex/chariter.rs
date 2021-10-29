use std::iter::FlatMap;

pub struct CharIter<'a> {
    lines: &'a mut (dyn Iterator<Item = String> + 'a),
}

impl<'a> IntoIterator for CharIter<'a> {
    type Item = char;
    type IntoIter = FlatMap<
        &'a mut (dyn Iterator<Item = String> + 'a),
        std::vec::IntoIter<char>,
        fn(String) -> std::vec::IntoIter<char>,
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.lines.flat_map(|s| {
            let mut v: Vec<char> = s.chars().collect();
            if v.last() != Some(&'\n') {
                v.push('\n');
            }
            v.into_iter()
        })
    }
}

impl<'a> CharIter<'a> {
    pub fn new(lines: &'a mut (dyn Iterator<Item = String> + 'a)) -> Self {
        CharIter { lines }
    }
}
