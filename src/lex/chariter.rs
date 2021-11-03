use std::iter::FlatMap;

pub struct CharIter<T: Iterator<Item = String>> {
    lines: T,
}

impl<T> IntoIterator for CharIter<T>
where
    T: Iterator<Item = String>,
{
    type Item = char;
    type IntoIter = FlatMap<T, std::vec::IntoIter<char>, fn(T::Item) -> std::vec::IntoIter<char>>;

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

impl<T: Iterator<Item = String>> CharIter<T> {
    pub fn new(lines: T) -> Self {
        CharIter { lines }
    }
}
