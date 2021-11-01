use crate::location::Location;
use std::iter::Peekable;
mod chariter;
use chariter::CharIter;

pub struct Lexer<'a> {
    label: &'a str,
    chars: Peekable<<CharIter<'a> as IntoIterator>::IntoIter>,
    row: usize,
    col: usize,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Identifier(String),
    Str(String),
    OpenParen,
    CloseParen,
    Comma,
    Semicolon,
    Char(char),
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl<'a> Lexer<'a> {
    pub fn new(label: &'a str, lines: &'a mut dyn Iterator<Item = String>) -> Lexer<'a> {
        Lexer {
            label,
            chars: CharIter::new(lines).into_iter().peekable(),
            row: 1,
            col: 0,
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        Location::new(self.label, self.row, self.col)
    }

    fn expect<F>(&mut self, func: F) -> char
    where
        F: FnOnce(&char) -> bool,
    {
        assert!(func(self.chars.peek().unwrap()));
        self.col += 1;
        self.chars.next().unwrap()
    }

    fn expect_char(&mut self, c: char) -> char {
        self.expect(|ch| ch == &c)
    }

    fn identifier(&mut self) -> TokenType {
        use TokenType::Identifier;
        let mut chars: Vec<char> = Vec::new();
        match self.chars.peek().unwrap() {
            'a'..='z' => chars.push(self.chars.next().unwrap()),
            'A'..='Z' => chars.push(self.chars.next().unwrap()),
            '_' => chars.push(self.chars.next().unwrap()),
            _ => unreachable!(),
        }
        loop {
            match self.chars.peek().unwrap() {
                '0'..='9' => chars.push(self.chars.next().unwrap()),
                'a'..='z' => chars.push(self.chars.next().unwrap()),
                'A'..='Z' => chars.push(self.chars.next().unwrap()),
                '_' => chars.push(self.chars.next().unwrap()),
                _ => break,
            }
        }

        self.col += chars.len();
        return Identifier(String::from_iter(chars));
    }

    fn str(&mut self) -> TokenType {
        use TokenType::Str;
        self.expect_char('"');
        let mut chars: Vec<char> = Vec::new();
        while let Some(c) = self.chars.next() {
            chars.push(if c == '\\' {
                if let Some(c) = self.chars.next() {
                    match c {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        _ => c,
                    }
                } else {
                    panic!("Unexpected End of File");
                }
            } else if c == '"' {
                break;
            } else {
                c
            });
        }
        self.col += chars.len() + 2;
        Str(String::from_iter(chars))
    }

    fn eat_ret<F>(&mut self, rtn: F) -> F {
        self.chars.next();
        rtn
    }

    fn lex_token(&mut self) -> Option<Token> {
        use TokenType::*;
        loop {
            break Some(Token {
                token_type: match self.chars.peek()? {
                    '(' => self.eat_ret(OpenParen),
                    ')' => self.eat_ret(CloseParen),
                    ',' => self.eat_ret(Comma),
                    ';' => self.eat_ret(Semicolon),
                    'a'..='z' => self.identifier(),
                    'A'..='Z' => self.identifier(),
                    '_' => self.identifier(),
                    '"' => self.str(),
                    '\n' => {
                        self.col = 1;
                        self.row += 1;
                        self.chars.next();
                        continue;
                    }
                    _ => {
                        self.col += 1;
                        Char(self.chars.next()?)
                    }
                },
                location: self.location(),
            });
        }
    }
}
