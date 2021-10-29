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

#[allow(dead_code)]
#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
    location: Location,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum TokenType {
    Identifier(String),
    OpenParen(char),
    CloseParen(char),
    Str(String),
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

        return Identifier(String::from_iter(chars));
    }

    fn lex_token(&mut self) -> Option<Token> {
        use TokenType::*;
        Some(Token {
            token_type: match self.chars.peek()? {
                '(' => OpenParen(self.chars.next()?),
                ')' => CloseParen(self.chars.next()?),
                'a'..='z' => self.identifier(),
                'A'..='Z' => self.identifier(),
                '_' => self.identifier(),
                _ => Char(self.chars.next()?),
            },
            location: self.location(),
        })
    }
}
