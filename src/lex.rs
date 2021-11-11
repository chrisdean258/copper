use crate::location::Location;
use crate::reiter::{ReIter, ReIterable};
use std::borrow::Borrow;
mod chariter;
use chariter::CharIter;
use std::rc::Rc;

pub struct Lexer<T: Iterator<Item = String>> {
    label: Rc<String>,
    chars: ReIterable<<CharIter<T> as IntoIterator>::IntoIter>,
    row: usize,
    col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum TokenType {
    Identifier(String),
    Str(String),
    Int(i64),
    Float(f64),
    OpenParen,
    CloseParen,
    Comma,
    Dot,
    Equal,
    AndEq,
    XorEq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    Inc,
    Dec,
    BitShiftRightEq,
    BitShiftLeftEq,
    Semicolon,
    OpenBrace,
    CloseBrace,
    Char(char),
    ErrChar(char),
    Keyword(Keyword),
    CmpEqual,
    CmpGE,
    CmpGT,
    CmpLE,
    CmpLT,
    BitOr,
    BoolOr,
    BitAnd,
    BoolAnd,
    BitXor,
    BoolXor,
    BitShiftRight,
    BitShiftLeft,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Keyword {
    If,
    Else,
}

impl<T: Iterator<Item = String>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let borrowed: &CharIter<T> = self.chars.borrow();
        let (row, col) = borrowed.location();
        self.row = row;
        self.col = col;
        self.lex_token()
    }
}

impl<T: Iterator<Item = String>> Lexer<T> {
    pub fn new(label: &str, lines: T) -> Lexer<T> {
        Lexer {
            label: Rc::new(label.into()),
            chars: CharIter::new(lines).into_iter().reiter(),
            row: 1,
            col: 0,
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        Location::new(self.label.clone(), self.row, self.col)
    }

    fn expect<F>(&mut self, func: F) -> char
    where
        F: FnOnce(&char) -> bool,
    {
        assert!(func(&self.chars.peek().unwrap()));
        self.col += 1;
        self.chars.next().unwrap()
    }

    fn expect_char(&mut self, c: char) -> char {
        self.expect(|ch| ch == &c)
    }

    fn num(&mut self) -> TokenType {
        use TokenType::{Float, Int};
        let mut chars = Vec::new();
        loop {
            match self.chars.peek() {
                Some(c) => match c {
                    '0'..='9' => chars.push(self.chars.next().unwrap()),
                    _ => break,
                },
                None => break,
            }
        }
        if self.chars.peek() != Some('.') {
            let s: String = chars.into_iter().collect();
            return Int(s.parse().unwrap());
        }
        self.chars.next();
        chars.push('.');
        loop {
            match self.chars.peek() {
                Some(c) => match c {
                    '0'..='9' => chars.push(self.chars.next().unwrap()),
                    _ => break,
                },
                None => break,
            }
        }
        let s: String = chars.into_iter().collect();
        return Float(s.parse().unwrap());
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

    fn chr(&mut self) -> TokenType {
        use TokenType::Char;
        self.expect_char('\'');
        let c: char;
        if let Some(cc) = self.chars.next() {
            c = if cc == '\\' {
                if let Some(ccc) = self.chars.next() {
                    match ccc {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '0' => '\0',
                        _ => ccc,
                    }
                } else {
                    panic!("Unexpected End of File");
                }
            } else {
                cc
            }
        } else {
            panic!("Unexpected End of File");
        }
        self.expect_char('\'');
        return Char(c);
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
                    '{' => self.eat_ret(OpenBrace),
                    '}' => self.eat_ret(CloseBrace),
                    ',' => self.eat_ret(Comma),
                    '.' => self.eat_ret(Dot),
                    ';' => self.eat_ret(Semicolon),
                    'a'..='z' => self.identifier(),
                    'A'..='Z' => self.identifier(),
                    '_' => self.identifier(),
                    '"' => self.str(),
                    '\'' => self.chr(),
                    '0'..='9' => self.num(),
                    '\n' => {
                        self.col = 1;
                        self.row += 1;
                        self.chars.next();
                        continue;
                    }
                    ' ' => {
                        self.row += 1;
                        self.chars.next();
                        continue;
                    }
                    '\t' => {
                        self.row += 1;
                        self.chars.next();
                        continue;
                    }
                    '=' => match self.eat_ret(self.chars.peek()) {
                        Some('=') => self.eat_ret(CmpEqual),
                        _ => Equal,
                    },
                    '|' => match self.eat_ret(self.chars.peek()) {
                        Some('|') => self.eat_ret(BoolOr),
                        _ => BitOr,
                    },
                    '&' => match self.eat_ret(self.chars.peek()) {
                        Some('&') => self.eat_ret(BoolAnd),
                        Some('=') => self.eat_ret(AndEq),
                        _ => BitAnd,
                    },
                    '^' => match self.eat_ret(self.chars.peek()) {
                        Some('^') => self.eat_ret(BoolXor),
                        Some('=') => self.eat_ret(XorEq),
                        _ => BitXor,
                    },
                    '>' => match self.eat_ret(self.chars.peek()) {
                        Some('>') => match self.eat_ret(self.chars.peek()) {
                            Some('=') => self.eat_ret(BitShiftRightEq),
                            _ => BitShiftRight,
                        },
                        Some('=') => self.eat_ret(CmpGE),
                        _ => CmpGT,
                    },
                    '<' => match self.eat_ret(self.chars.peek()) {
                        Some('<') => match self.eat_ret(self.chars.peek()) {
                            Some('=') => self.eat_ret(BitShiftLeftEq),
                            _ => BitShiftLeft,
                        },
                        Some('=') => self.eat_ret(CmpLE),
                        _ => CmpLT,
                    },

                    '+' => match self.eat_ret(self.chars.peek()) {
                        Some('+') => self.eat_ret(Inc),
                        Some('=') => self.eat_ret(PlusEq),
                        _ => Plus,
                    },

                    '-' => match self.eat_ret(self.chars.peek()) {
                        Some('-') => self.eat_ret(Dec),
                        Some('=') => self.eat_ret(MinusEq),
                        _ => Minus,
                    },

                    '%' => match self.eat_ret(self.chars.peek()) {
                        Some('=') => self.eat_ret(ModEq),
                        _ => Mod,
                    },

                    '*' => match self.eat_ret(self.chars.peek()) {
                        Some('=') => self.eat_ret(TimesEq),
                        _ => Times,
                    },

                    '/' => match self.eat_ret(self.chars.peek()) {
                        Some('=') => self.eat_ret(DivEq),
                        _ => Div,
                    },

                    _ => {
                        self.col += 1;
                        ErrChar(self.chars.next()?)
                    }
                },
                location: self.location(),
            });
        }
    }
}
