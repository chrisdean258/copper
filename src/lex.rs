use crate::location::Location;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
mod chariter;
use chariter::CharIter;
use std::rc::Rc;

pub struct Lexer<T: Iterator<Item = String>> {
    label: Rc<String>,
    chars: CharIter<T>,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub enum TokenType {
    Identifier(String),
    LambdaArg(usize),
    Str(String),
    Int(i64),
    Float(f64),
    Bool(u8),
    Char(char),
    If,
    Else,
    And,
    While,
    Function,
    Lambda,
    Global,
    Null,
    OpenParen,
    CloseParen,
    Comma,
    Dot,
    Equal,
    AndEq,
    XorEq,
    OrEq,
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
    OpenBracket,
    CloseBracket,
    CmpEq,
    CmpNotEq,
    CmpGE,
    CmpGT,
    CmpLE,
    CmpLT,
    BitOr,
    BoolOr,
    BitAnd,
    BoolAnd,
    BitNot,
    BitXor,
    BoolXor,
    BoolNot,
    BitShiftRight,
    BitShiftLeft,
    ErrChar(char),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use TokenType::*;
        match self {
            Identifier(s) => f.write_fmt(format_args!("{}", s))?,
            LambdaArg(u) => f.write_fmt(format_args!("{}", u))?,
            Str(s) => f.write_fmt(format_args!("\"{}\"", s))?,
            Int(i) => f.write_fmt(format_args!("{}", i))?,
            Float(d) => f.write_fmt(format_args!("{}", d))?,
            Bool(u) => f.write_fmt(format_args!("{}", u))?,
            Char(c) => f.write_fmt(format_args!("{}", c))?,
            If => f.write_str("if")?,
            Else => f.write_str("else")?,
            And => f.write_str("and")?,
            While => f.write_str("while")?,
            Function => f.write_str("fn")?,
            Global => f.write_str("global")?,
            Lambda => f.write_str("\\")?,
            Null => f.write_str("null")?,
            OpenParen => f.write_str("(")?,
            CloseParen => f.write_str(")")?,
            Comma => f.write_str(",")?,
            Dot => f.write_str(".")?,
            Equal => f.write_str("=")?,
            AndEq => f.write_str("&=")?,
            XorEq => f.write_str("^=")?,
            OrEq => f.write_str("|=")?,
            PlusEq => f.write_str("+=")?,
            MinusEq => f.write_str("-=")?,
            TimesEq => f.write_str("*=")?,
            DivEq => f.write_str("/=")?,
            ModEq => f.write_str("%=")?,
            Plus => f.write_str("+")?,
            Minus => f.write_str("-")?,
            Times => f.write_str("*")?,
            Div => f.write_str("/")?,
            Mod => f.write_str("%")?,
            Inc => f.write_str("++")?,
            Dec => f.write_str("--")?,
            BitShiftRightEq => f.write_str(">>=")?,
            BitShiftLeftEq => f.write_str("<<=")?,
            Semicolon => f.write_str(";")?,
            OpenBrace => f.write_str("{{")?,
            CloseBrace => f.write_str("}}")?,
            OpenBracket => f.write_str("[")?,
            CloseBracket => f.write_str("]")?,
            CmpEq => f.write_str("==")?,
            CmpNotEq => f.write_str("!=")?,
            CmpGE => f.write_str(">=")?,
            CmpGT => f.write_str(">")?,
            CmpLE => f.write_str("<=")?,
            CmpLT => f.write_str("<")?,
            BitOr => f.write_str("|")?,
            BoolOr => f.write_str("||")?,
            BitAnd => f.write_str("&")?,
            BoolAnd => f.write_str("&&")?,
            BitNot => f.write_str("~")?,
            BitXor => f.write_str("^")?,
            BoolXor => f.write_str("^^")?,
            BoolNot => f.write_str("!")?,
            BitShiftRight => f.write_str(">>")?,
            BitShiftLeft => f.write_str("<<")?,
            ErrChar(c) => f.write_fmt(format_args!("error: {}", c))?,
        }
        Ok(())
    }
}

impl<T: Iterator<Item = String>> Iterator for Lexer<T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl<T: Iterator<Item = String>> Lexer<T> {
    pub fn new(label: &str, lines: T) -> Lexer<T> {
        Lexer {
            label: Rc::new(label.into()),
            chars: CharIter::new(lines).into_iter(),
        }
    }

    pub fn new_with_lineno(label: &str, lines: T, lineno: usize) -> Lexer<T> {
        Lexer {
            label: Rc::new(label.into()),
            chars: CharIter::new_with_lineno(lines, lineno).into_iter(),
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        let borrowed: &CharIter<T> = self.chars.borrow();
        let (row, col) = borrowed.location();
        Location::new(self.label.clone(), row, col)
    }

    fn expect<F>(&mut self, func: F) -> char
    where
        F: FnOnce(&char) -> bool,
    {
        assert!(func(&self.chars.peek().unwrap()));
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
        let mut chars = String::new();
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

        match &chars[..] {
            "while" => TokenType::While,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "and" => TokenType::And,
            "fn" => TokenType::Function,
            "null" => TokenType::Null,
            "global" => TokenType::Global,
            "true" => TokenType::Bool(1),
            "false" => TokenType::Bool(0),
            _ => Identifier(chars),
        }
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

    fn eat_peek(&mut self) -> Option<char> {
        self.chars.next();
        self.chars.peek()
    }

    fn lambda_arg(&mut self) -> TokenType {
        match self.chars.next().unwrap() {
            '\\' => (),
            _ => unreachable!(),
        }

        let mut chars = String::new();

        loop {
            match self.chars.peek().unwrap() {
                '0'..='9' => chars.push(self.chars.next().unwrap()),
                _ => break,
            }
        }
        if chars.len() == 0 {
            TokenType::Lambda
        } else {
            TokenType::LambdaArg(chars.parse().expect("Expected integer"))
        }
    }

    fn lex_token(&mut self) -> Option<Token> {
        use TokenType::*;
        loop {
            self.chars.peek();
            break Some(Token {
                location: self.location(),
                token_type: match self.chars.peek()? {
                    '(' => self.eat_ret(OpenParen),
                    ')' => self.eat_ret(CloseParen),
                    '{' => self.eat_ret(OpenBrace),
                    '}' => self.eat_ret(CloseBrace),
                    '[' => self.eat_ret(OpenBracket),
                    ']' => self.eat_ret(CloseBracket),
                    ',' => self.eat_ret(Comma),
                    '.' => self.eat_ret(Dot),
                    ';' => self.eat_ret(Semicolon),
                    'a'..='z' => self.identifier(),
                    'A'..='Z' => self.identifier(),
                    '_' => self.identifier(),
                    '"' => self.str(),
                    '\'' => self.chr(),
                    '0'..='9' => self.num(),
                    '\\' => self.lambda_arg(),
                    '\n' => {
                        self.chars.next();
                        continue;
                    }
                    ' ' => {
                        self.chars.next();
                        continue;
                    }
                    '\t' => {
                        self.chars.next();
                        continue;
                    }
                    '=' => match self.eat_peek() {
                        Some('=') => self.eat_ret(CmpEq),
                        _ => Equal,
                    },
                    '|' => match self.eat_peek() {
                        Some('|') => self.eat_ret(BoolOr),
                        Some('=') => self.eat_ret(OrEq),
                        _ => BitOr,
                    },
                    '&' => match self.eat_peek() {
                        Some('&') => self.eat_ret(BoolAnd),
                        Some('=') => self.eat_ret(AndEq),
                        _ => BitAnd,
                    },
                    '^' => match self.eat_peek() {
                        Some('^') => self.eat_ret(BoolXor),
                        Some('=') => self.eat_ret(XorEq),
                        _ => BitXor,
                    },
                    '!' => match self.eat_peek() {
                        Some('=') => self.eat_ret(CmpNotEq),
                        _ => BoolNot,
                    },
                    '>' => match self.eat_peek() {
                        Some('>') => match self.eat_peek() {
                            Some('=') => self.eat_ret(BitShiftRightEq),
                            _ => BitShiftRight,
                        },
                        Some('=') => self.eat_ret(CmpGE),
                        _ => CmpGT,
                    },
                    '<' => match self.eat_peek() {
                        Some('<') => match self.eat_peek() {
                            Some('=') => self.eat_ret(BitShiftLeftEq),
                            _ => BitShiftLeft,
                        },
                        Some('=') => self.eat_ret(CmpLE),
                        _ => CmpLT,
                    },

                    '+' => match self.eat_peek() {
                        Some('+') => self.eat_ret(Inc),
                        Some('=') => self.eat_ret(PlusEq),
                        _ => Plus,
                    },

                    '-' => match self.eat_peek() {
                        Some('-') => self.eat_ret(Dec),
                        Some('=') => self.eat_ret(MinusEq),
                        _ => Minus,
                    },

                    '%' => match self.eat_peek() {
                        Some('=') => self.eat_ret(ModEq),
                        _ => Mod,
                    },

                    '*' => match self.eat_peek() {
                        Some('=') => self.eat_ret(TimesEq),
                        _ => Times,
                    },

                    '/' => match self.eat_peek() {
                        Some('=') => self.eat_ret(DivEq),
                        _ => Div,
                    },
                    '~' => self.eat_ret(BitNot),
                    '#' => {
                        while let Some(c) = self.chars.next() {
                            match c {
                                '\n' => break,
                                _ => (),
                            }
                        }
                        continue;
                    }

                    _ => ErrChar(self.chars.next()?),
                },
            });
        }
    }
}
