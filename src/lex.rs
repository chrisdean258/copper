use crate::location::Location;
use crate::reiter::{ReIter, ReIterable};
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
mod chariter;
use chariter::CharIter;
use std::rc::Rc;

pub struct Lexer<T: Iterator<Item = String>> {
    label: Rc<String>,
    chars: ReIterable<<CharIter<T> as IntoIterator>::IntoIter>,
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
            If => f.write_fmt(format_args!("if",))?,
            Else => f.write_fmt(format_args!("else",))?,
            And => f.write_fmt(format_args!("and",))?,
            While => f.write_fmt(format_args!("while",))?,
            Function => f.write_fmt(format_args!("fn",))?,
            Lambda => f.write_fmt(format_args!("\\",))?,
            Null => f.write_fmt(format_args!("null",))?,
            OpenParen => f.write_fmt(format_args!("(",))?,
            CloseParen => f.write_fmt(format_args!(")",))?,
            Comma => f.write_fmt(format_args!(",",))?,
            Dot => f.write_fmt(format_args!(".",))?,
            Equal => f.write_fmt(format_args!("=",))?,
            AndEq => f.write_fmt(format_args!("&=",))?,
            XorEq => f.write_fmt(format_args!("^=",))?,
            OrEq => f.write_fmt(format_args!("|=",))?,
            PlusEq => f.write_fmt(format_args!("+=",))?,
            MinusEq => f.write_fmt(format_args!("-=",))?,
            TimesEq => f.write_fmt(format_args!("*=",))?,
            DivEq => f.write_fmt(format_args!("/=",))?,
            ModEq => f.write_fmt(format_args!("%=",))?,
            Plus => f.write_fmt(format_args!("+",))?,
            Minus => f.write_fmt(format_args!("-",))?,
            Times => f.write_fmt(format_args!("*",))?,
            Div => f.write_fmt(format_args!("/",))?,
            Mod => f.write_fmt(format_args!("%",))?,
            Inc => f.write_fmt(format_args!("++",))?,
            Dec => f.write_fmt(format_args!("--",))?,
            BitShiftRightEq => f.write_fmt(format_args!(">>=",))?,
            BitShiftLeftEq => f.write_fmt(format_args!("<<=",))?,
            Semicolon => f.write_fmt(format_args!(";",))?,
            OpenBrace => f.write_fmt(format_args!("{{",))?,
            CloseBrace => f.write_fmt(format_args!("}}",))?,
            OpenBracket => f.write_fmt(format_args!("[",))?,
            CloseBracket => f.write_fmt(format_args!("]",))?,
            CmpEq => f.write_fmt(format_args!("==",))?,
            CmpNotEq => f.write_fmt(format_args!("!=",))?,
            CmpGE => f.write_fmt(format_args!(">=",))?,
            CmpGT => f.write_fmt(format_args!(">",))?,
            CmpLE => f.write_fmt(format_args!("<=",))?,
            CmpLT => f.write_fmt(format_args!("<",))?,
            BitOr => f.write_fmt(format_args!("|",))?,
            BoolOr => f.write_fmt(format_args!("||",))?,
            BitAnd => f.write_fmt(format_args!("&",))?,
            BoolAnd => f.write_fmt(format_args!("&&",))?,
            BitNot => f.write_fmt(format_args!("~",))?,
            BitXor => f.write_fmt(format_args!("^",))?,
            BoolXor => f.write_fmt(format_args!("^^",))?,
            BoolNot => f.write_fmt(format_args!("!",))?,
            BitShiftRight => f.write_fmt(format_args!(">>",))?,
            BitShiftLeft => f.write_fmt(format_args!("<<",))?,
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
            chars: CharIter::new(lines).into_iter().reiter(),
        }
    }

    pub fn new_with_lineno(label: &str, lines: T, lineno: usize) -> Lexer<T> {
        Lexer {
            label: Rc::new(label.into()),
            chars: CharIter::new_with_lineno(lines, lineno)
                .into_iter()
                .reiter(),
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        let borrowed: &CharIter<T> = self.chars.borrow();
        let (row, col) = borrowed.location();
        Location::new(self.label.clone(), row, col + 1)
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

                    _ => ErrChar(self.chars.next()?),
                },
            });
        }
    }
}
