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
    Import,
    As,
    From,
    If,
    Else,
    And,
    While,
    For,
    In,
    Break,
    Continue,
    Return,
    Function,
    Lambda,
    Class,
    Field,
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
    ErrChar(char, &'static str),
    UnexpectedEOF(&'static str),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Identifier(s) => f.write_fmt(format_args!("{}", s)),
            Self::LambdaArg(u) => f.write_fmt(format_args!("{}", u)),
            Self::Str(s) => f.write_fmt(format_args!("\"{}\"", s)),
            Self::Int(i) => f.write_fmt(format_args!("{}", i)),
            Self::Float(d) => f.write_fmt(format_args!("{}", d)),
            Self::Bool(u) => f.write_fmt(format_args!("{}", u)),
            Self::Char(c) => f.write_fmt(format_args!("{}", c)),
            Self::Import => f.write_str("import"),
            Self::As => f.write_str("as"),
            Self::From => f.write_str("from"),
            Self::If => f.write_str("if"),
            Self::Else => f.write_str("else"),
            Self::And => f.write_str("and"),
            Self::While => f.write_str("while"),
            Self::For => f.write_str("for"),
            Self::In => f.write_str("in"),
            Self::Break => f.write_str("break"),
            Self::Continue => f.write_str("continue"),
            Self::Return => f.write_str("return"),
            Self::Function => f.write_str("fn"),
            Self::Lambda => f.write_str("\\"),
            Self::Class => f.write_str("class"),
            Self::Field => f.write_str("field"),
            Self::Null => f.write_str("null"),
            Self::OpenParen => f.write_str("("),
            Self::CloseParen => f.write_str(")"),
            Self::Comma => f.write_str(","),
            Self::Dot => f.write_str("."),
            Self::Equal => f.write_str("="),
            Self::AndEq => f.write_str("&="),
            Self::XorEq => f.write_str("^="),
            Self::OrEq => f.write_str("|="),
            Self::PlusEq => f.write_str("+="),
            Self::MinusEq => f.write_str("-="),
            Self::TimesEq => f.write_str("*="),
            Self::DivEq => f.write_str("/="),
            Self::ModEq => f.write_str("%="),
            Self::Plus => f.write_str("+"),
            Self::Minus => f.write_str("-"),
            Self::Times => f.write_str("*"),
            Self::Div => f.write_str("/"),
            Self::Mod => f.write_str("%"),
            Self::Inc => f.write_str("++"),
            Self::Dec => f.write_str("--"),
            Self::BitShiftRightEq => f.write_str(">>="),
            Self::BitShiftLeftEq => f.write_str("<<="),
            Self::Semicolon => f.write_str(";"),
            Self::OpenBrace => f.write_str("{"),
            Self::CloseBrace => f.write_str("}"),
            Self::OpenBracket => f.write_str("["),
            Self::CloseBracket => f.write_str("]"),
            Self::CmpEq => f.write_str("=="),
            Self::CmpNotEq => f.write_str("!="),
            Self::CmpGE => f.write_str(">="),
            Self::CmpGT => f.write_str(">"),
            Self::CmpLE => f.write_str("<="),
            Self::CmpLT => f.write_str("<"),
            Self::BitOr => f.write_str("|"),
            Self::BoolOr => f.write_str("||"),
            Self::BitAnd => f.write_str("&"),
            Self::BoolAnd => f.write_str("&&"),
            Self::BitNot => f.write_str("~"),
            Self::BitXor => f.write_str("^"),
            Self::BoolXor => f.write_str("^^"),
            Self::BoolNot => f.write_str("!"),
            Self::BitShiftRight => f.write_str(">>"),
            Self::BitShiftLeft => f.write_str("<<"),
            Self::ErrChar(c, s) => {
                f.write_fmt(format_args!("Unexpected character '{c}' while lexing {s}",))
            }
            Self::UnexpectedEOF(s) => f.write_fmt(format_args!("Unexpected EOF while lexing {s}",)),
        }
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
            chars: CharIter::new(lines),
        }
    }

    pub fn new_with_lineno(label: &str, lines: T, lineno: usize) -> Lexer<T> {
        Lexer {
            label: Rc::new(label.into()),
            chars: CharIter::new_with_lineno(lines, lineno),
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        let borrowed: &CharIter<T> = self.chars.borrow();
        let (row, col) = borrowed.location();
        Location::new(self.label.clone(), row, col)
    }

    fn expect_char(&mut self, c: char) -> bool {
        self.chars.next_if(|ch| ch == c).is_some()
    }

    fn string_of_digits(&mut self, radix: u32) -> String {
        self.chars.takewhile(|c| c.is_digit(radix))
    }

    fn num(&mut self, radix: u32) -> TokenType {
        use TokenType::{Float, Int};
        let mut string = self.string_of_digits(radix);
        if !self.expect_char('.') || radix != 10 {
            return Int(i64::from_str_radix(&string, radix).unwrap());
        };
        string.push('.');
        string.push_str(&self.string_of_digits(radix));
        Float(string.parse().unwrap())
    }

    fn identifier(&mut self) -> TokenType {
        use TokenType::Identifier;
        let mut chars = String::new();
        // have to assume first character is valid or is this is a bug and we want to crash
        if let Some(c) = self.chars.next_if(|c| c.is_ascii_alphabetic() || c == '_') {
            chars.push(c)
        } else {
            unreachable!(
                "Called identifier without a ascii alphabetic character or underscore leading"
            );
        }

        while let Some(c) = self
            .chars
            .next_if(|c| c.is_ascii_alphanumeric() || c == '_')
        {
            chars.push(c)
        }

        match &chars[..] {
            "import" => TokenType::Import,
            "as" => TokenType::As,
            "from" => TokenType::From,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "in" => TokenType::In,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "and" => TokenType::And,
            "fn" => TokenType::Function,
            "class" => TokenType::Class,
            "field" => TokenType::Field,
            "null" => TokenType::Null,
            "true" => TokenType::Bool(1),
            "false" => TokenType::Bool(0),
            "break" => TokenType::Break,
            "continue" => TokenType::Continue,
            "return" => TokenType::Return,
            _ => Identifier(chars),
        }
    }

    fn escaped_char_decode(&mut self) -> Option<char> {
        Some(match self.chars.next()? {
            'n' => '\n',
            't' => '\t',
            'r' => '\r',
            '0' => '\0',
            a => a,
        })
    }

    fn single_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some('\\') => self.escaped_char_decode(),
            a => a,
        }
    }

    fn chr(&mut self) -> TokenType {
        use TokenType::Char;
        let v = self.expect_char('\'');
        debug_assert!(v);
        let Some(c) = self.single_char() else {
            return TokenType::UnexpectedEOF("char");
        };
        self.expect_char('\'');
        Char(c)
    }

    fn str(&mut self) -> TokenType {
        let v = self.expect_char('"');
        debug_assert!(v);
        let mut string = String::new();
        while let Some(c) = self.single_char() {
            if c == '"' {
                return TokenType::Str(string);
            } else {
                string.push(c)
            }
        }
        TokenType::UnexpectedEOF("string")
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
        let v = self.expect_char('\\');
        debug_assert!(v);
        let chars = self.string_of_digits(10);
        if chars.is_empty() {
            TokenType::Lambda
        } else {
            // unwrap is OK because of return from string_of_digits
            TokenType::LambdaArg(chars.parse().unwrap())
        }
    }

    fn lex_token(&mut self) -> Option<Token> {
        use TokenType::*;
        loop {
            match self.chars.peek()? {
                c if c.is_ascii_whitespace() => {
                    self.chars.next();
                }
                '#' => {
                    self.chars.takewhile(|c| c != '\n');
                    self.chars.next_if(|c| c == '\n');
                }
                _ => break,
            }
        }
        Some(Token {
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
                '0' => match self.eat_peek() {
                    Some('x' | 'X') => {
                        self.chars.next();
                        self.num(16)
                    }
                    Some('b' | 'B') => {
                        self.chars.next();
                        self.num(2)
                    }
                    Some('o' | 'O') => {
                        self.chars.next();
                        self.num(8)
                    }
                    Some('0'..='9') => self.num(10), // Ok because leading 0
                    Some('.') => self.num(10),
                    _ => Int(0),
                },
                '1'..='9' => self.num(10),
                '\\' => self.lambda_arg(),
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

                _ => ErrChar(self.chars.next()?, "token"),
            },
        })
    }
}
