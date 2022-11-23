use crate::location::Location;
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
mod chariter;
use chariter::CharIter;
use std::rc::Rc;

pub struct Lexer {
    label: Rc<String>,
    chars: CharIter,
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
            Self::Identifier(s) => write!(f, "{s}"),
            Self::LambdaArg(u) => write!(f, "{u}"),
            Self::Str(s) => write!(f, "\"{}\"", s),
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(d) => write!(f, "{}", d),
            Self::Bool(u) => write!(f, "{}", u),
            Self::Char(c) => write!(f, "{}", c),
            Self::Import => write!(f, "import"),
            Self::As => write!(f, "as"),
            Self::From => write!(f, "from"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::And => write!(f, "and"),
            Self::While => write!(f, "while"),
            Self::For => write!(f, "for"),
            Self::In => write!(f, "in"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::Return => write!(f, "return"),
            Self::Function => write!(f, "fn"),
            Self::Lambda => write!(f, "\\"),
            Self::Class => write!(f, "class"),
            Self::Field => write!(f, "field"),
            Self::Null => write!(f, "null"),
            Self::OpenParen => write!(f, "("),
            Self::CloseParen => write!(f, ")"),
            Self::Comma => write!(f, ","),
            Self::Dot => write!(f, "."),
            Self::Equal => write!(f, "="),
            Self::AndEq => write!(f, "&="),
            Self::XorEq => write!(f, "^="),
            Self::OrEq => write!(f, "|="),
            Self::PlusEq => write!(f, "+="),
            Self::MinusEq => write!(f, "-="),
            Self::TimesEq => write!(f, "*="),
            Self::DivEq => write!(f, "/="),
            Self::ModEq => write!(f, "%="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Times => write!(f, "*"),
            Self::Div => write!(f, "/"),
            Self::Mod => write!(f, "%"),
            Self::Inc => write!(f, "++"),
            Self::Dec => write!(f, "--"),
            Self::BitShiftRightEq => write!(f, ">>="),
            Self::BitShiftLeftEq => write!(f, "<<="),
            Self::Semicolon => write!(f, ";"),
            Self::OpenBrace => write!(f, "{{"),
            Self::CloseBrace => write!(f, "}}"),
            Self::OpenBracket => write!(f, "["),
            Self::CloseBracket => write!(f, "]"),
            Self::CmpEq => write!(f, "=="),
            Self::CmpNotEq => write!(f, "!="),
            Self::CmpGE => write!(f, ">="),
            Self::CmpGT => write!(f, ">"),
            Self::CmpLE => write!(f, "<="),
            Self::CmpLT => write!(f, "<"),
            Self::BitOr => write!(f, "|"),
            Self::BoolOr => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BoolAnd => write!(f, "&&"),
            Self::BitNot => write!(f, "~"),
            Self::BitXor => write!(f, "^"),
            Self::BoolXor => write!(f, "^^"),
            Self::BoolNot => write!(f, "!"),
            Self::BitShiftRight => write!(f, ">>"),
            Self::BitShiftLeft => write!(f, "<<"),
            Self::ErrChar(c, s) => {
                write!(f, "Unexpected character '{c}' while lexing {s}",)
            }
            Self::UnexpectedEOF(s) => write!(f, "Unexpected EOF while lexing {s}",),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}

impl Lexer {
    pub fn new(label: String, line: String) -> Lexer {
        Lexer {
            label: Rc::new(label),
            chars: CharIter::new(line),
        }
    }

    pub fn new_with_lineno(label: String, line: String, lineno: usize) -> Lexer {
        Lexer {
            label: Rc::new(label),
            chars: CharIter::new_with_lineno(line, lineno),
        }
    }

    pub fn from_lines<T: Iterator<Item = String>>(label: String, lines: T) -> Lexer {
        Lexer {
            label: Rc::new(label),
            chars: CharIter::from_lines(lines),
        }
    }

    #[allow(dead_code)]
    pub fn from_lines_with_lineno<T: Iterator<Item = String>>(
        label: String,
        lines: T,
        lineno: usize,
    ) -> Lexer {
        Lexer {
            label: Rc::new(label),
            chars: CharIter::from_lines_with_lineno(lines, lineno),
        }
    }

    #[allow(dead_code)]
    fn location(&self) -> Location {
        let borrowed: &CharIter = self.chars.borrow();
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
