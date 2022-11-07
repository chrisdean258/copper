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
    ErrChar(char, Option<&'static str>),
    UnexpectedEOF(Option<&'static str>),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use TokenType::*;
        match self {
            Identifier(s) => f.write_fmt(format_args!("{}", s)),
            LambdaArg(u) => f.write_fmt(format_args!("{}", u)),
            Str(s) => f.write_fmt(format_args!("\"{}\"", s)),
            Int(i) => f.write_fmt(format_args!("{}", i)),
            Float(d) => f.write_fmt(format_args!("{}", d)),
            Bool(u) => f.write_fmt(format_args!("{}", u)),
            Char(c) => f.write_fmt(format_args!("{}", c)),
            Import => f.write_str("import"),
            As => f.write_str("as"),
            From => f.write_str("from"),
            If => f.write_str("if"),
            Else => f.write_str("else"),
            And => f.write_str("and"),
            While => f.write_str("while"),
            For => f.write_str("for"),
            In => f.write_str("in"),
            Break => f.write_str("break"),
            Continue => f.write_str("continue"),
            Return => f.write_str("return"),
            Function => f.write_str("fn"),
            Lambda => f.write_str("\\"),
            Class => f.write_str("class"),
            Field => f.write_str("field"),
            Null => f.write_str("null"),
            OpenParen => f.write_str("("),
            CloseParen => f.write_str(")"),
            Comma => f.write_str(","),
            Dot => f.write_str("."),
            Equal => f.write_str("="),
            AndEq => f.write_str("&="),
            XorEq => f.write_str("^="),
            OrEq => f.write_str("|="),
            PlusEq => f.write_str("+="),
            MinusEq => f.write_str("-="),
            TimesEq => f.write_str("*="),
            DivEq => f.write_str("/="),
            ModEq => f.write_str("%="),
            Plus => f.write_str("+"),
            Minus => f.write_str("-"),
            Times => f.write_str("*"),
            Div => f.write_str("/"),
            Mod => f.write_str("%"),
            Inc => f.write_str("++"),
            Dec => f.write_str("--"),
            BitShiftRightEq => f.write_str(">>="),
            BitShiftLeftEq => f.write_str("<<="),
            Semicolon => f.write_str(";"),
            OpenBrace => f.write_str("{"),
            CloseBrace => f.write_str("}"),
            OpenBracket => f.write_str("["),
            CloseBracket => f.write_str("]"),
            CmpEq => f.write_str("=="),
            CmpNotEq => f.write_str("!="),
            CmpGE => f.write_str(">="),
            CmpGT => f.write_str(">"),
            CmpLE => f.write_str("<="),
            CmpLT => f.write_str("<"),
            BitOr => f.write_str("|"),
            BoolOr => f.write_str("||"),
            BitAnd => f.write_str("&"),
            BoolAnd => f.write_str("&&"),
            BitNot => f.write_str("~"),
            BitXor => f.write_str("^"),
            BoolXor => f.write_str("^^"),
            BoolNot => f.write_str("!"),
            BitShiftRight => f.write_str(">>"),
            BitShiftLeft => f.write_str("<<"),
            ErrChar(c, s) => f.write_fmt(format_args!(
                "Unexpected character '{c}'{}",
                s.map(|c| format!(" while lexing {c}")).unwrap_or_default()
            )),
            UnexpectedEOF(s) => f.write_fmt(format_args!(
                "Unexpected EOF{}",
                s.map(|c| format!(" while lexing {c}")).unwrap_or_default()
            )),
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
        match self.chars.peek().unwrap() {
            'a'..='z' => chars.push(self.chars.next().unwrap()),
            'A'..='Z' => chars.push(self.chars.next().unwrap()),
            '_' => chars.push(self.chars.next().unwrap()),
            _ => unreachable!(),
        }
        while let Some(c) = self.chars.peek() {
            match c {
                '0'..='9' => chars.push(c),
                'a'..='z' => chars.push(c),
                'A'..='Z' => chars.push(c),
                '_' => chars.push(c),
                _ => break,
            }
            self.chars.next();
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
            return TokenType::UnexpectedEOF(Some("char"));
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
        TokenType::UnexpectedEOF(Some("string"))
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

                _ => ErrChar(self.chars.next()?, None),
            },
        })
    }
}
