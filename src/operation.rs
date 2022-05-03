use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Operation {
    Nop,
    Crash,
    Push,
    Pop,
    BoolOr,
    BoolXor,
    BoolAnd,
    BitOr,
    BitXor,
    BitAnd,
    CmpGE,
    CmpGT,
    CmpLE,
    CmpLT,
    CmpEq,
    CmpNotEq,
    BitShiftLeft,
    BitShiftRight,
    Minus,
    Plus,
    Times,
    Mod,
    Div,
    Equal,
    AndEq,
    XorEq,
    OrEq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    BitShiftRightEq,
    BitShiftLeftEq,
    BoolNot,
    BitNot,
    Inc,
    Dec,
}

impl Operation {
    pub fn is_binop(&self) -> bool {
        use Operation::*;
        match self {
            BoolOr => true,
            BoolXor => true,
            BoolAnd => true,
            BitOr => true,
            BitXor => true,
            BitAnd => true,
            CmpGE => true,
            CmpGT => true,
            CmpLE => true,
            CmpLT => true,
            CmpEq => true,
            CmpNotEq => true,
            BitShiftLeft => true,
            BitShiftRight => true,
            Minus => true,
            Plus => true,
            Times => true,
            Mod => true,
            Div => true,
            _ => false,
        }
    }

    pub fn is_assignop(&self) -> bool {
        use Operation::*;
        match self {
            Equal => true,
            AndEq => true,
            XorEq => true,
            OrEq => true,
            PlusEq => true,
            MinusEq => true,
            TimesEq => true,
            DivEq => true,
            ModEq => true,
            BitShiftRightEq => true,
            BitShiftLeftEq => true,
            _ => false,
        }
    }

    pub fn is_preunop(&self) -> bool {
        use Operation::*;
        match self {
            BoolNot => true,
            BitNot => true,
            Inc => true,
            Dec => true,
            _ => false,
        }
    }

    pub fn is_postunop(&self) -> bool {
        use Operation::*;
        match self {
            Inc => true,
            Dec => true,
            _ => false,
        }
    }

    pub fn is_machineop(&self) -> bool {
        use Operation::*;
        match self {
            Nop => true,
            Crash => true,
            Push => true,
            Pop => true,
            BoolOr => true,
            BoolXor => true,
            BoolAnd => true,
            BitOr => true,
            BitXor => true,
            BitAnd => true,
            CmpGE => true,
            CmpGT => true,
            CmpLE => true,
            CmpLT => true,
            CmpEq => true,
            CmpNotEq => true,
            BitShiftLeft => true,
            BitShiftRight => true,
            Minus => true,
            Plus => true,
            Times => true,
            Mod => true,
            Div => true,
            Equal => true,
            BoolNot => true,
            BitNot => true,
            Inc => true,
            Dec => true,
            _ => false,
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        use Operation::*;
        f.write_str(match self {
            Nop => "nop",
            Crash => "CRASH",
            Push => "push",
            Pop => "pop",
            BoolOr => "||",
            BoolXor => "^^",
            BoolAnd => "&&",
            BitOr => "|",
            BitXor => "^",
            BitAnd => "&",
            CmpGE => ">=",
            CmpGT => ">",
            CmpLE => "<",
            CmpLT => "<=",
            CmpEq => "==",
            CmpNotEq => "!=",
            BitShiftLeft => "<<",
            BitShiftRight => ">>",
            Minus => "-",
            Plus => "+",
            Times => "*",
            Mod => "%",
            Div => "/",
            Equal => "=",
            AndEq => "&=",
            XorEq => "^=",
            OrEq => "|=",
            PlusEq => "+=",
            MinusEq => "-=",
            TimesEq => "*=",
            DivEq => "/=",
            ModEq => "%=",
            BitShiftRightEq => ">>=",
            BitShiftLeftEq => "<<=",
            BoolNot => "!",
            BitNot => "~",
            Inc => "++",
            Dec => "--",
        })
    }
}
