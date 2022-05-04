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
    UnaryMinus,
    UnaryPlus,
    BoolNot,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

impl Operation {
    pub fn is_binop(&self) -> bool {
        match self {
            Operation::BoolOr => true,
            Operation::BoolXor => true,
            Operation::BoolAnd => true,
            Operation::BitOr => true,
            Operation::BitXor => true,
            Operation::BitAnd => true,
            Operation::CmpGE => true,
            Operation::CmpGT => true,
            Operation::CmpLE => true,
            Operation::CmpLT => true,
            Operation::CmpEq => true,
            Operation::CmpNotEq => true,
            Operation::BitShiftLeft => true,
            Operation::BitShiftRight => true,
            Operation::Minus => true,
            Operation::Plus => true,
            Operation::Times => true,
            Operation::Mod => true,
            Operation::Div => true,
            _ => false,
        }
    }

    pub fn is_assignop(&self) -> bool {
        match self {
            Operation::Equal => true,
            Operation::AndEq => true,
            Operation::XorEq => true,
            Operation::OrEq => true,
            Operation::PlusEq => true,
            Operation::MinusEq => true,
            Operation::TimesEq => true,
            Operation::DivEq => true,
            Operation::ModEq => true,
            Operation::BitShiftRightEq => true,
            Operation::BitShiftLeftEq => true,
            _ => false,
        }
    }

    pub fn is_preunop(&self) -> bool {
        match self {
            Operation::BoolNot => true,
            Operation::BitNot => true,
            Operation::PreInc => true,
            Operation::PreDec => true,
            _ => false,
        }
    }

    pub fn is_postunop(&self) -> bool {
        match self {
            Operation::PostInc => true,
            Operation::PostDec => true,
            _ => false,
        }
    }

    pub fn is_machineop(&self) -> bool {
        match self {
            Operation::Nop => true,
            Operation::Crash => true,
            Operation::Push => true,
            Operation::Pop => true,
            Operation::BoolOr => true,
            Operation::BoolXor => true,
            Operation::BoolAnd => true,
            Operation::BitOr => true,
            Operation::BitXor => true,
            Operation::BitAnd => true,
            Operation::CmpGE => true,
            Operation::CmpGT => true,
            Operation::CmpLE => true,
            Operation::CmpLT => true,
            Operation::CmpEq => true,
            Operation::CmpNotEq => true,
            Operation::BitShiftLeft => true,
            Operation::BitShiftRight => true,
            Operation::Minus => true,
            Operation::Plus => true,
            Operation::Times => true,
            Operation::Mod => true,
            Operation::Div => true,
            Operation::Equal => true,
            Operation::BoolNot => true,
            Operation::BitNot => true,
            _ => false,
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            Operation::Nop => "nop",
            Operation::Crash => "CRASH",
            Operation::Push => "push",
            Operation::Pop => "pop",
            Operation::BoolOr => "||",
            Operation::BoolXor => "^^",
            Operation::BoolAnd => "&&",
            Operation::BitOr => "|",
            Operation::BitXor => "^",
            Operation::BitAnd => "&",
            Operation::CmpGE => ">=",
            Operation::CmpGT => ">",
            Operation::CmpLE => "<",
            Operation::CmpLT => "<=",
            Operation::CmpEq => "==",
            Operation::CmpNotEq => "!=",
            Operation::BitShiftLeft => "<<",
            Operation::BitShiftRight => ">>",
            Operation::Minus => "-",
            Operation::Plus => "+",
            Operation::Times => "*",
            Operation::Mod => "%",
            Operation::Div => "/",
            Operation::Equal => "=",
            Operation::AndEq => "&=",
            Operation::XorEq => "^=",
            Operation::OrEq => "|=",
            Operation::PlusEq => "+=",
            Operation::MinusEq => "-=",
            Operation::TimesEq => "*=",
            Operation::DivEq => "/=",
            Operation::ModEq => "%=",
            Operation::BitShiftRightEq => ">>=",
            Operation::BitShiftLeftEq => "<<=",
            Operation::UnaryMinus => "-",
            Operation::UnaryPlus => "+",
            Operation::BoolNot => "!",
            Operation::BitNot => "~",
            Operation::PreInc => "++",
            Operation::PreDec => "--",
            Operation::PostInc => "++",
            Operation::PostDec => "--",
        })
    }
}
