use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum Operation {
    Nop,
    Crash,
    ConditionalFail,
    Push,
    Pop,
    Store,
    StoreN,
    Load,
    Alloc,
    Reserve,
    Dup,
    Rotate,
    Swap,
    RefFrame,
    Jump,
    JumpIf,
    JumpRel,
    JumpRelIf,
    PrepCall,
    Call,
    Return,
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
        matches!(
            self,
            Operation::BoolOr
                | Operation::BoolXor
                | Operation::BoolAnd
                | Operation::BitOr
                | Operation::BitXor
                | Operation::BitAnd
                | Operation::CmpGE
                | Operation::CmpGT
                | Operation::CmpLE
                | Operation::CmpLT
                | Operation::CmpEq
                | Operation::CmpNotEq
                | Operation::BitShiftLeft
                | Operation::BitShiftRight
                | Operation::Minus
                | Operation::Plus
                | Operation::Times
                | Operation::Mod
                | Operation::Div
        )
    }

    pub fn is_assignop(&self) -> bool {
        matches!(
            self,
            Operation::Equal
                | Operation::AndEq
                | Operation::XorEq
                | Operation::OrEq
                | Operation::PlusEq
                | Operation::MinusEq
                | Operation::TimesEq
                | Operation::DivEq
                | Operation::ModEq
                | Operation::BitShiftRightEq
                | Operation::BitShiftLeftEq
        )
    }

    pub fn underlying_binop(&self) -> Operation {
        match self {
            Operation::AndEq => Operation::BitAnd,
            Operation::XorEq => Operation::BitXor,
            Operation::OrEq => Operation::BitOr,
            Operation::PlusEq => Operation::Plus,
            Operation::MinusEq => Operation::Minus,
            Operation::TimesEq => Operation::Times,
            Operation::DivEq => Operation::Div,
            Operation::ModEq => Operation::Mod,
            Operation::BitShiftRightEq => Operation::BitShiftRight,
            Operation::BitShiftLeftEq => Operation::BitShiftLeft,
            _ => unreachable!(),
        }
    }

    pub fn is_preunop(&self) -> bool {
        matches!(
            self,
            Operation::BoolNot | Operation::BitNot | Operation::PreInc | Operation::PreDec
        )
    }

    pub fn is_postunop(&self) -> bool {
        matches!(self, Operation::PostInc | Operation::PostDec)
    }

    pub fn is_machineop(&self) -> bool {
        matches!(
            self,
            Operation::Nop
                | Operation::Crash
                | Operation::ConditionalFail
                | Operation::Push
                | Operation::Pop
                | Operation::Dup
                | Operation::Store
                | Operation::StoreN
                | Operation::Alloc
                | Operation::Load
                | Operation::Rotate
                | Operation::Swap
                | Operation::Reserve
                | Operation::RefFrame
                | Operation::Jump
                | Operation::JumpRel
                | Operation::JumpIf
                | Operation::JumpRelIf
                | Operation::Call
                | Operation::PrepCall
                | Operation::Return
                | Operation::BoolOr
                | Operation::BoolXor
                | Operation::BoolAnd
                | Operation::BitOr
                | Operation::BitXor
                | Operation::BitAnd
                | Operation::CmpGE
                | Operation::CmpGT
                | Operation::CmpLE
                | Operation::CmpLT
                | Operation::CmpEq
                | Operation::CmpNotEq
                | Operation::BitShiftLeft
                | Operation::BitShiftRight
                | Operation::Minus
                | Operation::Plus
                | Operation::Times
                | Operation::Mod
                | Operation::Div
                | Operation::Equal
                | Operation::BoolNot
                | Operation::BitNot
        )
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            Operation::Nop => "nop",
            Operation::Crash => "CRASH",
            Operation::ConditionalFail => "ConditionalFail",
            Operation::Push => "push",
            Operation::Pop => "pop",
            Operation::Dup => "dup",
            Operation::Load => "load",
            Operation::Store => "store",
            Operation::StoreN => "storeN",
            Operation::Alloc => "alloc",
            Operation::Rotate => "rotate",
            Operation::Swap => "swap",
            Operation::Reserve => "reserve",
            Operation::RefFrame => "refframe",
            Operation::Jump => "jmp",
            Operation::JumpIf => "jmpif",
            Operation::JumpRel => "jmprel",
            Operation::JumpRelIf => "jmprelif",
            Operation::Call => "call",
            Operation::PrepCall => "prepcall",
            Operation::Return => "ret",
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
