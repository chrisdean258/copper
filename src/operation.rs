use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Operation {
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
    Deref,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MachineOperation {
    Nop,
    Crash,
    ConditionalFail,
    Push,
    Pop,
    Dup,
    Store,
    StoreN,
    Alloc,
    Load,
    Rotate,
    Swap,
    Reserve,
    RefFrame,
    Jump,
    JumpRel,
    JumpIf,
    JumpRelIf,
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
    BoolNot,
    BitNot,
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
            Operation::BoolNot
                | Operation::BitNot
                | Operation::PreInc
                | Operation::PreDec
                | Operation::Deref
        )
    }

    pub fn is_postunop(&self) -> bool {
        matches!(self, Operation::PostInc | Operation::PostDec)
    }

    pub fn as_machine_op(&self) -> MachineOperation {
        macro_rules! do_conv {
            ($($token:tt),+ $(,)?) => {
                match self {
                    $(Operation::$token => MachineOperation::$token),+,
                    o => unreachable!("Trying to convert {:?} to a machine op", o),
                }
            };
        }
        do_conv! {
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
            BoolNot,
            BitNot,
        }
    }
}

impl Display for Operation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
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
            Operation::Deref => "*",
        })
    }
}

impl Display for MachineOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str(match self {
            MachineOperation::Nop => "nop",
            MachineOperation::Crash => "CRASH",
            MachineOperation::ConditionalFail => "ConditionalFail",
            MachineOperation::Push => "push",
            MachineOperation::Pop => "pop",
            MachineOperation::Dup => "dup",
            MachineOperation::Load => "load",
            MachineOperation::Store => "store",
            MachineOperation::StoreN => "storeN",
            MachineOperation::Alloc => "alloc",
            MachineOperation::Rotate => "rotate",
            MachineOperation::Swap => "swap",
            MachineOperation::Reserve => "reserve",
            MachineOperation::RefFrame => "refframe",
            MachineOperation::Jump => "jmp",
            MachineOperation::JumpIf => "jmpif",
            MachineOperation::JumpRel => "jmprel",
            MachineOperation::JumpRelIf => "jmprelif",
            MachineOperation::Call => "call",
            MachineOperation::Return => "ret",
            MachineOperation::BoolOr => "||",
            MachineOperation::BoolXor => "^^",
            MachineOperation::BoolAnd => "&&",
            MachineOperation::BitOr => "|",
            MachineOperation::BitXor => "^",
            MachineOperation::BitAnd => "&",
            MachineOperation::CmpGE => ">=",
            MachineOperation::CmpGT => ">",
            MachineOperation::CmpLE => "<",
            MachineOperation::CmpLT => "<=",
            MachineOperation::CmpEq => "==",
            MachineOperation::CmpNotEq => "!=",
            MachineOperation::BitShiftLeft => "<<",
            MachineOperation::BitShiftRight => ">>",
            MachineOperation::Minus => "-",
            MachineOperation::Plus => "+",
            MachineOperation::Times => "*",
            MachineOperation::Mod => "%",
            MachineOperation::Div => "/",
            MachineOperation::BoolNot => "!",
            MachineOperation::BitNot => "~",
        })
    }
}
