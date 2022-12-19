use crate::value::Value;
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
    Extract,
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
    Negate,
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MachineOperation {
    Nop,
    Crash,
    ConditionalFail,
    Push(Value),
    Inplace(Value),
    Pop,
    Save,
    Dup,
    Store,
    FastStore,
    StoreN(usize),
    Alloc,
    Load,
    LoadAddr(usize),
    LoadLocal(isize),
    LoadN(usize),
    Rotate(usize),
    Swap,
    Cast(Value),
    Reserve(usize),
    RefFrame(isize),
    Jump(usize),
    JumpRel(isize),
    JumpIf(usize),
    JumpRelIf(isize),
    Call,
    CallBuiltin(usize),
    CallKnown(usize),
    CallBuiltinSize(usize, usize),
    CallKnownSize(usize, usize),
    ExitWith,
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
    Negate,
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
                | Operation::Extract
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
                | Operation::Negate
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
            Negate,
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
            Operation::CmpLE => "<=",
            Operation::CmpLT => "<",
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
            Operation::Extract => "<-",
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
            Operation::Negate => "-",
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

fn hex(val: isize) -> String {
    if val >= 0 {
        format!("+0x{:x}", val)
    } else {
        format!("-0x{:x}", -val)
    }
}

impl Display for MachineOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            MachineOperation::Nop => write!(f, "nop"),
            MachineOperation::Crash => write!(f, "CRASH"),
            MachineOperation::ConditionalFail => write!(f, "ConditionalFail"),
            MachineOperation::Push(v) => write!(f, "push ({v:?})"),
            MachineOperation::Inplace(v) => write!(f, "inplace ({v:?})"),
            MachineOperation::Pop => write!(f, "pop"),
            MachineOperation::Save => write!(f, "save"),
            MachineOperation::Dup => write!(f, "dup"),
            MachineOperation::Load => write!(f, "load"),
            MachineOperation::LoadAddr(ptr) => write!(f, "load (0x{ptr:x})"),
            MachineOperation::LoadLocal(n) => write!(f, "loadlocal ({n})"),
            MachineOperation::LoadN(n) => write!(f, "loadN ({n})"),
            MachineOperation::Store => write!(f, "store"),
            MachineOperation::FastStore => write!(f, "faststore"),
            MachineOperation::StoreN(n) => write!(f, "storeN ({n})"),
            MachineOperation::Alloc => write!(f, "alloc"),
            MachineOperation::Rotate(n) => write!(f, "rotate ({n})"),
            MachineOperation::Swap => write!(f, "swap"),
            MachineOperation::Cast(v) => write!(f, "cast ({v})"),
            MachineOperation::Reserve(n) => write!(f, "reserve ({n})"),
            MachineOperation::RefFrame(offset) => write!(f, "refframe ({})", hex(*offset)),
            MachineOperation::Jump(ip) => write!(f, "jmp (+0x{ip:x})"),
            MachineOperation::JumpIf(ip) => write!(f, "jmpif (+0x{ip:x})"),
            MachineOperation::JumpRel(offset) => write!(f, "jmprel ({})", hex(*offset)),
            MachineOperation::JumpRelIf(offset) => write!(f, "jmprelif ({})", hex(*offset)),
            MachineOperation::Call => write!(f, "call"),
            MachineOperation::CallBuiltin(addr) => write!(f, "callbuiltin (0x{addr:x})"),
            MachineOperation::CallKnown(addr) => write!(f, "callknown (0x{addr:x})"),
            MachineOperation::CallBuiltinSize(addr, num_args) => {
                write!(f, "callbuiltin (0x{:x}, {})", addr, num_args)
            }
            MachineOperation::CallKnownSize(addr, num_args) => {
                write!(f, "callknown (0x{:x}, {})", addr, num_args)
            }
            MachineOperation::ExitWith => write!(f, "exit_with"),
            MachineOperation::Return => write!(f, "ret"),
            MachineOperation::BoolOr => write!(f, "||"),
            MachineOperation::BoolXor => write!(f, "^^"),
            MachineOperation::BoolAnd => write!(f, "&&"),
            MachineOperation::BitOr => write!(f, "|"),
            MachineOperation::BitXor => write!(f, "^"),
            MachineOperation::BitAnd => write!(f, "&"),
            MachineOperation::CmpGE => write!(f, ">="),
            MachineOperation::CmpGT => write!(f, ">"),
            MachineOperation::CmpLE => write!(f, "<="),
            MachineOperation::CmpLT => write!(f, "<"),
            MachineOperation::CmpEq => write!(f, "=="),
            MachineOperation::CmpNotEq => write!(f, "!="),
            MachineOperation::BitShiftLeft => write!(f, "<<"),
            MachineOperation::BitShiftRight => write!(f, ">>"),
            MachineOperation::Minus => write!(f, "-"),
            MachineOperation::Plus => write!(f, "+"),
            MachineOperation::Times => write!(f, "*"),
            MachineOperation::Mod => write!(f, "%"),
            MachineOperation::Div => write!(f, "/"),
            MachineOperation::BoolNot => write!(f, "!"),
            MachineOperation::BitNot => write!(f, "~"),
            MachineOperation::Negate => write!(f, "-"),
        }
    }
}
