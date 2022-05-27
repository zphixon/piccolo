//! The compile-time representation of Piccolo code.
//!
//! Opcode bytes are currently unstable. Operands are little-endian.
//!
//! Index means the index in the chunk's constant table, and slot means the
//! index from the bottom of the call frame on the [`Machine`] stack.
//!
//! | Opcode            | Operands                  | Byte   |
//! |-------------------|---------------------------|--------|
//! | `Pop`             |                           | `0x00` |
//! | `Return`          |                           | `0x01` |
//! | <b>Constants</b>  |                           |        |
//! | `Constant`        | index into constant table | `0x02` |
//! | `Nil`             |                           | `0x03` |
//! | `True`            |                           | `0x04` |
//! | `False`           |                           | `0x05` |
//! | <b>Math</b>       |                           |        |
//! | `Negate`          |                           | `0x06` |
//! | `Not`             |                           | `0x07` |
//! | `Add`             |                           | `0x08` |
//! | `Subtract`        |                           | `0x09` |
//! | `Multiply`        |                           | `0x0a` |
//! | `Divide`          |                           | `0x0b` |
//! | `Modulo`          |                           | `0x0c` |
//! | <b>Comparison</b> |                           |        |
//! | `Equal`           |                           | `0x0d` |
//! | `Greater`         |                           | `0x0e` |
//! | `Less`            |                           | `0x0f` |
//! | `GreaterEqual`    |                           | `0x10` |
//! | `LessEqual`       |                           | `0x11` |
//! | <b>Variables</b>  |                           |        |
//! | `GetLocal`        | slot on stack             | `0x12` |
//! | `SetLocal`        | slot on stack             | `0x13` |
//! | `GetGlobal`       | index into constant table | `0x14` |
//! | `SetGlobal`       | index into constant table | `0x15` |
//! | `DeclareGlobal`   | index into constant table | `0x16` |
//! | <b>Jumps</b>      |                           |        |
//! | `Jump`            | forward offset            | `0x17` |
//! | `JumpFalse`       | forward offset            | `0x18` |
//! | `JumpTrue`        | forward offset            | `0x19` |
//! | `JumpBack`        | backward offset           | `0x1a` |
//! | <b>More math</b>  |                           |        |
//! | `BitAnd`          |                           | `0x1b` |
//! | `BitOr`           |                           | `0x1c` |
//! | `BitXor`          |                           | `0x1d` |
//! | `ShiftLeft`       |                           | `0x1e` |
//! | `ShiftRight`      |                           | `0x1f` |
//! | <b>Misc</b>       |                           |        |
//! | `Call`            |                           | `0x20` |
//! | `Assert`          |                           | `0xff` |
//!
//! [`Machine`]: ../vm/struct.Machine.html

macro_rules! opcodes {
    ($name:ident => $($op:ident = $num:expr,)*) => {
        /// Enum of Piccolo opcodes.
        #[derive(Debug, PartialEq, Clone, Copy)]
        #[repr(u8)]
        pub enum $name {
            $($op = $num,)*
        }

        impl Into<u8> for $name {
            fn into(self) -> u8 {
                match self {
                    $($name::$op => $num,)*
                }
            }
        }

        impl From<u8> for $name {
            fn from(v: u8) -> $name {
                match v {
                    $($num => $name::$op,)*
                    _ => panic!("{} does not correspond to any opcode in {}", v, stringify!($name))
                }
            }
        }
    };
}

opcodes!(Opcode =>
    Pop             = 0x00,
    Return          = 0x01,

    Constant        = 0x02,
    Nil             = 0x03,
    True            = 0x04,
    False           = 0x05,

    Negate          = 0x06,
    Not             = 0x07,
    Add             = 0x08,
    Subtract        = 0x09,
    Multiply        = 0x0a,
    Divide          = 0x0b,
    Modulo          = 0x0c,

    Equal           = 0x0d,
    Greater         = 0x0e,
    Less            = 0x0f,
    GreaterEqual    = 0x10,
    LessEqual       = 0x11,

    GetLocal        = 0x12,
    SetLocal        = 0x13,
    GetGlobal       = 0x14,
    SetGlobal       = 0x15,
    DeclareGlobal   = 0x16,

    JumpForward     = 0x17,
    JumpFalse       = 0x18,
    JumpTrue        = 0x19,
    JumpBack        = 0x1a,

    BitAnd          = 0x1b,
    BitOr           = 0x1c,
    BitXor          = 0x1d,
    ShiftLeft       = 0x1e,
    ShiftRight      = 0x1f,

    Call            = 0x20,

    Assert          = 0xff,
);

pub(crate) fn op_len(op: Opcode) -> usize {
    match op {
        Opcode::Constant
        | Opcode::GetLocal
        | Opcode::SetLocal
        | Opcode::GetGlobal
        | Opcode::SetGlobal
        | Opcode::DeclareGlobal
        | Opcode::JumpForward
        | Opcode::JumpFalse
        | Opcode::JumpTrue
        | Opcode::JumpBack
        | Opcode::Call
        | Opcode::Assert => 3,
        _ => 1,
    }
}
