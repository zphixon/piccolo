//! The compile-time representation of Piccolo code.
//!
//! Opcode bytes are currently unstable. Operands are little-endian.
//!
//! Index means the index in the chunk's constant table, and slot means the
//! index from the bottom of the call frame on the [`Machine`] stack.
//!
//! | Opcode            | Operands    | Byte   |
//! |-------------------|-------------|--------|
//! | `Pop`             |             | `0x00` |
//! | `Return`          |             | `0x01` |
//! |                   |             |        |
//! | `Constant`        | index       | `0x02` |
//! | `Nil`             |             | `0x03` |
//! | `True`            |             | `0x04` |
//! | `False`           |             | `0x05` |
//! |                   |             |        |
//! | `Negate`          |             | `0x06` |
//! | `Not`             |             | `0x07` |
//! | `Add`             |             | `0x08` |
//! | `Subtract`        |             | `0x09` |
//! | `Multiply`        |             | `0x0a` |
//! | `Divide`          |             | `0x0b` |
//! | `Modulo`          |             | `0x0c` |
//! |                   |             |        |
//! | `Equal`           |             | `0x0d` |
//! | `Greater`         |             | `0x0e` |
//! | `Less`            |             | `0x0f` |
//! | `GreaterEqual`    |             | `0x10` |
//! | `LessEqual`       |             | `0x11` |
//! |                   |             |        |
//! | `GetLocal`        | slot        | `0x12` |
//! | `SetLocal`        | slot        | `0x13` |
//! | `GetGlobal`       | index       | `0x14` |
//! | `SetGlobal`       | index       | `0x15` |
//! | `DeclareGlobal`   | index       | `0x16` |
//! |                   |             |        |
//! | `Assert`          |             | `0xff` |
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

    Jump            = 0x17,
    JumpFalse       = 0x18,

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
        | Opcode::Jump
        | Opcode::JumpFalse => 3,
        _ => 1,
    }
}
