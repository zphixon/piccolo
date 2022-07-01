//! The compile-time representation of Piccolo code.
//!
//! Opcode bytes are currently unstable. Operands are little-endian.
//!
//! Index means the index in the chunk's constant table, and slot means the
//! index from the bottom of the call frame on the [`Machine`] stack.
//!
//! | Opcode            | Operands                  |
//! |-------------------|---------------------------|
//! | `Pop`             |                           |
//! | `Return`          |                           |
//! | <b>Constants</b>  |                           |
//! | `Constant`        | index into constant table |
//! | `Nil`             |                           |
//! | `True`            |                           |
//! | `False`           |                           |
//! | <b>Math</b>       |                           |
//! | `Negate`          |                           |
//! | `Not`             |                           |
//! | `Add`             |                           |
//! | `Subtract`        |                           |
//! | `Multiply`        |                           |
//! | `Divide`          |                           |
//! | `Modulo`          |                           |
//! | <b>Comparison</b> |                           |
//! | `Equal`           |                           |
//! | `Greater`         |                           |
//! | `Less`            |                           |
//! | `GreaterEqual`    |                           |
//! | `LessEqual`       |                           |
//! | <b>Variables</b>  |                           |
//! | `GetLocal`        | slot on stack             |
//! | `SetLocal`        | slot on stack             |
//! | `GetGlobal`       | index into constant table |
//! | `SetGlobal`       | index into constant table |
//! | `DeclareGlobal`   | index into constant table |
//! | <b>Jumps</b>      |                           |
//! | `Jump`            | forward offset            |
//! | `JumpFalse`       | forward offset            |
//! | `JumpTrue`        | forward offset            |
//! | `JumpBack`        | backward offset           |
//! | <b>More math</b>  |                           |
//! | `BitAnd`          |                           |
//! | `BitOr`           |                           |
//! | `BitXor`          |                           |
//! | `ShiftLeft`       |                           |
//! | `ShiftRight`      |                           |
//! | <b>Misc</b>       |                           |
//! | `Call`            |                           |
//! | `Assert`          |                           |
//!
//! [`Machine`]: ../vm/struct.Machine.html

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Opcode {
    Pop,
    Return,

    Constant(u16),
    Nil,
    True,
    False,

    Negate,
    Not,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,

    Equal,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,

    GetLocal(u16),
    SetLocal(u16),
    GetGlobal(u16),
    SetGlobal(u16),
    DeclareGlobal(u16),

    JumpForward(u16),
    JumpFalse(u16),
    JumpTrue(u16),
    JumpBack(u16),

    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,

    Call(u16),

    Assert(u16),
}
