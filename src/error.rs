use thiserror::Error;
use std::fmt::{Display, Formatter};
use crate::op::Opcode;

#[derive(Error, Debug)]
pub enum PiccoloError {
    #[error("Unidentified tokens:\n{tokens}")]
    UnidentifiedToken {
        tokens: String,
    },
    #[error("Compile error at line {line}")]
    CompileError {
        line: usize,
    },
    #[error("Runtime error at line {line}")]
    RuntimeError {
        line: usize,
    },
    #[error("Stack underflow - file a bug report! line {line}, {op:?}")]
    StackUnderflow {
        line: usize,
        op: Opcode,
    },
}
