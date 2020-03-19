use thiserror::Error;
use std::fmt::{Display, Formatter};
use crate::op::Opcode;

#[derive(Error, Debug)]
pub enum InterpretError {
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

impl InterpretError {
    pub fn compile_error(line: usize) -> InterpretError {
        InterpretError::CompileError { line }
    }

    pub fn runtime_error(line: usize) -> InterpretError {
        InterpretError::RuntimeError { line }
    }

    pub fn stack_underflow(line: usize, op: Opcode) -> InterpretError {
        InterpretError::StackUnderflow { line, op }
    }
}
