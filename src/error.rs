use thiserror::Error;
use crate::op::Opcode;

#[derive(Error, Debug)]
pub enum PiccoloError {
    #[error("Multiple errors...{err}")]
    Lots {
        err: String,
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
    #[error("Invalid UTF8 on line {line}")]
    InvalidUTF8 {
        line: usize,
    },
    #[error("Unterminated string starting on line {line}")]
    UnterminatedString {
        line: usize,
    },
    #[error("Unknown format code '{code}' in string starting on line {line}")]
    UnknownFormatCode {
        code: char,
        line: usize,
    },
    #[error("Invalid number literal {literal} on line {line}")]
    InvalidNumberLiteral {
        line: usize,
        literal: String,
    }
}
