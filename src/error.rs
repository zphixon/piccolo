//! Types for dealing with errors in scanning, parsing, compiling, or executing Piccolo.

use crate::compiler::SourcePos;
use std::{
    error::Error, fmt, fmt::Write, io::Error as IoError, str::Utf8Error, string::FromUtf8Error,
};

#[derive(Debug)]
pub struct Callsite {
    pub name: String,
    pub pos: SourcePos,
}

/// The main error-reporting struct.
#[derive(Debug)]
pub struct PiccoloError {
    kind: ErrorKind,
    pos: Option<SourcePos>,
    file: Option<String>,
    msg: Option<String>,
    stack: Option<Vec<Callsite>>,
}

impl PiccoloError {
    pub fn new(kind: ErrorKind) -> Self {
        PiccoloError {
            kind,
            pos: None,
            file: None,
            msg: None,
            stack: None,
        }
    }

    pub(crate) fn todo(why: String) -> Self {
        PiccoloError {
            kind: ErrorKind::Todo { why },
            pos: None,
            file: None,
            msg: None,
            stack: None,
        }
    }

    pub fn unknown(err: impl Into<Box<dyn Error>>) -> Self {
        PiccoloError {
            kind: ErrorKind::Unknown { err: err.into() },
            pos: None,
            file: None,
            msg: None,
            stack: None,
        }
    }

    pub fn pos(self, pos: SourcePos) -> Self {
        PiccoloError {
            pos: Some(pos),
            ..self
        }
    }

    pub fn file(self, file: String) -> Self {
        PiccoloError {
            file: Some(file),
            ..self
        }
    }

    pub fn msg(self, msg: &str) -> Self {
        PiccoloError {
            msg: Some(String::from(msg)),
            ..self
        }
    }

    pub fn msg_string(self, msg: String) -> Self {
        PiccoloError {
            msg: Some(msg),
            ..self
        }
    }

    pub fn stack_trace(self, stack: Vec<Callsite>) -> Self {
        PiccoloError {
            stack: Some(stack),
            ..self
        }
    }

    pub fn was_eof(&self) -> bool {
        match self.kind {
            ErrorKind::ExpectedExpression { was_eof, .. } => was_eof,
            ErrorKind::UnexpectedToken { was_eof, .. } => was_eof,
            _ => false,
        }
    }
}

impl fmt::Display for PiccoloError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{file}{flsep}{line}{separator}{kind}{msg}{nl}{stack_trace}",
            file = if self.file.is_some() {
                self.file.as_ref().unwrap()
            } else {
                ""
            },
            flsep = if self.file.is_some() && self.pos.is_some() {
                ":"
            } else {
                " "
            },
            line = if self.pos.is_some() {
                format!("{} ", self.pos.unwrap())
            } else {
                "".into()
            },
            separator = if self.pos.is_some() || self.file.is_some() {
                "- "
            } else {
                ""
            },
            kind = self.kind,
            msg = if self.msg.is_some() {
                format!(" ({})", self.msg.as_ref().unwrap())
            } else {
                "".into()
            },
            nl = if self.stack.is_some() && self.stack.as_ref().unwrap().is_empty() {
                "\n"
            } else {
                ""
            },
            stack_trace = if self.stack.is_some() && !self.stack.as_ref().unwrap().is_empty() {
                let mut s = String::new();
                for (i, site) in self.stack.as_ref().unwrap().iter().enumerate() {
                    write!(s, "  {} called from line {}", site.name, site.pos).unwrap();
                    if i + 1 != self.stack.as_ref().unwrap().len() {
                        s.push('\n');
                    }
                }
                s
            } else {
                "".into()
            }
        )
    }
}

impl Error for PiccoloError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            ErrorKind::InvalidUTF8(err) => Some(err),
            ErrorKind::IOError(err) => Some(err),
            ErrorKind::Unknown { err } => Some(err.as_ref()),
            _ => None,
        }
    }
}

/// Types of errors possible in Piccolo.
#[derive(Debug)]
pub enum ErrorKind {
    InvalidUTF8(Utf8Error),
    IOError(IoError),
    UnterminatedString,
    UnknownFormatCode {
        code: char,
    },
    InvalidNumberLiteral {
        literal: String,
    },
    UnexpectedToken {
        was_eof: bool,
        exp: String,
        got: String,
    },
    IncorrectType {
        exp: String,
        got: String,
    },
    InvalidArgument {
        exp: String,
        got: String,
    },
    CannotCompare {
        got: String,
        exp: String,
    },
    DivideByZero,
    InvalidShift {
        value: i64,
    },
    UndefinedVariable {
        name: String,
    },
    UnknownField {
        obj: String,
        name: String,
    },
    ExpectedExpression {
        was_eof: bool,
        got: String,
    },
    CannotClone {
        ty: String,
    },
    AssertFailed {
        assertion: String,
    },
    SyntaxError,
    IncorrectArity {
        name: String,
        exp: crate::runtime::Arity,
        got: usize,
    },
    CannotCall {
        callee: String,
    },
    CannotSet {
        object: String,
        property: String,
        value: String,
    },
    CannotGet {
        object: String,
        index: String,
    },
    FormatError,
    Todo {
        why: String,
    },
    Unknown {
        err: Box<dyn Error>,
    },
}

pub struct ParseError {}

#[rustfmt::skip]
impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::InvalidUTF8(_)
                => write!(f, "Invalid UTF-8 sequence"),
            ErrorKind::IOError(e)
                => write!(f, "IO error: {e}"),
            ErrorKind::UnterminatedString
                => write!(f, "Unterminated string"),
            ErrorKind::UnknownFormatCode { code }
                => write!(f, "Unknown format code '\\{code}'"),
            ErrorKind::InvalidNumberLiteral { literal }
                => write!(f, "Invalid number literal '{literal}'"),
            ErrorKind::UnexpectedToken { exp, got, .. }
                => write!(f, "Unexpected token: expected {exp}, got {got}"),
            ErrorKind::IncorrectType { exp, got }
                => write!(f, "Incorrect type: expected {exp}, got {got}"),
            ErrorKind::InvalidArgument { exp, got }
                => write!(f, "Invalid argument: got {got}, expected {exp}"),
            ErrorKind::CannotCompare { exp, got }
                => write!(f, "Cannot compare {exp} and {got}"),
            ErrorKind::DivideByZero
                => write!(f, "Cannot divide by zero"),
            ErrorKind::InvalidShift { value }
                => write!(f, "Cannot shift by {value}"),
            ErrorKind::UndefinedVariable { name }
                => write!(f, "Undefined variable '{name}'"),
            ErrorKind::UnknownField { obj, name }
                => write!(f, "Unknown field '{name}' on {obj}"),
            ErrorKind::ExpectedExpression { got, .. }
                => write!(f, "Expected expression, got {got}"),
            ErrorKind::CannotClone { ty }
                => write!(f, "Cannot clone type {ty}"),
            ErrorKind::AssertFailed { assertion }
                => write!(f, "Assertion failed: {assertion}"),
            ErrorKind::SyntaxError
                => write!(f, "Syntax error"),
            ErrorKind::IncorrectArity { name, exp, got }
                => write!(f, "Incorrect arity: function {name} expected {exp} argument(s), got {got}"),
            ErrorKind::CannotCall { callee }
                => write!(f, "Cannot call value {callee}"),
            ErrorKind::CannotSet { object, property, value }
                => write!(f, "Cannot set property {property} of {object} to value {value}"),
            ErrorKind::CannotGet { object, index }
                => write!(f, "Cannot get property {index} of {object}"),
            ErrorKind::FormatError
                => write!(f, "The value could not be formatted"),
            ErrorKind::Todo { why }
                => write!(f, "TODO: {why}"),
            ErrorKind::Unknown { err }
                => write!(f, "{}", err),
        }
    }
}

impl From<Utf8Error> for PiccoloError {
    fn from(e: Utf8Error) -> PiccoloError {
        PiccoloError::new(ErrorKind::InvalidUTF8(e))
            .msg_string(format!("valid up to {}", e.valid_up_to()))
    }
}

impl From<FromUtf8Error> for PiccoloError {
    fn from(e: FromUtf8Error) -> PiccoloError {
        PiccoloError::from(e.utf8_error())
    }
}

impl From<IoError> for PiccoloError {
    fn from(e: IoError) -> PiccoloError {
        PiccoloError::new(ErrorKind::IOError(e))
    }
}

impl From<PiccoloError> for Vec<PiccoloError> {
    fn from(e: PiccoloError) -> Vec<PiccoloError> {
        vec![e]
    }
}

impl From<std::fmt::Error> for PiccoloError {
    fn from(_: std::fmt::Error) -> Self {
        PiccoloError::new(ErrorKind::FormatError)
    }
}
