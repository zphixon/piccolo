use crate::runtime::op::Opcode;

use core::fmt;

#[derive(Debug, Clone)]
pub struct PiccoloError {
    kind: ErrorKind,
    line: Option<usize>,
    file: Option<String>,
    msg: Option<String>,
}

impl PiccoloError {
    pub fn new(kind: ErrorKind) -> Self {
        PiccoloError {
            kind,
            line: None,
            file: None,
            msg: None,
        }
    }

    pub fn line(self, line: usize) -> Self {
        PiccoloError {
            line: Some(line),
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
}

impl fmt::Display for PiccoloError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}{}{}",
            if self.line.is_some() {
                format!("at line {} ", self.line.unwrap())
            } else {
                "".into()
            },
            if self.file.is_some() {
                format!("in file {} ", self.file.as_ref().unwrap())
            } else {
                "".into()
            },
            if self.line.is_some() || self.file.is_some() {
                "- "
            } else {
                ""
            },
            self.kind,
            if self.msg.is_some() {
                format!(" ({})", self.msg.as_ref().unwrap())
            } else {
                "".into()
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    StackUnderflow {
        op: Opcode,
    },
    InvalidUTF8,
    UnterminatedString,
    UnknownFormatCode {
        code: char,
    },
    InvalidNumberLiteral {
        literal: String,
    },
    UnexpectedToken {
        exp: String,
        got: String,
    },
    IncorrectType {
        exp: String,
        got: String,
        op: Opcode,
    },
    UndefinedVariable {
        name: String,
    },
    UnknownField {
        obj: String,
        name: String,
    },
    MalformedExpression {
        from: String,
    },
    ExpectedExpression {
        got: String,
    },
    CannotClone {
        ty: String,
    },
    AssertFailed,
    SyntaxError,
}

#[rustfmt::skip]
impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::StackUnderflow { op }
                => write!(f, "Stack underflow due to {:?}", op),
            ErrorKind::InvalidUTF8
                => write!(f, "Invalid UTF-8 sequence"),
            ErrorKind::UnterminatedString
                => write!(f, "Unterminated string"),
            ErrorKind::UnknownFormatCode { code }
                => write!(f, "Unknown format code '\\{}'", code),
            ErrorKind::InvalidNumberLiteral { literal }
                =>  write!(f, "Invalid number literal '{}'", literal) ,
            ErrorKind::UnexpectedToken { exp, got }
                => write!(f, "Unexpected token: expected {}, got {}", exp, got) ,
            ErrorKind::IncorrectType { exp, got, op }
                => write!(f, "Incorrect type: expected {}, got {} for op {:?}", exp, got, op),
            ErrorKind::UndefinedVariable { name }
                => write!(f, "Undefined variable '{}'", name),
            ErrorKind::UnknownField { obj, name }
                => write!(f, "Unknown field '{}' on {}", name, obj) ,
            ErrorKind::MalformedExpression { from }
                => write!(f, "Malformed expression from {}", from),
            ErrorKind::ExpectedExpression { got }
                => write!(f, "Expected expression, got {}", got),
            ErrorKind::CannotClone { ty }
                => write!(f, "Cannot clone type {}", ty),
            ErrorKind::AssertFailed
                => write!(f, "Assertion failed"),
            ErrorKind::SyntaxError
                => write!(f, "Syntax error"),
        }
    }
}
