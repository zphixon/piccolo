#[derive(Debug)]
pub struct PiccoloError {
    kind: ErrorKind,
    msg: String,
    line: u64,
    extra: Option<String>,
}

impl PiccoloError {
    pub fn new(kind: ErrorKind, msg: &str, line: u64) -> Self {
        PiccoloError {
            kind,
            line,
            msg: msg.to_owned(),
            extra: None,
        }
    }

    pub fn with_info(kind: ErrorKind, msg: &str, line: u64, extra: &str) -> Self {
        PiccoloError {
            kind,
            line,
            msg: msg.to_owned(),
            extra: Some(extra.to_owned()),
        }
    }

    // HACK
    // TODO - fix
    pub fn hack(msg: &str) -> Self {
        PiccoloError {
            kind: ErrorKind::Hack,
            line: 0,
            msg: msg.to_owned(),
            extra: None,
        }
    }
}

use std::fmt;
impl fmt::Display for PiccoloError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.kind == ErrorKind::Hack {
            return write!(f, "{}", self.msg);
        }

        let extra = if self.extra.is_some() {
            format!("\n{}", self.extra.as_ref().unwrap())
        } else {
            String::new()
        };

        write!(
            f,
            "Error, line {}: {:?} - {}{}",
            self.line, self.kind, self.msg, extra
        )
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum ErrorKind {
    Hack,
    SyntaxError,
    UnknownFormatCode,
    UnterminatedString,
    BadInteger,
    BadFloat,
    UnexpectedChar,
    UnexpectedToken,
    MathError,
    UndefinedVariable,
    IncorrectArity,
    NonFunction,
    NonInstance,
    NonData,
    NoSuchField,
    Unimplemented,
    NonIterator,
    IndexError,
}
