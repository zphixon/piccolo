
// TODO: do something real with this

use ::std::fmt;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnknownFormatCode,
    UnterminatedString,
    BadInteger,
    BadFloat,
    UnexpectedChar,
    UnexpectedToken,
}

