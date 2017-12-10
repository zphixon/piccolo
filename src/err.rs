
// TODO: do something real with this

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnknownFormatCode,
    UnterminatedString,
    BadInteger,
    BadFloat,
    UnexpectedChar,
    UnexpectedToken,
    MathError,
    UndefinedVariable,
}

