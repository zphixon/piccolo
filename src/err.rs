
// TODO: do something real with this

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
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
}

