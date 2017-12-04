
use ::std::fmt;

use failure::Error;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnknownFormatCode,
    UnterminatedString,
    BadInteger,
    BadFloat,
    UnexpectedChar,
    UnexpectedToken,
}

#[derive(Debug, Fail)]
pub enum PiccoloError {
    #[fail(display = "Line {}: Unknown format code: {}", line, code)]
    UnknownFormatCode {
        line: u64,
        code: char
    },

    #[fail(display = "Line {}: Unterminated string", line)]
    UnterminatedString {
        line: u64,
    },

    #[fail(display = "Line {}: Bad integer", line)]
    BadInteger {
        line: u64
    },

    #[fail(display = "Line {}: Bad float", line)]
    BadFloat {
        line: u64
    },

    #[fail(display = "Line {}: Unexpected character: {}", line, character)]
    UnexpectedCharacter {
        line: u64,
        character: char
    }
}

