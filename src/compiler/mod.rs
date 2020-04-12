use crate::error::{ErrorKind, PiccoloError};
use crate::compiler::scanner::{Token, TokenKind};

pub mod emitter;
pub mod scanner;

#[allow(dead_code)]
pub(crate) fn escape_string(t: &Token) -> Result<String, PiccoloError> {
    match t.kind {
        TokenKind::String => {
            let s = t.lexeme;
            let mut value = Vec::new();
            let line_start = t.line;
            let mut line = line_start;

            let mut i = 1;
            while i < s.as_bytes().len() - 1 {
                let byte = s.as_bytes()[i];
                if byte == b'\n' {
                    line += 1;
                }

                if byte == b'\\' {
                    i += 1;
                    let byte = s.as_bytes()[i];
                    match byte {
                        b'n' => {
                            value.push(b'\n');
                        }
                        b'r' => {
                            value.push(b'\r');
                        }
                        b'\\' => {
                            value.push(b'\\');
                        }
                        b'"' => {
                            value.push(b'"');
                        }
                        b't' => {
                            value.push(b'\t');
                        }
                        b'\n' => {
                            while i < s.as_bytes().len() - 1
                                && crate::compiler::scanner::is_whitespace(s.as_bytes()[i])
                            {
                                i += 1;
                            }
                            i -= 1;
                        }
                        c => {
                            return Err(PiccoloError::new(ErrorKind::UnknownFormatCode {
                                code: c as char,
                            })
                                .line(line));
                        }
                    }
                } else {
                    value.push(byte);
                }

                i += 1;
            }

            Ok(String::from_utf8(value)
                .map_err(|_| PiccoloError::new(ErrorKind::InvalidUTF8).line(line))?)
        }
        _ => {
            panic!("Cannot escape string from token {:?}", t);
        }
    }
}
