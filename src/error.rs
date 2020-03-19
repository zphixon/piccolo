use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug)]
pub enum InterpretError {
    CompileError,
    RuntimeError,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", &self)
    }
}

impl Error for InterpretError {}
