//! Contains types for working with Piccolo values at runtime.

use crate::{PiccoloError, Token, TokenKind};

use core::fmt;

/// Wrapper type for runtime Piccolo values.
///
/// `Value::Object` is a pointer into a [`Heap`].
///
/// [`Heap`]: ../memory/struct.Heap.html
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    Object(usize),
    Nil,
}

impl Value {
    /// A value is only false-y if it is of type bool and false, or of type nil.
    /// All other values are truth-y.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn into<T>(self) -> T
    where
        Value: Into<T>,
    {
        std::convert::Into::into(self)
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Value::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            Value::Integer(_) => true,
            _ => false,
        }
    }

    pub fn is_double(&self) -> bool {
        match self {
            Value::Double(_) => true,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Value::Object(_) => true,
            _ => false,
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            _ => false,
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Double(v) => v,
            _ => panic!("could not cast to f64"),
        }
    }
}

/// Compile-time constant Piccolo values.
///
/// Similar to [`Value`]. `Constant` is also used to return from Piccolo execution.
///
/// [`Value`]: ../value/enum.Value.html
#[derive(Clone, Debug, PartialOrd, PartialEq)]
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Nil,
}

impl Constant {
    pub(crate) fn ref_string(&self) -> &str {
        match self {
            Constant::String(v) => v,
            _ => panic!("ref string on non-string"),
        }
    }

    pub(crate) fn try_from(token: Token) -> Result<Constant, PiccoloError> {
        Ok(match token.kind {
            TokenKind::String => Constant::String(crate::compiler::escape_string(&token)?),
            TokenKind::Integer(v) => Constant::Integer(v),
            TokenKind::True => Constant::Bool(true),
            TokenKind::False => Constant::Bool(false),
            TokenKind::Double(v) => Constant::Double(v),
            TokenKind::Nil => Constant::Nil,
            _ => panic!("cannot create value from token {:?}", token),
        })
    }

    /// Convert a `Constant` into its base type.
    pub fn into<T>(self) -> T
    where
        Constant: Into<T>,
    {
        core::convert::Into::<T>::into(self)
    }

    pub fn is_string(&self) -> bool {
        match self {
            Constant::String(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Constant::String(v) => write!(f, "{}", v),
            Constant::Bool(v) => write!(f, "{}", v),
            Constant::Integer(v) => write!(f, "{}", v),
            Constant::Double(v) => write!(f, "{}", v),
            Constant::Nil => write!(f, "nil"),
        }
    }
}

impl Into<bool> for Constant {
    fn into(self) -> bool {
        match self {
            Constant::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Constant {
    fn into(self) -> i64 {
        match self {
            Constant::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Constant {
    fn into(self) -> f64 {
        match self {
            Constant::Double(v) => v,
            _ => panic!("could not cast to f64"),
        }
    }
}
