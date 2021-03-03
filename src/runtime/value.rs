//! Contains types for working with Piccolo values.

use crate::{PiccoloError, Token, TokenKind};

use super::object::ObjectPtr;

use core::fmt;

/// Wrapper type for runtime Piccolo values.
///
/// `Value::Object` is a pointer into a [`Heap`], and `Value::String` is a
/// pointer into an [`Interner`]. The end-user will never directly interact with this type,
/// instead values passed into and out of Piccolo will be [`Constant`]s.
///
/// [`Heap`]: ../memory/struct.Heap.html
/// [`Interner`]: ../memory/struct.Interner.html
/// [`Constant`]: ./enum.Constant.html
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(usize),
    Object(ObjectPtr),
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

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn is_bool(&self) -> bool {
        matches!(self, Value::Bool(_))
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Value::Integer(_))
    }

    pub fn is_double(&self) -> bool {
        matches!(self, Value::Double(_))
    }

    pub fn is_object(&self) -> bool {
        matches!(self, Value::Object(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
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
        matches!(self, Constant::String(_))
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
