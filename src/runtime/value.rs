//! Contains types for working with Piccolo values.

use crate::{Function, Object, PiccoloError, Token, TokenKind};

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
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(String),
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

    pub fn to_constant(&self) -> Constant {
        match self {
            Value::Bool(v) => Constant::Bool(*v),
            Value::Integer(v) => Constant::Integer(*v),
            Value::Double(v) => Constant::Double(*v),
            Value::String(v) => Constant::String(v.clone()),
            Value::Nil => Constant::Nil,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::String(_) => "string",
            Value::Nil => "nil",
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

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    pub fn eq(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Bool(l) => match other {
                Value::Bool(r) => l == r,
                _ => None?,
            },
            Value::Integer(l) => match other {
                Value::Integer(r) => l == r,
                Value::Double(r) => *l as f64 == *r,
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l == *r as f64,
                Value::Double(r) => l == r,
                _ => None?,
            },
            Value::String(l) => match other {
                Value::String(r) => l == r,
                _ => None?,
            },
            Value::Nil => match other {
                Value::Nil => true,
                _ => None?,
            },
        })
    }

    pub fn gt(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => l > r,
                Value::Double(r) => *l as f64 > *r,
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l > *r as f64,
                Value::Double(r) => l > r,
                _ => None?,
            },
            _ => None?,
        })
    }

    pub fn lt(&self, other: &Value) -> Option<bool> {
        Some(match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => l < r,
                Value::Double(r) => (*l as f64) < (*r),
                _ => None?,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => *l < *r as f64,
                Value::Double(r) => l < r,
                _ => None?,
            },
            _ => None?,
        })
    }
}

impl Object for Value {
    fn trace(&self) {
        match self {
            Value::Bool(_) => {}
            Value::Integer(_) => {}
            Value::Double(_) => {}
            Value::String(v) => v.trace(),
            Value::Nil => {}
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Double(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
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
#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Function(Function),
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

    pub fn to_value(&self) -> Value {
        match self {
            Constant::Bool(v) => Value::Bool(*v),
            Constant::Integer(v) => Value::Integer(*v),
            Constant::Double(v) => Value::Double(*v),
            Constant::String(v) => Value::String(v.clone()),
            Constant::Function(_) => todo!(),
            Constant::Nil => Value::Nil,
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
            Constant::Function(_) => todo!(),
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
