//! Contains types for working with Piccolo values.

use crate::{Function, Gc, Heap, NativeFunction, Object, PiccoloError, Token, TokenKind};

use core::fmt;

use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug)]
pub enum Value<'data> {
    Bool(bool),
    Integer(i64),
    Double(f64),
    String(Gc<'data, String>),
    Function(Gc<'data, Function>),
    NativeFunction(Gc<'data, NativeFunction>),
    Object(Gc<'data, dyn Object>),
    Nil,
}

impl<'data> Value<'data> {
    /// A value is only false-y if it is of type bool and false, or of type nil.
    /// All other values are truth-y.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn from_constant(c: Constant, h: &mut Heap<'data>) -> Value<'data> {
        match c {
            Constant::Bool(v) => Value::Bool(v),
            Constant::Integer(v) => Value::Integer(v),
            Constant::Double(v) => Value::Double(v),
            Constant::String(v) => Value::String({
                let root = h.manage(v);
                root.as_gc()
            }),
            Constant::Function(v) => Value::Function({
                let root = h.manage(v);
                root.as_gc()
            }),
            Constant::NativeFunction(v) => Value::NativeFunction({
                let root = h.manage(v);
                root.as_gc()
            }),
            Constant::Nil => Value::Nil,
        }
    }

    pub fn into_constant(self) -> Constant {
        match self {
            Value::Bool(v) => Constant::Bool(v),
            Value::Integer(v) => Constant::Integer(v),
            Value::Double(v) => Constant::Double(v),
            Value::String(v) => Constant::String(String::clone(&*v)),
            Value::Function(v) => Constant::Function(v.deep_copy()),
            Value::NativeFunction(v) => Constant::NativeFunction(v.deep_copy()),
            Value::Object(_) => todo!(),
            Value::Nil => Constant::Nil,
        }
    }

    pub fn into<T>(self) -> T
    where
        Value<'data>: Into<T>,
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

    pub fn is_function(&self) -> bool {
        matches!(self, Value::Function(_))
    }

    pub fn as_function(&self) -> Gc<'data, Function> {
        assert!(self.is_function());
        match self {
            Value::Function(f) => *f,
            _ => panic!(),
        }
    }

    pub fn is_native_function(&self) -> bool {
        matches!(self, Value::NativeFunction(_))
    }

    pub fn as_native_function(&self) -> Gc<'data, NativeFunction> {
        assert!(self.is_native_function());
        match self {
            Value::NativeFunction(f) => *f,
            _ => panic!(),
        }
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
                Value::String(r) => **l == **r,
                _ => None?,
            },
            Value::Function(l) => match other {
                Value::Function(r) => **l == **r,
                _ => None?,
            },
            Value::NativeFunction(l) => match other {
                Value::NativeFunction(r) => **l == **r,
                _ => None?,
            },
            Value::Nil => match other {
                Value::Nil => true,
                _ => None?,
            },
            _ => None?,
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

impl Object for Value<'_> {
    fn trace(&self) {
        match self {
            Value::Bool(v) => v.trace(),
            Value::Integer(v) => v.trace(),
            Value::Double(v) => v.trace(),
            Value::String(v) => v.trace(),
            Value::Function(v) => v.trace(),
            Value::NativeFunction(v) => v.trace(),
            Value::Object(v) => v.trace(),
            Value::Nil => {}
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(v) => v.type_name(),
            Value::Integer(v) => v.type_name(),
            Value::Double(v) => v.type_name(),
            Value::String(v) => v.type_name(),
            Value::Function(v) => v.type_name(),
            Value::NativeFunction(v) => v.type_name(),
            Value::Object(v) => v.type_name(),
            Value::Nil => "nil",
        }
    }
}

impl std::fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Double(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Function(v) => write!(f, "{}", v),
            Value::NativeFunction(v) => write!(f, "{}", v),
            Value::Object(v) => write!(f, "{}", v.format()),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Into<bool> for Value<'_> {
    fn into(self) -> bool {
        match self {
            Value::Bool(v) => v,
            _ => panic!("could not cast to bool"),
        }
    }
}

impl Into<i64> for Value<'_> {
    fn into(self) -> i64 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("could not cast to i64"),
        }
    }
}

impl Into<f64> for Value<'_> {
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
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Constant {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Function(Function),
    NativeFunction(NativeFunction),
    //Object(Box<dyn Object>), // TODO
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
            TokenKind::String => Constant::String(crate::compiler::escape_string(token)?),
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
            Constant::Function(v) => write!(f, "{}", v),
            Constant::NativeFunction(v) => write!(f, "{}", v),
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
