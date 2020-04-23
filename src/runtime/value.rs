use downcast_rs::Downcast;

use core::fmt;

use crate::{Token, TokenKind, PiccoloError};

/// Trait for Piccolo objects.
pub trait Object: Downcast + fmt::Debug + fmt::Display {
    /// Return the name of the type of the object. Used for runtime type comparison inside
    /// Piccolo via the `type()` builtin function.
    fn type_name(&self) -> &'static str {
        "object"
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn gt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn lt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Compare self to another object. Returns `None` if incomparable.
    fn eq(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    /// Get the named property from the object. Returns `None` if it doesn't exist.
    fn get(&self, _property: &str) -> Option<Value> {
        None
    }

    /// Sets the named property on the object. Returns `None` if it doesn't exist.
    fn set(&mut self, _property: &str) -> Option<Value> {
        None
    }

    /// Attempts to clone the object. Returns `None` if it is not possible.
    fn try_clone(&self) -> Option<Box<dyn Object>> {
        None
    }
}

downcast_rs::impl_downcast!(Object);

impl Object for String {
    fn type_name(&self) -> &'static str {
        "string"
    }

    fn gt(&self, other: &dyn Object) -> Option<bool> {
        other.downcast_ref::<String>().map(|s| self > s)
    }

    fn lt(&self, other: &dyn Object) -> Option<bool> {
        other.downcast_ref::<String>().map(|s| self < s)
    }

    fn eq(&self, other: &dyn Object) -> Option<bool> {
        other.downcast_ref::<String>().map(|s| self == s)
    }

    fn get(&self, property: &str) -> Option<Value> {
        match property {
            "len" => Some(Value::Integer(self.len() as i64)),
            _ => None,
        }
    }

    fn set(&mut self, _property: &str) -> Option<Value> {
        None
    }

    fn try_clone(&self) -> Option<Box<dyn Object>> {
        Some(Box::new(self.clone()))
    }
}

/// Wrapper type for piccolo values.
#[derive(Debug)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Object(Box<dyn Object>),
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

    /// Converts the value into a type T for which Value implements Into<T>.
    pub fn into<T>(self) -> T
    where
        Value: Into<T>,
    {
        core::convert::Into::<T>::into(self)
    }

    /// Takes a reference to the inner string. Panics if the value is not a string.
    pub fn ref_string(&self) -> &String {
        match self {
            Value::String(s) => s,
            _ => panic!("tried to take reference to inner string - file a bug report!"),
        }
    }

    /// Attempts to clone a value. Panics if it doesn't succeed.
    pub fn try_clone(&self) -> Option<Value> {
        Some(match self {
            Value::String(v) => Value::String(v.clone()),
            Value::Object(v) => Value::Object(v.try_clone()?),
            Value::Bool(v) => Value::Bool(*v),
            Value::Integer(v) => Value::Integer(*v),
            Value::Double(v) => Value::Double(*v),
            Value::Nil => Value::Nil,
        })
    }

    pub(crate) fn try_from(token: Token) -> Result<Value, PiccoloError> {
        Ok(match token.kind {
            TokenKind::Integer(v) => Value::Integer(v),
            TokenKind::True => Value::Bool(true),
            TokenKind::False => Value::Bool(false),
            TokenKind::Double(v) => Value::Double(v),
            TokenKind::String => Value::String(crate::compiler::escape_string(&token)?),
            TokenKind::Nil => Value::Nil,
            _ => panic!("cannot create value from token {:?}", token)
        })
    }

    pub fn is_string(&self) -> bool {
        match self {
            Value::String(_) => true,
            _ => false,
        }
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

    /// Returns the type name of a value.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(v) => v.type_name(),
            Value::Nil => "nil",
        }
    }

    /// Formats the value.
    pub fn fmt(&self) -> String {
        match self {
            Value::String(v) => v.to_string(),
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", v),
            Value::Nil => "nil".into(),
        }
    }

    /// Tests a value for equality. Returns `None` if incomparable.
    pub fn eq(&self, other: &Value) -> Option<bool> {
        match self {
            Value::String(l) => match other {
                Value::String(r) => Some(l == r),
                _ => None,
            },
            Value::Bool(l) => match other {
                Value::Bool(r) => Some(l == r),
                _ => None,
            },
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l == r),
                Value::Double(r) => Some(*l as f64 == *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l == *r as f64),
                Value::Double(r) => Some(l == r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => l.eq(r.as_ref()),
                _ => None,
            },
            Value::Nil => match other {
                Value::Nil => Some(true),
                _ => Some(false),
            },
        }
    }

    pub fn lt(&self, other: &Value) -> Option<bool> {
        match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l < r),
                Value::Double(r) => Some((*l as f64) < *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l < *r as f64),
                Value::Double(r) => Some(l < r),
                _ => None,
            },
            Value::String(l) => match other {
                Value::String(r) => Some(l < r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => l.lt(r.as_ref()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn gt(&self, other: &Value) -> Option<bool> {
        match self {
            Value::Integer(l) => match other {
                Value::Integer(r) => Some(l > r),
                Value::Double(r) => Some(*l as f64 > *r),
                _ => None,
            },
            Value::Double(l) => match other {
                Value::Integer(r) => Some(*l > *r as f64),
                Value::Double(r) => Some(l > r),
                _ => None,
            },
            Value::String(l) => match other {
                Value::String(r) => Some(l > r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => l.gt(r.as_ref()),
                _ => None,
            },
            _ => None,
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Value {
        self.try_clone().unwrap()
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Value::fmt(self))
    }
}

impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        Value::eq(self, rhs).unwrap()
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        match self {
            Value::String(v) => v,
            _ => panic!("could not cast {:?} to string", self),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Bool(v) => v,
            _ => panic!("could not cast {:?} to bool", self),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::Integer(v) => v,
            _ => panic!("could not cast {:?} to i64", self),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Double(v) => v,
            _ => panic!("could not cast {:?} to f64", self),
        }
    }
}

impl From<String> for Value {
    fn from(v: String) -> Self {
        Value::String(v)
    }
}

impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Integer(v)
    }
}

impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Double(v)
    }
}
