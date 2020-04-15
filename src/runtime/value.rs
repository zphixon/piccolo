use downcast_rs::Downcast;

use core::fmt;

use crate::{Token, TokenKind};

/// Trait for Piccolo objects.
pub trait Object: Downcast + fmt::Debug + fmt::Display {
    fn type_name(&self) -> &'static str {
        "object"
    }

    fn gt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn lt(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn eq(&self, _other: &dyn Object) -> Option<bool> {
        None
    }

    fn get(&self, _property: &str) -> Option<Value> {
        None
    }

    fn set(&mut self, _property: &str) -> Option<Value> {
        None
    }

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
        core::convert::Into::<T>::into(self)
    }

    pub fn ref_string(&self) -> &String {
        match self {
            Value::String(s) => s,
            _ => panic!("tried to take reference to inner string - file a bug report!"),
        }
    }

    pub fn try_clone(&self) -> Value {
        match self {
            Value::String(string) => Value::String(string.clone()),
            Value::Object(o) => Value::Object(
                o.try_clone()
                    .ok_or_else(|| panic!("cannot clone {}", o.type_name()))
                    .unwrap(),
            ),
            Value::Bool(bool) => Value::Bool(*bool),
            Value::Integer(i64) => Value::Integer(*i64),
            Value::Double(f64) => Value::Double(*f64),
            Value::Nil => Value::Nil,
        }
    }

    pub(crate) fn try_from(token: Token) -> Option<Value> {
        Some(match token.kind {
            TokenKind::Integer(v) => Value::Integer(v),
            TokenKind::True => Value::Bool(true),
            TokenKind::False => Value::Bool(false),
            TokenKind::Double(v) => Value::Double(v),
            TokenKind::String => Value::String(crate::compiler::escape_string(&token).ok()?),
            _ => None?,
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

    pub fn fmt(&self) -> String {
        match self {
            Value::String(v) => v.to_string(),
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", v),
            Value::Nil => String::new(),
        }
    }

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

impl PartialEq for Value {
    fn eq(&self, rhs: &Value) -> bool {
        Value::eq(self, rhs).unwrap()
    }
}

impl Into<String> for Value {
    fn into(self) -> String {
        match self {
            Value::String(t) => t,
            _ => panic!("could not cast {:?} to string", self),
        }
    }
}

impl Into<bool> for Value {
    fn into(self) -> bool {
        match self {
            Value::Bool(t) => t,
            _ => panic!("could not cast {:?} to bool", self),
        }
    }
}

impl Into<i64> for Value {
    fn into(self) -> i64 {
        match self {
            Value::Integer(t) => t,
            _ => panic!("could not cast {:?} to i64", self),
        }
    }
}

impl Into<f64> for Value {
    fn into(self) -> f64 {
        match self {
            Value::Double(t) => t,
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
