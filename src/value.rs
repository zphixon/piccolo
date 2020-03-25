#![allow(non_snake_case)]

use core::fmt;
use std::cmp::Ordering;
use std::fmt::{Debug, Display, Formatter};

use broom::prelude::*;

impl Trace<Self> for Value {
    fn trace(&self, _tracer: &mut Tracer<Self>) {
        match self {
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
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

    pub fn into<T>(self) -> T where Value: Into<T> {
        std::convert::Into::<T>::into(self)
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
            Value::Nil => "nil",
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        if self.is_string() && rhs.is_string() {
            match self {
                Value::String(l) => match rhs {
                    Value::String(r) => return l.partial_cmp(r),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        if self.is_bool() && rhs.is_bool() {
            match self {
                Value::Bool(l) => match rhs {
                    Value::Bool(r) => return l.partial_cmp(r),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        if self.is_integer() && rhs.is_integer() {
            match self {
                Value::Integer(l) => match rhs {
                    Value::Integer(r) => return l.partial_cmp(r),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        if self.is_double() && rhs.is_double() {
            match self {
                Value::Double(l) => match rhs {
                    Value::Double(r) => return l.partial_cmp(r),
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            }
        }

        if self.is_nil() && rhs.is_nil() {
            return Some(Ordering::Equal);
        }

        None
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
    fn from(String: String) -> Self {
        Value::String(String)
    }
}

impl From<bool> for Value {
    fn from(Bool: bool) -> Self {
        Value::Bool(Bool)
    }
}

impl From<i64> for Value {
    fn from(Integer: i64) -> Self {
        Value::Integer(Integer)
    }
}

impl From<f64> for Value {
    fn from(Double: f64) -> Self {
        Value::Double(Double)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::String(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Double(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
        }
    }
}
