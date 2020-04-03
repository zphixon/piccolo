#![allow(non_snake_case)]

use downcast_rs::Downcast;
use slotmap::DefaultKey;
use slotmap::DenseSlotMap;

use std::fmt::{Debug, Display};

// TODO: get/set needs access to the interpreter's heap so it can allocate objects
/// Trait for Piccolo objects.
pub trait Object: Downcast + Debug + Display {
    fn type_name(&self) -> &'static str {
        "object"
    }

    fn gt(&self, _other: &dyn Object) -> Option<bool> {
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
}

downcast_rs::impl_downcast!(Object);

impl Object for String {
    fn type_name(&self) -> &'static str {
        "string"
    }

    fn gt(&self, other: &dyn Object) -> Option<bool> {
        other
            .downcast_ref::<String>()
            .map_or(None, |s| Some(self > s))
    }

    fn eq(&self, other: &dyn Object) -> Option<bool> {
        other
            .downcast_ref::<String>()
            .map_or(None, |s| Some(self == s))
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
}

/// Wrapper type for piccolo values.
#[derive(Debug, Clone)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Double(f64),
    Object(DefaultKey),
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
        std::convert::Into::<T>::into(self)
    }

    pub fn ref_string(&self) -> &String {
        match self {
            Value::String(s) => s,
            _ => panic!("tried to take reference to inner string - file a bug report!"),
        }
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

    pub fn type_name(&self, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Integer(_) => "integer",
            Value::Double(_) => "double",
            Value::Object(v) => map.get(*v).unwrap().type_name(),
            Value::Nil => "nil",
        }
    }

    pub fn fmt(&self, map: &DenseSlotMap<DefaultKey, Box<dyn Object>>) -> String {
        match self {
            Value::String(v) => format!("{}", v),
            Value::Bool(v) => format!("{}", v),
            Value::Integer(v) => format!("{}", v),
            Value::Double(v) => format!("{}", v),
            Value::Object(v) => format!("{}", map.get(*v).unwrap()),
            Value::Nil => String::new(),
        }
    }

    pub fn eq(
        &self,
        other: &Value,
        map: &DenseSlotMap<DefaultKey, Box<dyn Object>>,
    ) -> Option<bool> {
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
                Value::Object(r) => map.get(*l).unwrap().eq(map.get(*r).unwrap().as_ref()),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn gt(
        &self,
        other: &Value,
        map: &DenseSlotMap<DefaultKey, Box<dyn Object>>,
    ) -> Option<bool> {
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
            Value::Bool(l) => match other {
                Value::Bool(r) => Some(l > r),
                _ => None,
            },
            Value::Object(l) => match other {
                Value::Object(r) => map.get(*l).unwrap().gt(map.get(*r).unwrap().as_ref()),
                _ => None,
            },
            _ => None,
        }
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
