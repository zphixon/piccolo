
use std::fmt;

pub fn parse_into_value(into: String) -> Value {
    match into.parse::<bool>() {
        Ok(b) => return Value::Bool(b),
        Err(_) => {},
    }

    match into.parse::<i64>() {
        Ok(i) => return Value::Integer(i),
        Err(_) => {},
    }

    match into.parse::<f64>() {
        Ok(f) => return Value::Float(f),
        Err(_) => {},
    }

    Value::String(into)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Nil,
}

impl<'a> From<&'a Value> for Value {
    fn from(v: &'a Value) -> Self {
        v.clone()
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Integer(i)
    }
}

impl From<u64> for Value {
    fn from(u: u64) -> Self {
        Value::Integer(u as i64)
    }
}

impl From<i32> for Value {
    fn from(i: i32) -> Self {
        Value::Integer(i as i64)
    }
}

impl From<u32> for Value {
    fn from(u: u32) -> Self {
        Value::Integer(u as i64)
    }
}

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<f32> for Value {
    fn from(f: f32) -> Self {
        Value::Float(f as f64)
    }
}

impl From<::expr::Literal> for Value {
    fn from(f: ::expr::Literal) -> Self {
        match f {
            ::expr::Literal::Float(v) => v.into(),
            ::expr::Literal::Integer(v) => v.into(),
            ::expr::Literal::Bool(v) => v.into(),
            ::expr::Literal::String(v) => v.into(),
            ::expr::Literal::Nil => Value::Nil,
            ::expr::Literal::Range => Value::Nil, // TODO
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Value::Bool(b) => write!(f, "{}", b),
            &Value::String(ref s) => write!(f, "{}", s),
            &Value::Float(fl) => write!(f, "{}", fl),
            &Value::Integer(i) => write!(f, "{}", i),
            &Value::Nil => write!(f, "nil"),
        }
    }
}
