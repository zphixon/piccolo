
use std::fmt;

use ::*;

use std::rc::Rc;
use std::cell::RefCell;

pub fn parse_into_value(into: String) -> Value {
    if let Ok(b) = into.parse::<bool>() {
        return Value::Bool(b);
    }

    if let Ok(i) = into.parse::<i64>() {
        return Value::Integer(i);
    }

    if let Ok(f) = into.parse::<f64>() {
        return Value::Float(f);
    }

    Value::String(into)
}

#[derive(PartialEq, Clone)]
pub enum Value {
    String(String),
    Bool(bool),
    Integer(i64),
    Float(f64),
    Array(Vec<Rc<RefCell<Value>>>),
    Func(func::Func),
    Data(data::Data),
    Instance(data::Instance),
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

impl From<f64> for Value {
    fn from(f: f64) -> Self {
        Value::Float(f)
    }
}

impl From<Vec<Rc<RefCell<Value>>>> for Value {
    fn from(f: Vec<Rc<RefCell<Value>>>) -> Self {
        Value::Array(f)
    }
}

impl From<expr::Literal> for Value {
    fn from(f: expr::Literal) -> Self {
        match f {
            expr::Literal::Float(v) => Value::Float(v),
            expr::Literal::Integer(v) => Value::Integer(v),
            expr::Literal::Bool(v) => Value::Bool(v),
            expr::Literal::String(v) => Value::String(v),
            expr::Literal::Array(_) => panic!("unreachable: .into() on literal array"),
            expr::Literal::Nil => Value::Nil,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(ref v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Integer(v) => write!(f, "{}", v),
            Value::Array(ref v) => write!(f, "{:?}", v),
            Value::Func(ref v) => {
                if v.is_native() {
                    write!(f, "native fn {}", v.name)
                } else {
                    write!(f, "fn {}", v.name)
                }
            },
            Value::Data(ref _v) => write!(f, "todo: data"),
            Value::Instance(ref _v) => write!(f, "todo: instance"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Value::Bool(v) => write!(f, "(bool {})", v),
            Value::String(ref v) => write!(f, "(string \"{}\")", v),
            Value::Float(v) => write!(f, "(float {})", v),
            Value::Integer(v) => write!(f, "(int {})", v),
            Value::Array(ref v) => {
                let mut s = String::from("(arr");
                for item in v {
                    s.push_str(&format!(" {:?}", (*item).borrow()));
                }
                s.push_str(")");
                write!(f, "{}", s)
            },
            Value::Func(ref v) => {
                if v.is_native() {
                    write!(f, "(native fn {})", v.name)
                } else {
                    let mut s = format!("(fn {}", v.name);
                    for arg in &v.decl.as_ref().unwrap().args {
                        s.push_str(&format!(" {}", arg.lexeme))
                    }
                    s.push_str(")");
                    write!(f, "{}", s)
                }
            },
            Value::Data(ref v) => {
                write!(f, "(data {})", v.name)
            },
            Value::Instance(ref v) => {
                let mut s = format!("(instance of {}", v.data.name);
                for (k, v) in &v.vars {
                    s.push_str(&format!(" ({} = {:?})", k, v));
                }
                s.push_str(")");
                write!(f, "{}", s)
            },
            Value::Nil => write!(f, "(nil)")
        }
    }
}

pub fn is_truthy(e: &Value) -> bool {
    match *e {
        Value::Bool(b) => b,
        Value::Nil => false,
        _ => true,
    }
}

pub fn is_equal(lhs: &Value, rhs: &Value) -> bool {
    if lhs == &Value::Nil && rhs == &Value::Nil {
        true
    } else if lhs == &Value::Nil || rhs == &Value::Nil {
        false
    } else {
        lhs == rhs
    }
}

