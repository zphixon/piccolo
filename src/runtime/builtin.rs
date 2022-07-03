use crate::{
    error::PiccoloError,
    runtime::{memory2::Heap, Arity, Value},
};
use std::fmt::Write;

pub fn print(heap: &mut Heap, values: &[Value]) -> Result<Value, PiccoloError> {
    let mut s = String::new();
    for (i, value) in values.iter().enumerate() {
        match value {
            Value::Nil => s.push_str("nil"),
            Value::String(_) => s.push_str("string todo"),
            Value::Bool(b) => write!(s, "{b}").unwrap(),
            Value::Double(d) => write!(s, "{d}").unwrap(),
            Value::Function(_) => write!(s, "<func>").unwrap(),
            Value::Object(ptr) => write!(s, "{}", heap.get(*ptr).unwrap().format(heap)).unwrap(),
        }
        if i != values.len() {
            s.push('\t');
        }
    }
    println!("{s}");
    Ok(Value::Nil)
}

pub fn rand(_: &mut Heap, _: &[Value]) -> Result<Value, PiccoloError> {
    Ok(Value::Double(rand::random()))
}

pub type PiccoloFunction = fn(&mut Heap, &[Value]) -> Result<Value, PiccoloError>;

pub struct NativeFunction {
    pub arity: Arity,
    pub ptr: PiccoloFunction,
}
