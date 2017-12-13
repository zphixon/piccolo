
use ::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    inner: Vec<HashMap<String, value::Value>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: vec![HashMap::new()]
        }
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Option<HashMap<String, value::Value>> {
        self.inner.pop()
    }

    pub fn define(&mut self, name: &str, value: value::Value) {
        for scope in self.inner.iter_mut().rev().skip(1) {
            if scope.contains_key(name) {
                scope.insert(name.to_owned(), value);
                return
            }
        }

        self.inner.iter_mut().rev().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("env is empty");
    }

    pub fn get(&mut self, name: &token::Token) -> Option<value::Value> {
        for scope in self.inner.iter().rev() {
            if scope.contains_key(&name.lexeme) {
                return scope.get(&name.lexeme).cloned()
            }
        }
        None
    }
}

