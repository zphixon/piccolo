
use ::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    pub values: HashMap<String, value::Value>,
    pub parent: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            values: HashMap::new(),
            parent: None
        }
    }

    pub fn with_parent(parent: Env) -> Self {
        Env {
            values: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn define(&mut self, name: &str, value: value::Value) {
        if self.values.contains_key(name) {
            self.values.insert(name.to_owned(), value);
        } else {
            match self.parent {
                Some(ref mut p) => p.define(name, value),
                None => {self.values.insert(name.to_owned(), value);}
            }
        }
    }

    pub fn get(&mut self, name: &token::Token) -> Option<value::Value> {
        if self.values.contains_key(&name.lexeme) {
            self.values.get(&name.lexeme).cloned()
        } else {
            match self.parent {
                Some(ref mut p) => p.get(&name),
                None => None
            }
        }
    }
}

