
use ::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    pub values: HashMap<String, value::Value>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            values: HashMap::new()
        }
    }

    pub fn define(&mut self, name: String, value: value::Value) {
        self.values.insert(name, value);
    }

    pub fn get(&mut self, name: &token::Token) -> Option<value::Value> {
        self.values.get(&name.lexeme).cloned()
    }
}

