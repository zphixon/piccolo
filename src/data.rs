
use ::*;

#[derive(PartialEq, Clone, Debug)]
pub struct Data {
    pub name: String,
}

impl Data {
    pub fn new(name: &str) -> Self {
        Data {
            name: name.to_owned(),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Instance {
    pub data: Data,
    pub vars: std::collections::HashMap<String, value::Value>,
}

impl Instance {
    pub fn get(&mut self, name: &str) -> Option<value::Value> {
        if self.vars.contains_key(name) {
            self.vars.get(name).cloned()
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, value: value::Value) {
        self.vars.insert(name.to_owned(), value);
    }
}

