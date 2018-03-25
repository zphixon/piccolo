//use ::*;
use super::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct Field {
    pub value: value::Value,
}

impl Field {
    pub fn new(value: value::Value) -> Self {
        Field { value }
    }
}

impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Data {
    pub name: String,
    pub fields: HashMap<String, Field>,
    pub methods: HashMap<String, Field>,
}

impl Data {
    pub fn new(
        name: &str,
        fields: HashMap<String, Field>,
        methods: HashMap<String, Field>,
    ) -> Self {
        Data {
            name: name.to_owned(),
            fields,
            methods,
        }
    }

    pub fn get_method(&self, inst: Instance, name: &str) -> Option<value::Value> {
        self.methods.get(name).cloned().map(|ok| match ok.value {
            value::Value::Func(f) => value::Value::Func(f.bind(inst)),
            _ => panic!("non-fn method"),
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct InstanceInner {
    pub data: Rc<Data>,
    pub vars: HashMap<String, Field>,
}

#[derive(PartialEq, Clone)]
pub struct Instance {
    pub inner: Rc<RefCell<InstanceInner>>,
}

impl Instance {
    pub fn new(data: &Data, vars: HashMap<String, Field>) -> Self {
        Instance {
            inner: Rc::new(RefCell::new(InstanceInner {
                data: Rc::new(data.clone()),
                vars,
            })),
        }
    }

    pub fn get(&self, name: &str) -> Option<value::Value> {
        if self.inner.borrow().vars.contains_key(name) {
            let field = self.inner.borrow();
            let field = &field.vars[name];
            Some(field.value.clone())
        } else {
            let c = self.clone();
            self.inner.borrow().data.get_method(c, name)
        }
    }

    pub fn set(&self, name: &str, value: value::Value) -> Result<(), ()> {
        if self.inner.borrow().vars.contains_key(name) {
            self.inner
                .borrow_mut()
                .vars
                .insert(name.to_owned(), Field { value });
            Ok(())
        } else {
            Err(())
        }
    }
}

impl fmt::Debug for Instance {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", value::Value::Instance(self.clone()))
    }
}
