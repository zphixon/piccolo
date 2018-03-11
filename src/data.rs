
use ::*;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq, Clone)]
pub struct Field {
    pub public: bool,
    pub normal: bool,
    pub value: value::Value,
}

impl Field {
    pub fn new(value: value::Value) -> Self {
        Field {
            public: true,
            normal: true,
            value
        }
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
    pub fn new(name: &str, fields: HashMap<String, Field>, methods: HashMap<String, Field>) -> Self {
        Data {
            name: name.to_owned(),
            fields,
            methods,
        }
    }

    pub fn is_public(&self, name: &str) -> bool {
        if let Some(f) = self.fields.get(name) {
            f.public
        } else {
            false
        }
    }

    pub fn get_method(&self, inst: Instance, name: &str) -> Option<value::Value> {
        //inst.all_public(); // panic
        //inst.reset();
        self.methods.get(name).cloned().map(|ok| match ok.value {
            value::Value::Func(f) => value::Value::Func(f.bind(inst)),
            _ => panic!("non-fn method")
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
            }))
        }
    }

    pub fn all_public(&self) {
        let mut inner = self.inner.borrow_mut();
        for var in inner.vars.values_mut() {
            var.public = true;
        }
    }

    pub fn reset(&self) {
        let mut inner = self.inner.borrow_mut();
        for var in inner.vars.values_mut() {
            var.public = var.normal;
        }
    }

    // TODO: move this to Result
    pub fn get(&self, name: &str, as_me: bool) -> Option<value::Value> {
        if self.inner.borrow().vars.contains_key(name) {
            let field = self.inner.borrow();
            let field = &field.vars[name];

            if !as_me {
            if field.public {
                Some(field.value.clone())
            } else {
                None
            }
            } else {
                Some(field.value.clone())
            }
        } else {
            let c = self.clone();
            //c.all_public(); // no panic, but it doesn't stay private afterward
            self.inner.borrow().data.get_method(c, name)
        }
    }

    pub fn set(&self, name: &str, value: value::Value) -> Result<(), ()> {
        let exists = {self.inner.borrow().vars.get(name).is_some()};
        if exists {
            let (public, normal) = {
                let inner = self.inner.borrow();
                let var = &inner.vars[name];
                (var.public, var.normal)
            };

            self.inner.borrow_mut().vars.insert(name.to_owned(), Field {
                value, public, normal,
            });

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

