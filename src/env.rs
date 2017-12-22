
//extern crate backtrace;
//backtrace::trace(|frame| {
//    backtrace::resolve(frame.ip(), |symbol| {
//        print!("{:?}: ", frame.ip());
//        if let Some(ln) = symbol.lineno() {
//            print!("{} ", ln);
//        }
//        if let Some(name) = symbol.name() {
//            print!("{}", name);
//        } else {
//            print!("anon");
//        }
//        print!(" in ");
//        if let Some(filename) = symbol.filename() {
//            println!("{}", filename.display());
//        } else {
//            println!("anon");
//        }
//    });
//    true
//});

use ::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct EnvInner {
    pub inner: Vec<HashMap<String, value::Value>>,
    pub me: Vec<data::Instance>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Env {
    pub inner: Rc<RefCell<EnvInner>>
}

impl Env {
    pub fn new() -> Self {
        let inner = Rc::new(RefCell::new(EnvInner {
            inner: vec![HashMap::new()],
            me: Vec::new()
        }));

        Env { inner }
    }

    pub fn push_me(&self, me: data::Instance) {
        let mut inner = self.inner.borrow_mut();
        if !inner.me.contains(&me) {
            inner.me.push(me);
        }
    }

    pub fn pop_me(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.me.pop().expect("empty me");
    }

    pub fn latest_me(&self) -> data::Instance {
        let inner = self.inner.borrow();
        inner.me[inner.me.len() - 1].clone()
    }

    pub fn push(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.inner.push(HashMap::new());
    }

    pub fn pop(&self) {
        self.inner.borrow_mut().inner.pop();
    }

    pub fn set(&self, name: &str, value: value::Value) {
        let mut inner = self.inner.borrow_mut();
        for scope in inner.inner.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_owned(), value);
                return
            }
        }

        inner.inner.iter_mut().rev().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("empty env");
    }

    pub fn set_local(&self, name: &str, value: value::Value) {
        let mut inner = self.inner.borrow_mut();

        inner.inner.iter_mut().rev().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("empty env");
    }

    pub fn get(&self, name: &str) -> Option<value::Value> {
        for scope in self.inner.borrow().inner.iter().rev() {
            if scope.contains_key(name) {
                if let Some(r) = scope.get(name) {
                    return Some(r.clone())
                }
            }
        }
        None
    }

    pub fn new_native_func(&self, name: &str, arity: func::Arity, func: func::NativeFuncType) {
        self.set(name, value::Value::Func(func::Func::new_native(arity, func::NativeFunc::new(func))));
    }
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::from("env:\n  me:\n");
        for inst in self.inner.borrow().me.iter() {
            s.push_str(&format!("    {:?}\n", inst));
        }
        for (n, ctx) in self.inner.borrow().inner.iter().rev().enumerate() {
            s.push_str(&format!("  layer {}\n", n));
            for (k, v) in ctx.iter() {
                s.push_str(&format!("    {} = {:?}\n", k, v));
            }
        }
        write!(f, "{}", s)
    }
}

