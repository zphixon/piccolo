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

//extern crate backtrace;

use super::*;

use std::collections::{HashMap, LinkedList};

#[derive(Clone, PartialEq, Debug)]
pub struct Scope {
    inner: LinkedList<HashMap<String, value::Value>>,
    split: Vec<usize>,
}

impl Scope {
    pub fn new() -> Self {
        let mut inner = LinkedList::new();
        inner.push_front(HashMap::new());
        Scope {
            inner,
            split: Vec::new(),
        }
    }

    pub fn push(&mut self) {
        self.inner.push_back(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.inner.pop_back().unwrap();
    }

    pub fn append(&mut self, mut other: Scope) -> Scope {
        let len = self.inner.len();
        self.inner.append(&mut other.inner);
        self.split.push(len);
        self.clone()
    }

    pub fn split(&mut self) -> Scope {
        let at = self.split.pop().unwrap();
        Scope {
            inner: self.inner.split_off(at),
            split: Vec::new(),
        }
    }

    pub fn set(&mut self, name: &str, value: value::Value) {
        for scope in self.inner.iter_mut().rev().skip(1) {
            if scope.contains_key(name) {
                scope.insert(name.to_owned(), value);
                return;
            }
        }

        self.inner
            .iter_mut()
            .rev()
            .nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("scope empty");
    }

    pub fn set_local(&mut self, name: &str, value: value::Value) {
        self.inner
            .iter_mut()
            .rev()
            .nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("scope empty");
    }

    pub fn get(&self, name: &str) -> Option<value::Value> {
        for scope in self.inner.iter().rev() {
            if scope.contains_key(name) {
                return scope.get(name).cloned();
            }
        }

        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut value::Value> {
        for scope in self.inner.iter_mut().rev() {
            if scope.contains_key(name) {
                return scope.get_mut(name);
            }
        }

        None
    }

    pub fn delete(&mut self, name: &str) -> Option<value::Value> {
        for scope in self.inner.iter_mut().rev() {
            if scope.contains_key(name) {
                return scope.remove(name);
            }
        }

        None
    }

    pub fn new_native_func(&mut self, name: &str, _arity: func::Arity, func: func::NativeFuncType) {
        self.set(
            name,
            value::Value::Foreign(foreign::ForeignOuter::new(func::ForeignFunc {
                inner: func,
            })),
        );
    }
}

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::from("env:\n");
        for (n, ctx) in self.inner.iter().enumerate() {
            s.push_str(&format!("  layer {}", n));
            if self.split.contains(&n) {
                s.push_str(" (split)");
            }
            s.push_str("\n");
            for (k, v) in ctx.iter() {
                s.push_str(&format!("    {} = {:?}\n", k, v));
            }
        }
        write!(f, "{}", s)
    }
}
