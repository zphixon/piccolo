
extern crate backtrace;

use ::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Default)]
struct EnvInner {
    inner: Vec<HashMap<String, value::Value>>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Env {
    inner: Rc<RefCell<EnvInner>>
    //pub inner: Vec<HashMap<String, Rc<RefCell<value::Value>>>>,
    //pub splits: Vec<usize>,
}

impl Env {
    pub fn new() -> Self {
        let inner = Rc::new(RefCell::new(EnvInner {
            inner: vec![HashMap::new()]
        }));

        Env { inner }
    }

    pub fn with_parent(parent: &Env) -> Self {
        let e = Env {
            inner: parent.inner.clone()
        };
        e.push();
        e
        //let inner = Rc::new(RefCell::new(EnvInner {
        //    inner: parent.inner.clone(),
        //}));
        //Env { inner }
        //let mut e = Env {
            //inner: parent.inner,
            ////splits: vec![]
        //};
        //e.push();
        //e
    }

    pub fn push(&self) {
        let mut inner = self.inner.borrow_mut();
        inner.inner.push(HashMap::new());
    }

    //pub fn push_parent(&mut self, parent: Env) {
    //    //self.splits.push(parent.inner.len());
    //    //let mut old = std::mem::replace(&mut self.inner, parent.inner);
    //    //self.inner.append(&mut old);
    //}

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
        //self.inner.borrow_mut().inner
        /*if name == "counter" {
            println!("found");
            backtrace::trace(|frame| {
                backtrace::resolve(frame.ip(), |symbol| {
                    print!("{:?}: ", frame.ip());
                    if let Some(ln) = symbol.lineno() {
                        print!("{} ", ln);
                    }
                    if let Some(name) = symbol.name() {
                        print!("{}", name);
                    } else {
                        print!("anon");
                    }
                    print!(" in ");
                    if let Some(filename) = symbol.filename() {
                        println!("{}", filename.display());
                    } else {
                        println!("anon");
                    }
                });
                true
            });
        }*/

        /*for scope in self.inner.iter_mut().rev().skip(1) {
            if scope.contains_key(name) {
                scope.insert(name.to_owned(), value);
                return
            }
        }

        self.inner.iter_mut().rev().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("env is empty");*/
    }

    pub fn get(&self, name: &str) -> Option<value::Value> {
        for scope in self.inner.borrow().inner.iter().rev() {
            if scope.contains_key(name) {
                if let Some(r) = scope.get(name) {
                    return Some(r.clone())
                }
                //let g: Option<&Rc<RefCell<value::Value>>> = scope.get(name);
                //return Rc::get_mut(scope.get(name))
            }
        }
        None
    }

    //pub fn set_local(&mut self, name: &str, value: Rc<RefCell<value::Value>>)  {
    //    //self.inner.iter_mut().rev().nth(0)
    //    //    .map(|m| m.insert(name.to_owned(), value))
    //    //    .expect("env is empty");
    //}

    //// TODO: re-visit closures in ch. 11
    //pub fn split(&mut self) -> Env {
    //    let split = self.splits.pop().expect("no closure");
    //    let inner = self.inner.iter().skip(split).cloned().collect();
    //    Env {
    //        inner,
    //        splits: Vec::new(),
    //    }
    //}

    //pub fn children(&self) -> Env {
    //    Env {
    //        inner: self.inner.iter().skip(1).cloned().collect(),
    //        splits: Vec::new(),
    //    }
    //}

    pub fn new_native_func(&self, name: &str, arity: func::Arity, func: func::NativeFuncType) {
        self.set(name, value::Value::Func(func::Func::new_native(name, arity, func::NativeFunc::new(func))));
    }
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::from("env:\n");
        //let mut s = format!("env: {:?}\n", self.splits);
        for (n, ctx) in self.inner.borrow().inner.iter().rev().enumerate() {
            s.push_str(&format!("  layer {}\n", n));
            for (k, v) in ctx.iter() {
                s.push_str(&format!("    {} = {}\n", k, v));
            }
        }
        write!(f, "{}", s)
    }
}

