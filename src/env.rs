
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

extern crate backtrace;

use ::*;

use std::collections::{HashMap, LinkedList};

#[derive(Clone, PartialEq, Debug)]
pub struct Scope(LinkedList<HashMap<String, value::Value>>);

impl Scope {
    pub fn new() -> Self {
        let mut ll = LinkedList::new();
        ll.push_front(HashMap::new());
        Scope(ll)
    }

    pub fn push(&mut self) {
        self.0.push_front(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.0.pop_front();
    }

    pub fn append(&self, other: &Scope) -> Scope {
        let mut this = self.0.clone();
        let mut a = other.0.clone();
        //this.prepend(&mut a);
        for item in a { this.push_front(item); }
        Scope(this)
    }

    pub fn set(&mut self, name: &str, value: value::Value) {
        for scope in self.0.iter_mut().skip(1) {
            if scope.contains_key(name) {
                scope.insert(name.to_owned(), value);
                return
            }
        }

        self.0.iter_mut().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("scope empty");
    }

    pub fn set_local(&mut self, name: &str, value: value::Value) {
        self.0.iter_mut().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("scope empty");
    }

    pub fn get(&self, name: &str) -> Option<value::Value> {
        for scope in self.0.iter() {
            if scope.contains_key(name) {
                return scope.get(name).cloned()
            }
        }

        None
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut value::Value> {
        for scope in self.0.iter_mut() {
            if scope.contains_key(name) {
                return scope.get_mut(name);
            }
        }

        None
    }

    pub fn delete(&mut self, name: &str) -> Option<value::Value> {
        for scope in self.0.iter_mut() {
            if scope.contains_key(name) {
                return scope.remove(name);
            }
        }

        None
    }

    pub fn new_native_func(&mut self, name: &str, arity: func::Arity, func: func::NativeFuncType) {
        self.set(name, value::Value::Func(func::Func::new_native(arity, func::NativeFunc::new(func))));
    }
}

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = String::from("env:\n");
        for (n, ctx) in self.0.iter().enumerate() {
            s.push_str(&format!("  layer {}\n", n));
            for (k, v) in ctx.iter() {
                s.push_str(&format!("    {} = {:?}\n", k, v));
            }
        }
        write!(f, "{}", s)
    }
}

//#[derive(Debug, Clone, PartialEq, Default)]
//pub struct Env {
    //pub inner: Vec<HashMap<String, value::Value>>,
    //pub me: Vec<data::Instance>
//}
//
//impl Env {
    //pub fn new() -> Self {
        //Env {
            //inner: vec![HashMap::new()],
            //me: vec![]
        //}
    //}
//
    //pub fn push_me(&mut self, me: data::Instance) {
        //if !self.me.contains(&me) {
            //self.me.push(me);
        //}
    //}
//
    //pub fn pop_me(&mut self) {
        //self.me.pop().expect("empty me");
    //}
//
    //pub fn latest_me(&self) -> Option<data::Instance> {
        //if self.me.is_empty() {
            //None
        //} else {
            //Some(self.me[self.me.len() - 1].clone())
        //}
    //}
//
    //pub fn push(&mut self) {
        //self.inner.push(HashMap::new());
    //}
//
    //pub fn pop(&mut self) {
        //self.inner.pop();
    //}
//
    //pub fn set(&mut self, name: &str, value: value::Value) {
        //for scope in self.inner.iter_mut().rev().skip(1) {
            //if scope.contains_key(name) {
                //scope.insert(name.to_owned(), value);
                //return
            //}
        //}
//
        //self.inner.iter_mut().rev().nth(0)
            //.map(|m| m.insert(name.to_owned(), value))
            //.expect("env is empty");
    //}
//
    //pub fn get(&self, name: &str) -> Option<value::Value> {
        //for scope in self.inner.iter().rev() {
            //if scope.contains_key(name) {
                //return scope.get(name).cloned()
            //}
        //}
        //None
    //}
//
    //pub fn get_mut(&mut self, name: &str) -> Option<&mut value::Value> {
        //for scope in self.inner.iter_mut().rev() {
            //if scope.contains_key(name) {
                //return scope.get_mut(name)
            //}
        //}
        //None
    //}
//
    //pub fn set_local(&mut self, name: &str, value: value::Value)  {
        //self.inner.iter_mut().rev().nth(0)
            //.map(|m| m.insert(name.to_owned(), value))
            //.expect("env is empty");
    //}
//
    //pub fn new_native_func(&mut self, name: &str, arity: func::Arity, func: func::NativeFuncType) {
        //self.set(name, value::Value::new(value::ValueKind::Func(func::Func::new_native(arity, func::NativeFunc::new(func)))));
    //}
//}
//
//impl std::fmt::Display for Env {
    //fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        //let mut s = String::from("env:\n  me:\n");
        //for inst in &self.me {
            //s.push_str(&format!("    {:?}\n", inst));
        //}
        //for (n, ctx) in self.inner.iter().rev().enumerate() {
            //s.push_str(&format!("  layer {}\n", n));
            //for (k, v) in ctx.iter() {
                //s.push_str(&format!("    {} = {:?}\n", k, v));
            //}
        //}
        //write!(f, "{}", s)
    //}
//}

