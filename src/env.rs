
extern crate backtrace;

use ::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Env {
    pub inner: Vec<HashMap<String, value::Value>>,
    pub splits: Vec<usize>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            inner: vec![HashMap::new()],
            splits: vec![]
        }
    }

    pub fn with_parent(parent: Env) -> Self {
        let mut e = Env {
            inner: parent.inner,
            splits: vec![]
        };
        e.push();
        e
    }

    pub fn push(&mut self) {
        self.inner.push(HashMap::new());
    }

    pub fn push_parent(&mut self, parent: Env) {
        self.splits.push(parent.inner.len());
        let mut old = std::mem::replace(&mut self.inner, parent.inner);
        self.inner.append(&mut old);
    }

    pub fn pop(&mut self) -> Option<HashMap<String, value::Value>> {
        self.inner.pop()
    }

    pub fn define(&mut self, name: &str, value: value::Value) {
        if name == "counter" {
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
        }

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

    pub fn get(&self, name: &str) -> Option<value::Value> {
        for scope in self.inner.iter().rev() {
            if scope.contains_key(name) {
                return scope.get(name).cloned()
            }
        }
        None
    }

    pub fn set_local(&mut self, name: &str, value: value::Value)  {
        self.inner.iter_mut().rev().nth(0)
            .map(|m| m.insert(name.to_owned(), value))
            .expect("env is empty");
    }

    // TODO: re-visit closures in ch. 11
    pub fn split(&mut self) -> Env {
        let split = self.splits.pop().expect("no closure");
        let inner = self.inner.iter().skip(split).cloned().collect();
        Env {
            inner,
            splits: Vec::new(),
        }
    }

    pub fn children(&self) -> Env {
        Env {
            inner: self.inner.iter().skip(1).cloned().collect(),
            splits: Vec::new(),
        }
    }
}

impl std::fmt::Display for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = format!("env: {:?}\n", self.splits);
        for (n, ctx) in self.inner.iter().rev().enumerate() {
            s.push_str(&format!("  layer {}\n", n));
            'inner: for (k, v) in ctx.iter() {
                s.push_str(&format!("    {} = ", k));
                match *v {
                    value::Value::Func(ref f) => {
                        s.push_str(&format!("fn {} ", f.name));
                        if f.is_native() {
                            s.push_str("(native)\n");
                            continue 'inner;
                        } else {
                            for arg in &f.decl.as_ref().unwrap().args {
                                s.push_str(&format!("{}, ", arg.lexeme));
                            }
                            s.push_str("\n");
                            //s.push_str("\n      closure:\n");
                            //for item in &f.closure.inner {
                            //    s.push_str(&format!("        {:?}\n", item));
                            //}
                            for stmt in &f.decl.as_ref().unwrap().body {
                                s.push_str(&format!("      {}\n", AstPrinter.print_stmt(stmt)))
                            }
                        }
                    }
                    _ => s.push_str(&format!("{:?}\n", v))
                }
            }
        }
        write!(f, "{}", s)
    }
}

