extern crate rustyline;
extern crate time;

use super::*;
use value::{is_truthy, Value};
use interp::Interpreter;
use err::{ErrorKind, PiccoloError};
use foreign::{Foreign, Something};

use self::rustyline::Editor;

use std::collections::HashMap;

fn new_native_func(arity: func::Arity, func: func::NativeFuncType) -> data::Field {
    data::Field {
        //public: true,
        //normal: true,
        value: value::Value::Func(func::Func::new_native(arity, func::NativeFunc::new(func))),
    }
}

pub fn create_stdlib() -> env::Scope {
    let mut env = env::Scope::new();

    let module = data::Data::new("mod", HashMap::new(), HashMap::new());

    // io

    let mut io_vars = HashMap::new();

    io_vars.insert(
        "prln".into(),
        new_native_func(func::Arity::Multi, |_, args| {
            if args.is_empty() {
                println!();
            } else if args.len() == 1 {
                println!("{}", args[0]);
            } else {
                print!("{}", args[0]);
                for item in &args[1..args.len()] {
                    print!("\t{}", item);
                }
                println!();
            }
            Ok(Value::Nil)
        }),
    );

    io_vars.insert(
        "pr".into(),
        new_native_func(func::Arity::Multi, |_, args| {
            if args.is_empty() {
            } else if args.len() == 1 {
                print!("{}", args[0]);
            } else {
                print!("{}", args[0]);
                for item in &args[1..args.len()] {
                    print!("\t{}", item);
                }
            }
            Ok(Value::Nil)
        }),
    );

    io_vars.insert(
        "input".into(),
        new_native_func(func::Arity::None, |_, _| {
            let mut rl = Editor::<()>::new();
            if let Ok(input) = rl.readline("") {
                Ok(Value::String(input))
            } else {
                Ok(Value::Nil)
            }
        }),
    );

    env.set("io", Value::Instance(data::Instance::new(&module, io_vars)));

    // sys

    let mut sys_vars = HashMap::new();

    sys_vars.insert(
        "clock".into(),
        new_native_func(func::Arity::None, |_, _| {
            let ts = time::now().to_timespec();
            Ok((ts.sec * 1_000 + i64::from(ts.nsec) / 1_000_000).into())
        }),
    );

    sys_vars.insert(
        "panic".into(),
        new_native_func(func::Arity::Some(1), |_, args| {
            eprintln!("piccolo panic! {}", args[0]);
            std::process::exit(1);
        }),
    );

    sys_vars.insert(
        "assert".into(),
        new_native_func(func::Arity::Some(1), |_, args| {
            if !is_truthy(&args[0]) {
                eprintln!("assert failed: {}", args[0]);
                std::process::exit(1);
            }
            Ok(Value::Bool(true))
        }),
    );

    sys_vars.insert(
        "show_env".into(),
        new_native_func(func::Arity::None, |i, _| {
            println!("{}", i.env);
            Ok(Value::Nil)
        }),
    );

    sys_vars.insert(
        "exit".into(),
        new_native_func(func::Arity::None, |_, _| {
            std::process::exit(0);
        }),
    );

    env.set(
        "sys",
        Value::Instance(data::Instance::new(&module, sys_vars)),
    );

    // arr

    let mut arr_vars = HashMap::new();

    arr_vars.insert(
        "push".into(),
        new_native_func(func::Arity::Some(2), |_, args| match args[0] {
            Value::Array(ref a) => {
                let mut na = a.clone();
                na.push(args[1].clone());
                Ok(Value::Array(na))
            }
            _ => Err(PiccoloError::new(
                ErrorKind::IndexError,
                "Cannot push onto a non-array",
                0,
            )),
        }),
    );

    arr_vars.insert(
        "pop".into(),
        new_native_func(func::Arity::Some(1), |_, args| match args[0] {
            Value::Array(ref a) => {
                let mut na = a.clone();
                if let Some(v) = na.pop() {
                    Ok(Value::Array(vec![v, Value::Array(na)]))
                } else {
                    Ok(Value::Nil)
                }
            }
            _ => Err(PiccoloError::new(
                ErrorKind::IndexError,
                "Cannot pop from a non-array",
                0,
            )),
        }),
    );

    arr_vars.insert(
        "insert".into(),
        new_native_func(func::Arity::Some(3), |_, args| match args[0] {
            Value::Array(ref a) => match args[2] {
                Value::Integer(n) => {
                    if (n as usize) < a.len() {
                        let mut na = a.clone();
                        na.insert(n as usize, args[1].clone());
                        Ok(Value::Array(na))
                    } else if (n as usize) == a.len() {
                        let mut na = a.clone();
                        na.push(args[1].clone());
                        Ok(Value::Array(na))
                    } else {
                        Err(PiccoloError::new(
                            ErrorKind::IndexError,
                            "Index out of bounds",
                            0,
                        ))
                    }
                }
                _ => Err(PiccoloError::new(
                    ErrorKind::IndexError,
                    "Cannot index with non-integer",
                    0,
                )),
            },
            _ => Err(PiccoloError::new(
                ErrorKind::IndexError,
                "Cannot insert into a non-array",
                0,
            )),
        }),
    );

    arr_vars.insert(
        "set".into(),
        new_native_func(func::Arity::Some(3), |_, args| match args[0] {
            Value::Array(ref a) => match args[2] {
                Value::Integer(n) => {
                    if (n as usize) < a.len() {
                        let mut na = a.clone();
                        na[n as usize] = args[1].clone();
                        Ok(Value::Array(na))
                    } else {
                        Err(PiccoloError::new(
                            ErrorKind::IndexError,
                            "Index out of bounds",
                            0,
                        ))
                    }
                }
                _ => Err(PiccoloError::new(
                    ErrorKind::IndexError,
                    "Cannot index with non-integer",
                    0,
                )),
            },
            _ => Err(PiccoloError::new(
                ErrorKind::IndexError,
                "Cannot insert into a non-array",
                0,
            )),
        }),
    );

    env.set(
        "arr",
        Value::Instance(data::Instance::new(&module, arr_vars)),
    );

    // none

    env.new_native_func("str", func::Arity::Some(1), |_, args| {
        Ok(Value::String(format!("{}", args[0])))
    });

    env.new_native_func("num", func::Arity::Some(1), |_, args| {
        if let Ok(n) = format!("{}", args[0]).parse::<i64>() {
            Ok(Value::Integer(n))
        } else if let Ok(n) = format!("{}", args[0]).parse::<f64>() {
            Ok(Value::Float(n))
        } else {
            Ok(Value::Nil)
        }
    });

    env.new_native_func("len", func::Arity::Some(1), |_, args| match args[0] {
        Value::Array(ref v) => Ok(Value::Integer(v.len() as i64)),
        Value::String(ref v) => Ok(Value::Integer(v.len() as i64)),
        _ => Ok(Value::Integer(0)),
    });

    env.new_native_func("type", func::Arity::Some(1), |_, args| match args[0] {
        Value::String(_) => Ok("string".into()),
        Value::Bool(_) => Ok("bool".into()),
        Value::Integer(_) => Ok("int".into()),
        Value::Float(_) => Ok("float".into()),
        Value::Array(_) => Ok("array".into()),
        Value::Func(_) => Ok("fn".into()),
        Value::Data(_) => Ok("data".into()),
        Value::Instance(ref i) => Ok(i.inner.borrow().data.name.clone().into()),
        Value::Foreign(ref f) => Ok(f.get_name().into()),
        Value::Nil => Ok("nil".into()),
    });

    env.new_native_func("inject_foreign", func::Arity::None, |_, _| {
        Ok(Value::Foreign(::std::rc::Rc::new(Something {
            num: 3,
            s: "hello!".into(),
        })))
    });

    env
}
