use crate::{
    error::PiccoloError,
    make_error,
    runtime::{interner::StringPtr, Arity, ContextMut, Object, This, Value},
};
use once_cell::sync::Lazy;
use std::{
    fmt::{Debug, Write as FmtWrite},
    io::Write as IoWrite,
    time::{Duration, Instant},
};

pub fn to_string(ctx: &mut ContextMut, values: &[Value]) -> Result<Value, PiccoloError> {
    if values.is_empty() {
        Ok(Value::String(ctx.interner.allocate_str("")))
    } else if let [Value::String(s)] = values {
        Ok(Value::String(*s))
    } else {
        let mut s = String::new();

        for (i, value) in values.iter().enumerate() {
            write!(s, "{}", value.format(ctx.as_ref()))?;

            if i + 1 != values.len() {
                s.push('\t');
            }
        }

        let ptr = ctx.interner.allocate_string(s);

        Ok(Value::String(ptr))
    }
}

pub fn write(ctx: &mut ContextMut, values: &[Value]) -> Result<Value, PiccoloError> {
    if let Value::String(ptr) = to_string(ctx, values)? {
        print!("{}", ctx.interner.get_string(ptr));
        std::io::stdout().flush().unwrap();
    }

    Ok(Value::Nil)
}

pub fn print(ctx: &mut ContextMut, values: &[Value]) -> Result<Value, PiccoloError> {
    write(ctx, values)?;
    println!();

    Ok(Value::Nil)
}

pub fn rand(_: &mut ContextMut, _: &[Value]) -> Result<Value, PiccoloError> {
    Ok(Value::Double(rand::random()))
}

pub fn clone(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    let arg = args[0];
    match arg {
        // TODO when we have closures
        // Value::Function(f) => {}
        Value::Object(ptr) => return Ok(Value::Object(ctx.heap.clone(ptr))),
        _ => {}
    }
    Ok(arg)
}

pub fn type_(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    let arg = args[0];
    let name = arg.type_name(ctx.as_ref());
    Ok(Value::String(ctx.interner.allocate_str(name)))
}

static START: Lazy<Instant> = Lazy::new(Instant::now);
pub fn clock(_: &mut ContextMut, _: &[Value]) -> Result<Value, PiccoloError> {
    let duration = Instant::now() - *START;
    Ok(Value::Double(duration.as_secs_f64()))
}

pub fn sleep(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if args.len() != 1 && args.len() != 2 {
        return Err(make_error!(IncorrectArity {
            name: "sleep".to_string(),
            exp: Arity::AtLeast(1),
            got: args.len(),
        }));
    }

    let non_negative = |secs: i64| -> Result<u64, PiccoloError> {
        secs.try_into().map_err(|_| {
            make_error!(InvalidArgument {
                exp: "non-negative integer".to_string(),
                got: args[0].format(ctx.as_ref()),
            })
        })
    };

    use std::thread::sleep;
    match (args[0], args.get(1)) {
        (Value::Integer(secs), None) => {
            sleep(Duration::from_secs(non_negative(secs)?));
        }

        (Value::Integer(length), Some(Value::String(unit))) => {
            let length = non_negative(length)?;
            match ctx.interner.get_string(*unit) {
                "ns" | "nanosecond" | "nanoseconds" => sleep(Duration::from_nanos(length)),
                "us" | "microsecond" | "microseconds" => sleep(Duration::from_micros(length)),
                "ms" | "millisecond" | "milliseconds" => sleep(Duration::from_millis(length)),
                "s" | "sec" | "second" | "seconds" => sleep(Duration::from_secs(length)),
                "m" | "min" | "minute" | "minutes" => sleep(60 * Duration::from_secs(length)),
                "h" | "hr" | "hour" | "hours" => sleep(60 * 60 * Duration::from_secs(length)),

                _ => {
                    return Err(make_error!(InvalidArgument {
                        exp: "nanoseconds, microseconds, milliseconds, seconds, minutes, or hours"
                            .to_string(),
                        got: ctx.interner.get_string(*unit).to_string(),
                    }))
                }
            }
        }

        _ => {
            return Err(make_error!(IncorrectType {
                exp: "integer and optional string unit".to_string(),
                got: args[0].type_name(ctx.as_ref()).to_string(),
            }))
        }
    }

    Ok(Value::Nil)
}

pub fn truncate(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Integer(num.trunc() as i64)),
        Value::Integer(num) => Ok(Value::Integer(num)),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn double(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Double(num)),
        Value::Integer(num) => Ok(Value::Double(num as f64)),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn floor(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Integer(num.floor() as i64)),
        Value::Integer(num) => Ok(Value::Integer(num)),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn ceil(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Integer(num.ceil() as i64)),
        Value::Integer(num) => Ok(Value::Integer(num)),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn round(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Integer(num.round() as i64)),
        Value::Integer(num) => Ok(Value::Integer(num)),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn abs(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Double(num.abs())),
        Value::Integer(num) => Ok(Value::Integer(num.abs())),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn sign(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    match args[0] {
        Value::Double(num) => Ok(Value::Integer(num.signum() as i64)),
        Value::Integer(num) => Ok(Value::Integer(num.signum())),
        val => Err(make_error!(IncorrectType {
            exp: "double or integer".to_string(),
            got: val.type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub fn cos(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if let Value::Double(num) = args[0] {
        Ok(Value::Double(num.cos()))
    } else {
        Err(make_error!(IncorrectType {
            exp: "double".to_string(),
            got: args[0].type_name(ctx.as_ref()).to_string(),
        }))
    }
}

pub fn sin(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if let Value::Double(num) = args[0] {
        Ok(Value::Double(num.sin()))
    } else {
        Err(make_error!(IncorrectType {
            exp: "double".to_string(),
            got: args[0].type_name(ctx.as_ref()).to_string(),
        }))
    }
}

pub fn tan(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if let Value::Double(num) = args[0] {
        Ok(Value::Double(num.tan()))
    } else {
        Err(make_error!(IncorrectType {
            exp: "double".to_string(),
            got: args[0].type_name(ctx.as_ref()).to_string(),
        }))
    }
}

pub fn input(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if !args.is_empty() {
        write(ctx, args)?;
    }

    let mut buf = String::new();
    std::io::stdin().read_line(&mut buf)?;

    Ok(Value::String(ctx.interner.allocate_string(buf)))
}

pub fn exit(ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
    if args.len() > 1 {
        return Err(make_error!(IncorrectArity {
            name: "exit".to_string(),
            exp: Arity::AtLeast(0),
            got: args.len(),
        }));
    }

    match args.get(0) {
        Some(Value::Integer(code)) => std::process::exit(*code as i32),
        None => std::process::exit(0),
        _ => Err(make_error!(IncorrectType {
            exp: "integer".to_string(),
            got: args[0].type_name(ctx.as_ref()).to_string(),
        })),
    }
}

pub type PiccoloFunction = fn(&mut ContextMut, &[Value]) -> Result<Value, PiccoloError>;

#[derive(Clone, Copy)]
pub struct BuiltinFunction {
    pub name: StringPtr,
    pub arity: Arity,
    pub ptr: PiccoloFunction,
    pub this: This,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

impl BuiltinFunction {
    pub fn new(name: StringPtr, arity: Arity, ptr: PiccoloFunction) -> Self {
        BuiltinFunction {
            name,
            arity,
            ptr,
            this: This::None,
        }
    }

    pub fn name(&self) -> StringPtr {
        self.name
    }

    pub fn call(&self, ctx: &mut ContextMut, args: &[Value]) -> Result<Value, PiccoloError> {
        if !self.arity.is_compatible(args.len()) {
            return Err(make_error!(IncorrectArity {
                name: ctx.interner.get_string(self.name).to_string(),
                exp: self.arity,
                got: args.len(),
            }));
        }

        (self.ptr)(ctx, args)
    }
}

impl Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("arity", &self.arity)
            .field("ptr", &(self.ptr as *const fn() as usize))
            .finish()
    }
}
