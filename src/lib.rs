//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate fnv;
pub mod compiler;
pub mod error;
pub mod runtime;

#[cfg(feature = "cli")]
pub mod pretty;

#[macro_export]
macro_rules! trace {
    ($($log:expr),*) => {
        #[cfg(feature = "log")]
        log::trace!($($log),*);
    };
}

#[macro_export]
macro_rules! debug {
    ($($log:expr),*) => {
        #[cfg(feature = "log")]
        log::debug!($($log),*);
    };
}

#[macro_export]
macro_rules! info {
    ($($log:expr),*) => {
        #[cfg(feature = "log")]
        log::info!($($log),*);
    };
}

#[macro_export]
macro_rules! warn {
    ($($log:expr),*) => {
        #[cfg(feature = "log")]
        log::warn!($($log),*);
    };
}

#[macro_export]
macro_rules! error {
    ($($log:expr),*) => {
        #[cfg(feature = "log")]
        log::error!($($log),*);
    };
}

#[macro_export]
macro_rules! make_error {
    ($Variant:ident) => {
        $crate::make_error!($Variant {})
    };

    ($Variant:ident {
        $($field:ident $(: $value:expr)? ),* $(,)?
    }) => {{
        let err = $crate::error::PiccoloError::new($crate::error::ErrorKind::$Variant {
            $($field $(: $value)?),*
        });
        $crate::error!("{}", err);
        err
    }};
}

use crate::{
    compiler::emitter::Emitter,
    error::PiccoloError,
    runtime::{
        interner::Interner, memory::Heap, value::Value, vm::Machine, Context, ContextMut, Object,
    },
};
use std::path::Path;

pub fn interpret(src: &str) -> Result<(Environment, Value), Vec<PiccoloError>> {
    use compiler::parser;

    let mut env = Environment::new();

    debug!("parse");
    let ast = parser::parse(src)?;

    #[cfg(feature = "cli")]
    debug!("ast\n{}", pretty::print_ast(&ast));

    debug!("compile");
    env.compile(&ast)?;

    #[cfg(feature = "cli")]
    debug!("{}", env.disassemble(""));

    debug!("interpret");
    let value = env.interpret_compiled()?;
    Ok((env, value))
}

/// Reads a file and interprets its contents.
pub fn do_file(file: &Path) -> Result<(Environment, Value), Vec<PiccoloError>> {
    let contents = std::fs::read_to_string(file).map_err(|e| vec![PiccoloError::from(e)])?;
    interpret(&contents).map_err(|v| {
        v.into_iter()
            .map(|e| e.file(file.to_str().unwrap().to_owned()))
            .collect()
    })
}

pub struct Environment {
    pub emitter: Emitter,
    pub heap: Heap,
    pub vm: Machine,
    pub interner: Interner,
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

impl Environment {
    pub fn empty() -> Environment {
        Environment {
            emitter: Emitter::new(),
            heap: Heap::new(),
            vm: Machine::new(),
            interner: Interner::new(),
        }
    }

    pub fn new() -> Environment {
        use runtime::{
            builtin::{self, BuiltinFunction},
            Arity,
        };

        let emitter = Emitter::new();
        let heap = Heap::new();
        let vm = Machine::new();
        let interner = Interner::new();

        let mut env = Environment {
            emitter,
            heap,
            vm,
            interner,
        };

        macro_rules! add_builtin_function {
            ($name:ident, $arity:expr) => {
                add_builtin_function!($name, $name, $arity);
            };

            ($name:ident, $funcname:ident, $arity:expr) => {
                let name = env
                    .interner_mut()
                    .allocate_string(String::from(stringify!($name)));
                env.add_global_variable(
                    stringify!($name),
                    Value::BuiltinFunction(BuiltinFunction::new(name, $arity, builtin::$funcname)),
                );
            };
        }

        add_builtin_function!(write, Arity::Any);
        add_builtin_function!(print, Arity::Any);
        add_builtin_function!(rand, Arity::Exact(0));
        add_builtin_function!(toString, to_string, Arity::Any);
        add_builtin_function!(clone, Arity::Exact(1));
        add_builtin_function!(type, type_, Arity::Exact(1));
        add_builtin_function!(clock, Arity::Exact(0));
        add_builtin_function!(sleep, Arity::AtLeast(1));
        add_builtin_function!(truncate, Arity::Exact(1));
        add_builtin_function!(double, Arity::Exact(1));
        add_builtin_function!(floor, Arity::Exact(1));
        add_builtin_function!(ceil, Arity::Exact(1));
        add_builtin_function!(round, Arity::Exact(1));
        add_builtin_function!(abs, Arity::Exact(1));
        add_builtin_function!(sign, Arity::Exact(1));
        add_builtin_function!(cos, Arity::Exact(1));
        add_builtin_function!(sin, Arity::Exact(1));
        add_builtin_function!(tan, Arity::Exact(1));
        add_builtin_function!(input, Arity::Any);
        add_builtin_function!(exit, Arity::Any);

        env
    }

    #[cfg(feature = "cli")]
    pub fn dump(&self) {
        println!("{}", self.disassemble(""));
        println!("{:#?}", self.vm);
        println!("{:#?}", self.heap);
        println!("{:#?}", self.interner);
    }

    pub fn context(&self) -> Context {
        Context {
            heap: &self.heap,
            interner: &self.interner,
        }
    }

    pub fn context_mut(&mut self) -> ContextMut {
        ContextMut {
            heap: &mut self.heap,
            interner: &mut self.interner,
        }
    }

    pub fn add_global_variable(&mut self, name: &str, value: Value) {
        self.emitter
            .make_global_ident(&mut self.interner, compiler::Token::identifier(name));
        self.vm
            .globals
            .insert(self.interner.allocate_str(name), value);
    }

    pub fn compile(&mut self, ast: &compiler::ast::Ast) -> Result<(), Vec<PiccoloError>> {
        compiler::emitter::compile_with(&mut self.emitter, &mut self.interner, ast)
    }

    #[cfg(feature = "cli")]
    #[must_use]
    pub fn disassemble(&self, name_of_module: &str) -> String {
        pretty::disassemble(&self.interner, self.emitter.module(), name_of_module)
    }

    pub fn interpret(&mut self, src: &str) -> Result<Value, Vec<PiccoloError>> {
        let ast = compiler::parser::parse(src)?;
        self.compile(&ast)?;
        self.interpret_compiled().map_err(|e| vec![e])
    }

    pub fn interpret_compiled(&mut self) -> Result<Value, PiccoloError> {
        self.vm.interpret(
            &mut ContextMut {
                heap: &mut self.heap,
                interner: &mut self.interner,
            },
            self.emitter.module(),
        )
    }

    pub fn interpret_continue(&mut self) -> Result<Value, PiccoloError> {
        self.vm.interpret_continue(
            &mut ContextMut {
                heap: &mut self.heap,
                interner: &mut self.interner,
            },
            self.emitter.module(),
        )
    }

    pub fn clear_errors(&mut self) {
        self.vm
            .clear_stack_and_move_to_end_of_module(self.emitter.module());
        self.emitter.reset_after_errors();
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
    }

    pub fn interner_mut(&mut self) -> &mut Interner {
        &mut self.interner
    }

    pub fn strings(&self) -> impl Iterator<Item = &str> {
        self.interner().strings()
    }

    pub fn objects(&self) -> impl Iterator<Item = &dyn Object> {
        self.heap.objects()
    }

    pub fn collect(&mut self) {
        self.heap.collect(self.vm.roots())
    }

    pub fn format(&self, value: Value) -> String {
        value.format(self.context())
    }

    pub fn debug(&self, value: Value) -> String {
        value.debug_format(self.context())
    }

    pub fn format_object(&self, object: &dyn Object) -> String {
        object.format(self.context())
    }

    pub fn debug_object(&self, object: &dyn Object) -> String {
        object.debug_format(self.context())
    }
}

#[cfg(test)]
mod test_lib {
    use super::*;

    #[test]
    fn reentrant() {
        use crate::compiler::parser::parse;

        let ast1 = parse("x=:3").unwrap();
        let ast2 = parse("assert x == 3").unwrap();
        let ast3 = parse("fn z(a) do\n  print(\"a is\", a)\n  end\n").unwrap();
        let ast4 = parse("z(x)").unwrap();

        let mut env = Environment::new();

        env.compile(&ast1).unwrap();
        #[cfg(feature = "cli")]
        println!("{}", env.disassemble(""));

        env.compile(&ast2).unwrap();
        #[cfg(feature = "cli")]
        println!("{}", env.disassemble(""));

        env.compile(&ast3).unwrap();
        #[cfg(feature = "cli")]
        println!("{}", env.disassemble(""));

        env.compile(&ast4).unwrap();
        #[cfg(feature = "cli")]
        println!("{}", env.disassemble(""));
    }
}
