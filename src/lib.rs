//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate fnv;
pub mod compiler;
pub mod error;
pub mod runtime;

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

use {error::PiccoloError, std::path::Path};

pub fn interpret(src: &str) -> Result<(Environment, Value), Vec<PiccoloError>> {
    use compiler::parser;

    let mut env = Environment::new();

    debug!("parse");
    let ast = parser::parse(src)?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));

    debug!("compile");
    env.compile(&ast)?;
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

use compiler::emitter::Emitter;
use runtime::{interner::Interner, memory::Heap, value::Value, vm::Machine, Context, ContextMut};

use crate::runtime::Object;

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

    pub fn dump(&self) {
        println!("{}", self.disassemble(""));
        println!("{:#?}", self.vm);
        println!("{:#?}", self.heap);
        println!("{:#?}", self.interner);
        //pub emitter: Emitter,
        //pub heap: Heap,
        //pub vm: Machine,
        //pub interner: Interner,
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

    #[must_use]
    pub fn disassemble(&self, name_of_module: &str) -> String {
        runtime::chunk::disassemble(&self.interner, self.emitter.module(), name_of_module)
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
mod integration {
    use crate::runtime::ContextMut;

    use super::*;

    use {
        compiler::{ast, emitter, parser},
        runtime::{chunk, memory::Heap, vm::Machine},
    };

    #[test]
    #[ignore]
    fn very_long() {
        let path = std::path::Path::new("examples/long.pc");
        crate::do_file(path).unwrap();
    }

    #[test]
    fn idk() {
        let src = "a=:1+2";
        let ast = parser::parse(src).unwrap();
        println!("{}", ast::print_ast(&ast));
        let mut interner = Interner::new();
        let module = emitter::compile(&mut interner, &ast).unwrap();
        println!("{}", chunk::disassemble(&interner, &module, "idklol"));
        let mut heap = Heap::new();
        let mut vm = Machine::new();
        let mut interner = Interner::new();
        let mut ctx = ContextMut {
            heap: &mut heap,
            interner: &mut interner,
        };
        println!("{:?}", vm.interpret(&mut ctx, &module).unwrap());
    }

    #[test]
    fn visitor_emitter() {
        use {
            ast::{Expr, Stmt},
            compiler::{Token, TokenKind},
        };

        let src = "1+2*3+4";
        let ast = parser::parse(src).unwrap();
        if let Stmt::Expr { expr, .. } = &ast[0] {
            let equiv = Expr::Binary {
                lhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Literal {
                        literal: Token::test(TokenKind::Integer(1)),
                    }),
                    op: Token::test(TokenKind::Plus),
                    rhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Literal {
                            literal: Token::test(TokenKind::Integer(2)),
                        }),
                        op: Token::test(TokenKind::Multiply),
                        rhs: Box::new(Expr::Literal {
                            literal: Token::test(TokenKind::Integer(3)),
                        }),
                    }),
                }),
                op: Token::test(TokenKind::Plus),
                rhs: Box::new(Expr::Literal {
                    literal: Token::test(TokenKind::Integer(4)),
                }),
            };

            println!("got:  {}", ast::print_expression(expr));
            println!("want: {}", ast::print_expression(&equiv));
            assert_eq!(expr, &equiv);

            let mut interner = Interner::new();
            let module = emitter::compile(&mut interner, &ast).unwrap();

            println!("{}", chunk::disassemble(&mut interner, &module, "idklol"));

            let mut heap = Heap::new();
            let mut vm = Machine::new();
            let mut interner = Interner::new();
            let mut ctx = ContextMut {
                heap: &mut heap,
                interner: &mut interner,
            };
            assert!(vm
                .interpret(&mut ctx, &module)
                .unwrap()
                .eq(ctx.as_ref(), Value::Integer(11))
                .unwrap());
        } else {
            panic!("ast not initialized")
        }
    }
}
