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

use {error::PiccoloError, runtime::value::Constant, std::path::Path};

pub fn interpret(src: &str) -> Result<Constant, Vec<PiccoloError>> {
    use compiler::{emitter, parser};

    let mut env = Environment::new();

    debug!("parse");
    let ast = parser::parse(src)?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));

    debug!("compile");
    emitter::compile_with(&mut env.emitter, &ast)?;
    let module = env.emitter.module();
    debug!("{}", runtime::chunk::disassemble(module, ""));

    debug!("interpret");
    Ok(env
        .vm
        .interpret(&mut env.heap, module)?
        .into_constant(&env.heap))
}

/// Reads a file and interprets its contents.
pub fn do_file(file: &Path) -> Result<Constant, Vec<PiccoloError>> {
    let contents = std::fs::read_to_string(file).map_err(|e| vec![PiccoloError::from(e)])?;
    interpret(&contents).map_err(|v| {
        v.into_iter()
            .map(|e| e.file(file.to_str().unwrap().to_owned()))
            .collect()
    })
}

use compiler::emitter::Emitter;
use runtime::{memory::Heap, value::Value, vm::Machine};

use crate::runtime::Object;

pub struct Environment {
    pub emitter: Emitter,
    pub heap: Heap,
    pub vm: Machine,
}

impl Default for Environment {
    fn default() -> Self {
        Environment::new()
    }
}

impl Environment {
    pub fn new() -> Environment {
        use runtime::{
            builtin::{self, BuiltinFunction},
            Arity,
        };

        let emitter = Emitter::new();
        let heap = Heap::new();
        let vm = Machine::new();

        let mut env = Environment { emitter, heap, vm };

        macro_rules! add_builtin_function {
            ($name:ident, $arity:expr) => {
                add_builtin_function!($name, $name, $arity);
            };

            ($name:ident, $funcname:ident, $arity:expr) => {
                let name = env
                    .heap
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

    pub fn add_global_variable(&mut self, name: &str, value: Value) {
        self.emitter
            .make_global_ident(compiler::Token::identifier(name));
        self.vm.globals.insert(String::from(name), value);
    }
}

#[cfg(test)]
mod integration {
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
        let module = emitter::compile(&ast).unwrap();
        println!("{}", chunk::disassemble(&module, "idklol"));
        let mut heap = Heap::new();
        let mut vm = Machine::new();
        println!("{:?}", vm.interpret(&mut heap, &module).unwrap());
    }

    #[test]
    fn visitor_emitter() {
        use {
            ast::{Expr, Stmt},
            compiler::{SourcePos, Token, TokenKind},
        };

        let src = "1+2*3+4";
        let ast = parser::parse(src).unwrap();
        if let Stmt::Expr { expr, .. } = &ast[0] {
            let equiv = Expr::Binary {
                lhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Literal {
                        literal: Token::new(TokenKind::Integer(1), "1", SourcePos::empty()),
                    }),
                    op: Token::new(TokenKind::Plus, "+", SourcePos::empty()),
                    rhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Literal {
                            literal: Token::new(TokenKind::Integer(2), "2", SourcePos::empty()),
                        }),
                        op: Token::new(TokenKind::Multiply, "*", SourcePos::empty()),
                        rhs: Box::new(Expr::Literal {
                            literal: Token::new(TokenKind::Integer(3), "3", SourcePos::empty()),
                        }),
                    }),
                }),
                op: Token::new(TokenKind::Plus, "+", SourcePos::empty()),
                rhs: Box::new(Expr::Literal {
                    literal: Token::new(TokenKind::Integer(4), "4", SourcePos::empty()),
                }),
            };

            println!("got:  {}", ast::print_expression(expr));
            println!("want: {}", ast::print_expression(&equiv));
            assert_eq!(expr, &equiv);

            let module = emitter::compile(&ast).unwrap();

            println!("{}", chunk::disassemble(&module, "idklol"));

            let mut heap = Heap::new();
            let mut vm = Machine::new();
            assert_eq!(
                vm.interpret(&mut heap, &module)
                    .unwrap()
                    .into_constant(&heap),
                Constant::Integer(11)
            );
        } else {
            panic!("ast not initialized")
        }
    }
}
