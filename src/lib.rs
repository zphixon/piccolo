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

    let (mut emitter, mut heap, mut vm) = make_environment();

    debug!("parse");
    let ast = parser::parse(src)?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));

    debug!("compile");
    emitter::compile_with(&mut emitter, &ast)?;
    let module = emitter.module();
    debug!("{}", runtime::chunk::disassemble(module, ""));

    debug!("interpret");
    Ok(vm.interpret(&mut heap, module)?.into_constant(&heap))
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

pub fn make_environment() -> (
    compiler::emitter::Emitter,
    runtime::memory::Heap,
    runtime::vm::Machine,
) {
    use compiler::{emitter::Emitter, Token};
    use runtime::{
        builtin::{self, NativeFunction},
        memory::Heap,
        value::Value,
        vm::Machine,
        Arity,
    };

    let mut emitter = Emitter::new();
    let mut heap = Heap::new();
    let mut vm = Machine::new();

    macro_rules! add_native_function {
        ($name:ident, $arity:expr) => {
            add_native_function!($name, $name, $arity);
        };

        ($name:ident, $funcname:ident, $arity:expr) => {
            emitter.make_global_ident(Token::identifier(stringify!($name)));
            vm.globals.insert(
                String::from(stringify!($name)),
                Value::NativeFunction(NativeFunction::new(
                    heap.interner_mut()
                        .allocate_string(String::from(stringify!($name))),
                    $arity,
                    builtin::$funcname,
                )),
            );
        };
    }

    add_native_function!(print, Arity::Any);
    add_native_function!(rand, Arity::Exact(0));
    add_native_function!(toString, to_string, Arity::Any);
    add_native_function!(clone, Arity::Exact(1));
    add_native_function!(type, type_, Arity::Exact(1));
    add_native_function!(clock, Arity::Exact(0));

    (emitter, heap, vm)
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
            // TODO
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
