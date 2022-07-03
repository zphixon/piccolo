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
    use {
        compiler::{emitter, parser},
        runtime::{memory::Heap, vm::Machine},
    };

    debug!("parse");
    let ast = parser::parse(src)?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));

    debug!("compile");
    let module = emitter::compile(&ast)?;
    debug!("{}", runtime::chunk::disassemble(&module, ""));

    let mut heap = Heap::default();

    debug!("interpret");
    let mut vm = Machine::new(&mut heap);
    Ok(vm.interpret(&mut heap, &module)?.into_constant())
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
        let mut heap = Heap::default();
        let mut vm = Machine::new(&mut heap);
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

            let mut heap = Heap::default();
            let mut vm = Machine::new(&mut heap);
            // TODO
            assert_eq!(
                vm.interpret(&mut heap, &module).unwrap().into_constant(),
                Constant::Integer(11)
            );
        } else {
            panic!("ast not initialized")
        }
    }
}
