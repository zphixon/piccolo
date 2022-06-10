//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate fnv;
#[macro_use]
pub extern crate log;

pub mod compiler;
pub mod error;
pub mod runtime;

use {error::PiccoloError, runtime::value::Constant, std::path::Path};

pub fn interpret(src: &str) -> Result<Constant, Vec<PiccoloError>> {
    use {
        compiler::{ast, emitter, parser, scanner::Scanner},
        runtime::{chunk, memory::Heap, vm::Machine},
    };

    debug!("parse");
    let ast = parser::parse(&mut Scanner::new(src))?;
    debug!("ast\n{}", ast::print_ast(&ast));

    debug!("compile");
    let module = emitter::compile(&ast)?;
    debug!("{}", chunk::disassemble(&module, ""));

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

pub fn run_bin(file: &Path) -> Result<Constant, Vec<PiccoloError>> {
    use runtime::{memory::Heap, vm::Machine};

    let bytes = std::fs::read(file).map_err(|e| vec![PiccoloError::from(e)])?;
    let module = bincode::deserialize(&bytes).map_err(|e| vec![PiccoloError::from(e)])?;

    let mut heap = Heap::default();
    let mut vm = Machine::new(&mut heap);
    Ok(vm.interpret(&mut heap, &module)?.into_constant())
}

pub fn compile(src: &Path, dst: &Path) -> Result<(), Vec<PiccoloError>> {
    use compiler::{emitter, parser, scanner::Scanner};

    let src = std::fs::read_to_string(src).map_err(|e| vec![PiccoloError::from(e)])?;
    let ast = parser::parse(&mut Scanner::new(&src))?;
    let module = emitter::compile(&ast)?;

    std::fs::write(
        dst,
        bincode::serialize(&module).map_err(|e| vec![PiccoloError::from(e)])?,
    )
    .map_err(|e| vec![PiccoloError::from(e)])?;

    Ok(())
}

#[cfg(test)]
mod integration {
    use super::*;

    use {
        compiler::{ast, emitter, parser, scanner::Scanner},
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
        let mut scanner = Scanner::new(src);
        let ast = parser::parse(&mut scanner).unwrap();
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
            compiler::{Token, TokenKind},
        };

        let src = "1+2*3+4";
        let mut scanner = Scanner::new(src);
        let ast = parser::parse(&mut scanner).unwrap();
        if let Stmt::Expr { expr, .. } = &ast[0] {
            let equiv = Expr::Binary {
                lhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Literal {
                        literal: Token::new(TokenKind::Integer(1), "1", 1),
                    }),
                    op: Token::new(TokenKind::Plus, "+", 1),
                    rhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Literal {
                            literal: Token::new(TokenKind::Integer(2), "2", 1),
                        }),
                        op: Token::new(TokenKind::Multiply, "*", 1),
                        rhs: Box::new(Expr::Literal {
                            literal: Token::new(TokenKind::Integer(3), "3", 1),
                        }),
                    }),
                }),
                op: Token::new(TokenKind::Plus, "+", 1),
                rhs: Box::new(Expr::Literal {
                    literal: Token::new(TokenKind::Integer(4), "4", 1),
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
