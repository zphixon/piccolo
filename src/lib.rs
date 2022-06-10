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

/// Commonly used items that you might want access to.
pub mod prelude {
    pub use super::compiler::{
        ast::Ast, ast::Expr, ast::Stmt, emitter::Emitter, scanner::Scanner, Token, TokenKind,
    };
    pub use super::error::{ErrorKind, PiccoloError};
    pub use super::runtime::{
        chunk::Chunk,
        chunk::Module,
        memory::{Gc, Heap, Root, UniqueRoot},
        object::{Function, NativeFunction, Object},
        op::Opcode,
        value::Constant,
        value::Value,
        vm::Machine,
    };
}

pub mod debug {
    pub use crate::compiler::{
        ast::print_ast, ast::print_expression, compile_chunk, emitter::compile,
        emitter::compile_with, parser::parse, print_tokens, scan_all,
    };
    pub use crate::runtime::{chunk::disassemble, chunk::disassemble_instruction};
}

use prelude::*;

use std::path::Path;

pub fn interpret(src: &str) -> Result<Constant, Vec<PiccoloError>> {
    use debug::*;
    debug!("parse");
    let ast = parse(&mut Scanner::new(src))?;
    debug!("ast\n{}", print_ast(&ast));

    debug!("compile");
    let module = compile(&ast)?;
    debug!("{}", disassemble(&module, ""));

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
    let bytes = std::fs::read(file).map_err(|e| vec![PiccoloError::from(e)])?;
    let module = bincode::deserialize(&bytes).map_err(|e| vec![PiccoloError::from(e)])?;

    let mut heap = Heap::default();
    let mut vm = Machine::new(&mut heap);
    Ok(vm.interpret(&mut heap, &module)?.into_constant())
}

pub fn compile(src: &Path, dst: &Path) -> Result<(), Vec<PiccoloError>> {
    use debug::*;

    let src = std::fs::read_to_string(src).map_err(|e| vec![PiccoloError::from(e)])?;
    let ast = parse(&mut Scanner::new(&src))?;
    let module = compile(&ast)?;

    std::fs::write(
        dst,
        bincode::serialize(&module).map_err(|e| vec![PiccoloError::from(e)])?,
    )
    .map_err(|e| vec![PiccoloError::from(e)])?;

    Ok(())
}

#[cfg(test)]
mod integration {
    use crate::debug::*;
    use crate::prelude::*;

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
        let ast = parse(&mut scanner).unwrap();
        println!("{}", print_ast(&ast));
        let module = compile(&ast).unwrap();
        println!("{}", disassemble(&module, "idklol"));
        let mut heap = Heap::default();
        let mut vm = Machine::new(&mut heap);
        println!("{:?}", vm.interpret(&mut heap, &module).unwrap());
    }

    #[test]
    fn visitor_emitter() {
        let src = "1+2*3+4";
        let mut scanner = Scanner::new(src);
        let ast = parse(&mut scanner).unwrap();
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

            println!("got:  {}", print_expression(expr));
            println!("want: {}", print_expression(&equiv));
            assert_eq!(expr, &equiv);

            let module = compile(&ast).unwrap();

            println!("{}", disassemble(&module, "idklol"));

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
