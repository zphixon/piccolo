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

pub(crate) fn encode_bytes(low: u8, high: u8) -> u16 {
    ((high as u16) << 8) | (low as u16)
}

pub(crate) fn decode_bytes(bytes: u16) -> (u8, u8) {
    let high = (bytes >> 8) as u8;
    let low = (bytes & 0xff) as u8;
    (low, high)
}

#[cfg(feature = "fuzzer")]
pub mod fuzzer {
    extern crate rand;

    use crate::debug::*;
    use crate::prelude::*;

    use rand::distributions::{Distribution, Standard};
    use rand::Rng;

    /// Run `n` tests of random tokens.
    pub fn fuzz(n: usize, min_len: usize, max_len: usize) -> Option<Vec<usize>> {
        let mut ok = None;
        let start = std::time::Instant::now();
        let mut avg = 0.0;
        for n in 1..=n {
            let s = std::time::Instant::now();
            if let Some(_) = run(n, min_len, max_len) {
                if ok.is_none() {
                    ok = Some(vec![n]);
                } else {
                    ok.as_mut().unwrap().push(n);
                }
            }
            avg += (std::time::Instant::now() - s).as_secs_f64();
        }
        println!(
            "{} runs, in {:.8} sec ({:.8} avg per run)",
            n,
            (std::time::Instant::now() - start).as_secs_f64(),
            avg / n as f64
        );
        ok
    }

    // occasionally creates valid programs
    fn run(n: usize, min_len: usize, max_len: usize) -> Option<()> {
        let mut src = String::new();
        let mut r = rand::thread_rng();
        let lines = r.gen_range(min_len..max_len);
        for _ in 1..lines {
            let tk: TokenKind = r.gen();
            src.push_str(&format!("{} ", tk).to_lowercase());
        }

        if let Ok(chunk) = compile_chunk(&src) {
            println!("----- run {} compiles -----", n);
            print_tokens(&scan_all(&src).unwrap());
            disassemble(&chunk, "");
            let mut heap = Heap::default();
            let mut vm = Machine::new(&mut heap);
            vm.interpret(&mut heap, &chunk).ok().map(|_| {
                println!("----- run {} executes -----", n);
            })
        } else {
            None
        }
    }

    impl Distribution<TokenKind> for Standard {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TokenKind {
            match rng.gen_range(0..50) {
                0 => TokenKind::Do,
                1 => TokenKind::End,
                2 => TokenKind::Fn,
                3 => TokenKind::If,
                4 => TokenKind::Else,
                5 => TokenKind::While,
                6 => TokenKind::For,
                // 7 => TokenKind::In,
                // 8 => TokenKind::Data,
                9 => TokenKind::Let,
                // 10 => TokenKind::Is,
                // 11 => TokenKind::Me,
                // 12 => TokenKind::New,
                // 13 => TokenKind::Err,
                14 => TokenKind::Retn,
                15 => TokenKind::Nil,
                // 16 => TokenKind::LeftBracket,
                // 17 => TokenKind::RightBracket,
                18 => TokenKind::LeftParen,
                19 => TokenKind::RightParen,
                20 => TokenKind::Comma,
                21 => TokenKind::Period,
                // 22 => TokenKind::ExclusiveRange,
                // 23 => TokenKind::InclusiveRange,
                24 => TokenKind::Assign,
                25 => TokenKind::Not,
                26 => TokenKind::Plus,
                27 => TokenKind::Minus,
                28 => TokenKind::Multiply,
                29 => TokenKind::Divide,
                30 => TokenKind::Modulo,
                31 => TokenKind::LogicalAnd,
                32 => TokenKind::LogicalOr,
                33 => TokenKind::BitwiseAnd,
                34 => TokenKind::BitwiseOr,
                35 => TokenKind::BitwiseXor,
                36 => TokenKind::Equal,
                37 => TokenKind::NotEqual,
                38 => TokenKind::Less,
                39 => TokenKind::Greater,
                40 => TokenKind::LessEqual,
                41 => TokenKind::GreaterEqual,
                42 => TokenKind::ShiftLeft,
                43 => TokenKind::ShiftRight,
                44 => TokenKind::Identifier,
                45 => TokenKind::String,
                46 => TokenKind::True,
                47 => TokenKind::False,
                48 => TokenKind::Double(0.0),
                49 => TokenKind::Integer(1),
                _ => TokenKind::Nil,
            }
        }
    }
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
    fn encode_decode() {
        let bytes: u16 = 0xbead;
        let (low, high) = crate::decode_bytes(bytes);
        assert_eq!(high, 0xbe);
        assert_eq!(low, 0xad);

        let bytes2 = crate::encode_bytes(low, high);
        assert_eq!(bytes, bytes2);
    }

    #[test]
    fn idk() {
        let src = "a=:1+2";
        let mut scanner = Scanner::new(src);
        let ast = parse(&mut scanner).unwrap();
        println!("{}", print_ast(&ast));
        let module = compile(&ast).unwrap();
        #[cfg(feature = "pc-debug")]
        {
            println!("{}", disassemble(&module, "idklol"));
        }
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

            #[cfg(feature = "pc-debug")]
            {
                println!("{}", disassemble(&module, "idklol"));
            }

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
