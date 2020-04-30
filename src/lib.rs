//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate downcast_rs;
#[macro_use]
pub extern crate log;

pub mod compiler;
pub mod error;
pub mod runtime;

/// Commonly used items that you might want access to.
pub mod prelude {
    pub use super::compiler::{emitter::Emitter, parser::Parser, scanner::Scanner};
    pub use super::compiler::{Token, TokenKind};
    pub use super::error::{ErrorKind, PiccoloError};
    pub use super::runtime::{
        chunk::Chunk, object::Object, value::Constant, value::Value, vm::Machine,
    };
}

use prelude::*;

#[cfg(feature = "pc-debug")]
pub use compiler::{compile, scan_all};

#[cfg(feature = "fuzzer")]
pub use compiler::print_tokens;

/// Interprets a Piccolo source and returns its result.
///
/// # Examples
///
/// ```rust
/// # fn main() -> Result<(), Vec<piccolo::prelude::PiccoloError>> {
/// let result = piccolo::interpret("1 + 2")?;
/// assert_eq!(3, result.into::<i64>());
/// # Ok(())
/// # }
/// ```
pub fn interpret(src: &str) -> Result<Constant, Vec<PiccoloError>> {
    let mut scanner = Scanner::new(src);
    debug!("parse");
    let ast = Parser::new().parse(&mut scanner)?;
    debug!("ast\n{}", compiler::ast::AstPrinter::print(&ast));
    debug!("compile");
    let chunk = Emitter::new(Chunk::default()).compile(&ast)?;
    debug!("chunk\n{}", chunk.disassemble(""));
    debug!("interpret");
    Ok(Machine::new().interpret(&chunk)?)
}

/// Reads a file and interprets its contents.
pub fn do_file(file: &std::path::Path) -> Result<Constant, Vec<PiccoloError>> {
    let contents = std::fs::read_to_string(file).map_err(|e| vec![PiccoloError::from(e)])?;
    interpret(&contents).map_err(|v| {
        v.into_iter()
            .map(|e| e.file(file.to_str().unwrap().to_owned()))
            .collect()
    })
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

    use crate::compiler::TokenKind;
    use crate::Machine;

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
        let lines = r.gen_range(min_len, max_len);
        for _ in 1..lines {
            let tk: TokenKind = r.gen();
            src.push_str(&format!("{} ", tk).to_lowercase());
        }

        if let Ok(chunk) = crate::compile(&src) {
            println!("----- run {} compiles -----", n);
            crate::print_tokens(&crate::compiler::scan_all(&src).unwrap());
            chunk.disassemble("");
            Machine::new().interpret(&chunk).ok().map(|_| {
                println!("----- run {} executes -----", n);
            })
        } else {
            None
        }
    }

    impl Distribution<TokenKind> for Standard {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TokenKind {
            match rng.gen_range(0, 50) {
                // 0 => TokenKind::Do,
                // 1 => TokenKind::End,
                // 2 => TokenKind::Fn,
                // 3 => TokenKind::If,
                // 4 => TokenKind::Else,
                // 5 => TokenKind::While,
                // 6 => TokenKind::For,
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
                // 20 => TokenKind::Comma,
                // 21 => TokenKind::Period,
                // 22 => TokenKind::ExclusiveRange,
                // 23 => TokenKind::InclusiveRange,
                24 => TokenKind::Assign,
                25 => TokenKind::Not,
                26 => TokenKind::Plus,
                27 => TokenKind::Minus,
                28 => TokenKind::Multiply,
                29 => TokenKind::Divide,
                30 => TokenKind::Modulo,
                // 31 => TokenKind::LogicalAnd,
                // 32 => TokenKind::LogicalOr,
                // 33 => TokenKind::BitwiseAnd,
                // 34 => TokenKind::BitwiseOr,
                // 35 => TokenKind::BitwiseXor,
                36 => TokenKind::Equal,
                37 => TokenKind::NotEqual,
                38 => TokenKind::Less,
                39 => TokenKind::Greater,
                40 => TokenKind::LessEqual,
                41 => TokenKind::GreaterEqual,
                // 42 => TokenKind::ShiftLeft,
                // 43 => TokenKind::ShiftRight,
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
    use super::{Chunk, Emitter, Machine, Parser, Scanner, Token, TokenKind};
    use crate::compiler::ast::{AstPrinter, Expr, ExprAccept, Stmt};
    use crate::Constant;

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
        let ast = Parser::new().parse(&mut scanner).unwrap();
        println!("{}", AstPrinter::print(&ast));
        let mut ne = Emitter::new(Chunk::default());
        let chunk = ne.compile(&ast).unwrap();
        #[cfg(feature = "pc-debug")]
        {
            chunk.disassemble("idklol");
        }
        let mut vm = Machine::new();
        println!("{}", vm.interpret(&chunk).unwrap());
    }

    #[test]
    fn visitor_emitter() {
        let src = "1+2*3+4";
        let mut scanner = Scanner::new(src);
        let ast = Parser::new().parse(&mut scanner).unwrap();
        if let Stmt::Expr(expr) = &ast[0] {
            assert_eq!(
                expr,
                &Expr::Binary {
                    lhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(1), "1", 1))),
                        op: Token::new(TokenKind::Plus, "+", 1),
                        rhs: Box::new(Expr::Binary {
                            lhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(2), "2", 1))),
                            op: Token::new(TokenKind::Multiply, "*", 1),
                            rhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(3), "3", 1)))
                        })
                    }),
                    op: Token::new(TokenKind::Plus, "+", 1),
                    rhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(4), "4", 1))),
                }
            );

            let mut ne = Emitter::new(Chunk::default());
            println!("{}", AstPrinter::print_expr(expr));
            expr.accept(&mut ne).unwrap();
            #[cfg(feature = "pc-debug")]
            {
                ne.chunk().disassemble("idklol");
            }
            let mut vm = Machine::new();
            assert_eq!(vm.interpret(ne.chunk()).unwrap(), Constant::Integer(11));
        } else {
            panic!("ast not initialized")
        }
    }
}
