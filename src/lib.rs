//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate downcast_rs;
pub extern crate fnv;
#[macro_use]
pub extern crate log;

pub mod compiler;
pub mod error;
pub mod runtime;

/// Commonly used items that you might want access to.
pub mod prelude {
    pub use super::compiler::{
        ast::print_expression, emitter::Emitter, parser::parse, scanner::Scanner, Token, TokenKind,
    };
    pub use super::error::{ErrorKind, PiccoloError};
    pub use super::runtime::{
        chunk::Chunk,
        memory::{Gc, Heap, Object, Root, UniqueRoot},
        object::{Function, NativeFunction},
        value::Constant,
        value::Value,
        vm::Machine,
        ChunkIndex, ChunkOffset, ConstantIdx, Line, LocalSlotIdx,
    };
}

use prelude::*;

#[cfg(feature = "pc-debug")]
pub use compiler::{compile_chunk, scan_all};

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
    let ast = parse(&mut scanner)?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));
    debug!("compile");

    let mut emitter = compiler::emitter::Emitter::new();
    compiler::emitter::compile_ast(&mut emitter, &ast)?;
    let chunk = emitter.current_chunk();

    debug!("chunk\n{}", chunk.disassemble(""));
    debug!("interpret");

    Ok(Machine::new().interpret(&chunk)?)
}

pub fn interpret2(src: &str) -> Result<Constant, Vec<PiccoloError>> {
    debug!("parse");
    let ast = crate::compiler::parser::parse(&mut crate::compiler::scanner::Scanner::new(src))?;
    debug!("ast\n{}", compiler::ast::print_ast(&ast));

    debug!("compile");
    let mut emitter = crate::compiler::emitter::Emitter::new();
    crate::compiler::emitter::compile_ast(&mut emitter, &ast)?;
    let chunk = emitter.into_chunk();
    debug!("{}", chunk.disassemble(""));

    let constants = chunk.constants.clone();
    let mut module = runtime::vm2::Module::new(vec![chunk], constants);

    let mut heap = crate::runtime::memory::Heap::default();

    debug!("interpret");
    let mut vm = runtime::vm2::Vm2::new(&mut heap, &module);
    vm.interpret(&mut heap)?;
    Ok(Constant::Nil)
}

/// Reads a file and interprets its contents.
pub fn do_file(file: &std::path::Path) -> Result<Constant, Vec<PiccoloError>> {
    let contents = std::fs::read_to_string(file).map_err(|e| vec![PiccoloError::from(e)])?;
    interpret2(&contents).map_err(|v| {
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

        if let Ok(chunk) = crate::compile_chunk(&src) {
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
    use super::{parse, Emitter, Machine, Scanner, Token, TokenKind};
    use crate::compiler::ast::{self, Expr, Stmt};
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
        let ast = parse(&mut scanner).unwrap();
        println!("{}", ast::print_ast(&ast));
        let mut ne = Emitter::new();
        crate::compiler::emitter::compile_ast(&mut ne, &ast).unwrap();
        let chunk = ne.into_chunk();
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

            println!("got:  {}", ast::print_expression(expr));
            println!("want: {}", ast::print_expression(&equiv));
            assert_eq!(expr, &equiv);

            let mut ne = Emitter::new();
            crate::compiler::emitter::compile_ast(&mut ne, &ast).unwrap();
            let chunk = ne.into_chunk();

            #[cfg(feature = "pc-debug")]
            {
                println!("{}", chunk.disassemble("idklol"));
            }
            let mut vm = Machine::new();
            assert_eq!(vm.interpret(&chunk).unwrap(), Constant::Integer(11));
        } else {
            panic!("ast not initialized")
        }
    }
}
