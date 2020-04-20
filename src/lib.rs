//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

pub extern crate downcast_rs;

pub mod ast;
pub mod compiler;
pub mod error;
pub mod runtime;

pub use ast::Parser;
pub use compiler::{compile, emitter::Emitter, scan_all, scanner::Scanner, Token, TokenKind};
pub use error::{ErrorKind, PiccoloError};
pub use runtime::{chunk::Chunk, value::Value, vm::Machine};

#[cfg(feature = "pc-debug")]
pub use compiler::print_tokens;

/// Interprets a Piccolo source and returns its result.
///
/// # Examples
///
/// ```rust
/// # fn main() -> Result<(), Vec<piccolo::PiccoloError>> {
/// let result = piccolo::interpret("1 + 2")?;
/// assert_eq!(3, result.into::<i64>());
/// # Ok(())
/// # }
/// ```
pub fn interpret(src: &str) -> Result<Value, Vec<PiccoloError>> {
    let mut scanner = Scanner::new(src);
    let ast = Parser::new().parse(&mut scanner)?;
    let chunk = Emitter::new(Chunk::default()).compile(&ast)?;
    Ok(Machine::new(chunk).interpret()?)
}

/// Reads a file and interprets its contents.
pub fn do_file(file: &std::path::Path) -> Result<Value, Vec<PiccoloError>> {
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
            Machine::new(chunk).interpret().ok().map(|_| {
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
mod tests {
    use crate::ast::{Arity, AstPrinter, Expr, ExprAccept, Stmt};
    use crate::runtime::{op::Opcode, value::Value};
    use crate::{Chunk, Emitter, Machine, Parser, Scanner, Token, TokenKind};

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
        let mut vm = Machine::new(chunk);
        println!("{}", vm.interpret().unwrap());
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
            let mut vm = Machine::new(ne.chunk().clone());
            assert_eq!(vm.interpret().unwrap(), Value::Integer(11));
        } else {
            panic!("ast not initialized")
        }
    }

    #[test]
    fn scanner() {
        let src = "a = 3\nio.prln(a)\n";
        let mut scanner = Scanner::new(src);
        assert_eq!(
            scanner.peek_token(0).unwrap(),
            &Token::new(TokenKind::Identifier, "a", 1)
        );
        assert_eq!(
            scanner.peek_token(1).unwrap(),
            &Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Identifier, "a", 1)
        );

        assert_eq!(
            scanner.peek_token(0).unwrap(),
            &Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Integer(3), "3", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Identifier, "io", 2)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Period, ".", 2)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Identifier, "prln", 2)
        );
    }

    #[test]
    fn scanner2() {
        //               0 1 2  3 45   678
        let src = "a = 3\nio.prln(a)\n";
        let mut scanner = Scanner::new(src);

        assert_eq!(
            scanner.peek_token(0).unwrap(),
            &Token::new(TokenKind::Identifier, "a", 1)
        );
        assert_eq!(
            scanner.peek_token(1).unwrap(),
            &Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(
            scanner.peek_token(0).unwrap(),
            &Token::new(TokenKind::Identifier, "a", 1)
        );
        assert_eq!(
            scanner.peek_token(6).unwrap(),
            &Token::new(TokenKind::LeftParen, "(", 2)
        );
        assert_eq!(
            scanner.peek_token(8).unwrap(),
            &Token::new(TokenKind::RightParen, ")", 2)
        );

        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Identifier, "a", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Integer(3), "3", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            Token::new(TokenKind::Identifier, "io", 2)
        );
    }

    #[test]
    fn ast_print() {
        let source = "x =: 3\ny =: x - 4\nif x > y then\n  io.prln(3)\nend\n";
        let mut tokens = vec![
            Some(Token::new(TokenKind::Identifier, &source[0..1], 1)),
            Some(Token::new(TokenKind::Declare, &source[2..4], 1)),
            Some(Token::new(TokenKind::Integer(3), &source[5..6], 1)),
            Some(Token::new(TokenKind::Identifier, &source[7..8], 2)),
            Some(Token::new(TokenKind::Declare, &source[9..11], 2)),
            Some(Token::new(TokenKind::Identifier, &source[12..13], 2)),
            Some(Token::new(TokenKind::Minus, &source[14..15], 2)),
            Some(Token::new(TokenKind::Integer(3), &source[16..17], 2)),
            Some(Token::new(TokenKind::If, &source[18..19], 3)),
            Some(Token::new(TokenKind::Identifier, &source[21..22], 3)),
            Some(Token::new(TokenKind::Greater, &source[23..24], 3)),
            Some(Token::new(TokenKind::Identifier, &source[25..26], 3)),
            Some(Token::new(TokenKind::Nil, &source[27..31], 3)),
            Some(Token::new(TokenKind::Identifier, &source[34..36], 4)),
            Some(Token::new(TokenKind::Period, &source[36..37], 4)),
            Some(Token::new(TokenKind::Identifier, &source[37..41], 4)),
            Some(Token::new(TokenKind::LeftParen, &source[41..42], 4)),
            Some(Token::new(TokenKind::Integer(3), &source[42..43], 4)),
            Some(Token::new(TokenKind::RightParen, &source[43..44], 4)),
            Some(Token::new(TokenKind::End, &source[45..48], 5)),
        ];

        let ast = vec![
            Stmt::Assignment {
                name: tokens[0].take().unwrap(),
                op: tokens[1].take().unwrap(),
                value: Expr::Atom(Token::new(TokenKind::Integer(3), "3", 1)),
            },
            Stmt::Assignment {
                name: tokens[3].take().unwrap(),
                op: tokens[4].take().unwrap(),
                value: Expr::Binary {
                    lhs: Box::new(Expr::Variable(tokens[5].take().unwrap())),
                    op: tokens[6].take().unwrap(),
                    rhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(4), "4", 2))),
                },
            },
            Stmt::If {
                cond: Expr::Binary {
                    lhs: Box::new(Expr::Variable(tokens[9].take().unwrap())),
                    op: tokens[10].take().unwrap(),
                    rhs: Box::new(Expr::Variable(tokens[11].take().unwrap())),
                },
                then: vec![Stmt::Expr(Expr::Call {
                    callee: Box::new(Expr::Get {
                        object: Box::new(Expr::Variable(tokens[13].take().unwrap())),
                        name: tokens[15].take().unwrap(),
                    }),
                    paren: tokens[16].take().unwrap(),
                    arity: Arity::Some(1),
                    args: vec![Expr::Atom(Token::new(TokenKind::Integer(3), "3", 4))],
                })],
                else_: None,
            },
        ];

        assert_eq!(
            "(=: x 3)\n(=: y (- x 4))\n(if (> x y)\n  (expr (call (get prln io) 3)))\n",
            AstPrinter::print(&ast)
        );
    }

    #[test]
    fn get_line_from_index() {
        let mut c = Chunk::default();
        c.write_u8(Opcode::Return, 1); // 0
        c.write_u8(Opcode::Return, 1); // 1
        c.write_u8(Opcode::Return, 1); // 2
        c.write_u8(Opcode::Return, 1); // 3
        c.write_u8(Opcode::Return, 1); // 4
        c.write_u8(Opcode::Return, 1); // 5
        c.write_u8(Opcode::Return, 2); // 6
        c.write_u8(Opcode::Return, 2); // 7
        c.write_u8(Opcode::Return, 2); // 8
        c.write_u8(Opcode::Return, 2); // 9
        c.write_u8(Opcode::Return, 2); // 10
        c.write_u8(Opcode::Return, 3); // 11
        c.write_u8(Opcode::Return, 3); // 12
        c.write_u8(Opcode::Return, 3); // 13
        c.write_u8(Opcode::Return, 3); // 14
        c.write_u8(Opcode::Return, 4); // 15
        c.write_u8(Opcode::Return, 4); // 16
        c.write_u8(Opcode::Return, 4); // 17
        c.write_u8(Opcode::Return, 4); // 18
        c.write_u8(Opcode::Return, 5); // 19

        assert_eq!(c.get_line_from_index(0), 1);
        assert_eq!(c.get_line_from_index(5), 1);
        assert_eq!(c.get_line_from_index(6), 2);
        assert_eq!(c.get_line_from_index(10), 2);
        assert_eq!(c.get_line_from_index(11), 3);
        assert_eq!(c.get_line_from_index(14), 3);
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
    #[ignore]
    fn very_long() {
        let path = std::path::Path::new("examples/long.pc");
        crate::do_file(path).unwrap();
    }

    #[test]
    fn precedence_ord() {
        use crate::ast::parser::BindingPower;
        assert!(BindingPower::And > BindingPower::Or);
    }
}
