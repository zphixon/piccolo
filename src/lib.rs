//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

extern crate downcast_rs;
extern crate slotmap;

mod chunk;
mod compiler;
mod error;
mod machine;
mod op;
mod scanner;
pub mod value;

pub use chunk::Chunk;
pub use compiler::compile;
pub use error::PiccoloError;
pub use machine::Machine;
pub use scanner::Scanner;
pub use scanner::Token;

#[cfg(feature = "pc-debug")]
pub use scanner::print_tokens;

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
pub fn interpret(src: &str) -> Result<value::Value, Vec<error::PiccoloError>> {
    match Machine::new(compile(
        Chunk::default(),
        Scanner::new(src),
        //&Scanner::new(src).scan_tokens()?,
    )?)
    .interpret()
    {
        Ok(v) => Ok(v),
        Err(e) => Err(vec![e]),
    }
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

    use crate::chunk::Chunk;
    use crate::machine::Machine;
    use crate::{scanner, Scanner};
    use crate::scanner::Token;
    use crate::scanner::TokenKind;

    use rand::distributions::{Distribution, Standard};
    use rand::Rng;

    /// Run `n` tests of random tokens.
    ///
    /// Panics:
    ///
    /// Panics if an invalid program compiles and runs without an error.
    pub fn fuzz(n: usize, min_len: usize, max_len: usize) {
        let start = std::time::Instant::now();
        let mut avg = 0.0;
        for n in 1..=n {
            let s = std::time::Instant::now();
            if let Some(_) = run(n, min_len, max_len) {
                panic!(
                    "run {}: possibly invalid program compiled and executed successfully",
                    n
                );
            }
            avg += (std::time::Instant::now() - s).as_secs_f64();
        }
        println!(
            "{} runs, in {:.8} sec ({:.8} avg per run)",
            n,
            (std::time::Instant::now() - start).as_secs_f64(),
            avg / n as f64
        );
    }

    // occasionally creates valid programs
    fn run(n: usize, min_len: usize, max_len: usize) -> Option<()> {
        //let dummy_lexeme = "id";
        //let mut tokens: Vec<Token> = Vec::new();

        let mut src = String::new();
        let mut r = rand::thread_rng();
        let lines = r.gen_range(min_len, max_len);
        for (_, line) in (1..lines).enumerate() {
            let tk: TokenKind = r.gen();
            src.push_str(&format!("{}", tk));
            //tokens.push(Token::new(r.gen(), dummy_lexeme, line));
        }
        //tokens.push(Token::new(TokenKind::Eof, dummy_lexeme, lines));

        if let Ok(chunk) = crate::compile(Chunk::default(), Scanner::new(&src)) {
            println!("----- run {} compiles -----", n);
            //scanner::print_tokens(&tokens);
            chunk.disassemble("");
            Machine::new(chunk).interpret().ok().map(|_| {})
        } else {
            None
        }
    }

    impl Distribution<TokenKind> for Standard {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TokenKind {
            match rng.gen_range(0, 26) {
                //0 => TokenKind::Do,
                //1 => TokenKind::End,
                //2 => TokenKind::Fn,
                //3 => TokenKind::If,
                //4 => TokenKind::Else,
                //5 => TokenKind::While,
                //6 => TokenKind::For,
                //7 => TokenKind::In,
                //8 => TokenKind::Data,
                //9 => TokenKind::Is,
                //10 => TokenKind::Me,
                //11 => TokenKind::New,
                //12 => TokenKind::Err,
                1 => TokenKind::Retn,
                //14 => TokenKind::Nil,
                //15 => TokenKind::LeftBracket,
                //16 => TokenKind::RightBracket,
                2 => TokenKind::LeftParen,
                3 => TokenKind::RightParen,
                //19 => TokenKind::Comma,
                //20 => TokenKind::Period,
                //21 => TokenKind::ExclusiveRange,
                //22 => TokenKind::InclusiveRange,
                4 => TokenKind::Assign,
                5 => TokenKind::Declare,
                6 => TokenKind::Not,
                7 => TokenKind::Plus,
                8 => TokenKind::Minus,
                9 => TokenKind::Multiply,
                10 => TokenKind::Divide,
                11 => TokenKind::Modulo,
                12 => TokenKind::LogicalAnd,
                13 => TokenKind::LogicalOr,
                //34 => TokenKind::BitwiseAnd,
                //35 => TokenKind::BitwiseOr,
                //36 => TokenKind::BitwiseXor,
                14 => TokenKind::Equal,
                15 => TokenKind::NotEqual,
                16 => TokenKind::Less,
                17 => TokenKind::Greater,
                18 => TokenKind::LessEqual,
                19 => TokenKind::GreaterEqual,
                //43 => TokenKind::ShiftLeft,
                //44 => TokenKind::ShiftRight,
                20 => TokenKind::Identifier,
                21 => TokenKind::String,
                22 => TokenKind::True,
                23 => TokenKind::False,
                24 => TokenKind::Double(0.0),
                25 => TokenKind::Integer(1),
                _ => TokenKind::Nil, //_ => TokenKind::Eof,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::chunk::Chunk;
    use crate::compiler::Precedence;
    use crate::machine::Machine;
    use crate::op::Opcode;
    use crate::scanner::TokenKind;
    use crate::{Scanner, Token};
    use std::io::Write;

    #[test]
    fn scanner() {
        let mut scanner = Scanner::new("let a = 3\nlet b = 4\nretn a + b");
        assert_eq!(
            scanner.next_token().unwrap(),
            &Token::new(TokenKind::Let, "let", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            &Token::new(TokenKind::Identifier, "a", 1)
        );
        assert_eq!(
            scanner.previous(),
            &Token::new(TokenKind::Let, "let", 1)
        );
        assert_eq!(
            scanner.next_token().unwrap(),
            &Token::new(TokenKind::Assign, "=", 1)
        );
        assert_eq!(scanner.previous(), &Token::new(TokenKind::Identifier, "a", 1));
    }

    #[test]
    fn get_line_from_index() {
        let mut c = Chunk::default();
        c.write(Opcode::Return, 1); // 0
        c.write(Opcode::Return, 1); // 1
        c.write(Opcode::Return, 1); // 2
        c.write(Opcode::Return, 1); // 3
        c.write(Opcode::Return, 1); // 4
        c.write(Opcode::Return, 1); // 5
        c.write(Opcode::Return, 2); // 6
        c.write(Opcode::Return, 2); // 7
        c.write(Opcode::Return, 2); // 8
        c.write(Opcode::Return, 2); // 9
        c.write(Opcode::Return, 2); // 10
        c.write(Opcode::Return, 3); // 11
        c.write(Opcode::Return, 3); // 12
        c.write(Opcode::Return, 3); // 13
        c.write(Opcode::Return, 3); // 14
        c.write(Opcode::Return, 4); // 15
        c.write(Opcode::Return, 4); // 16
        c.write(Opcode::Return, 4); // 17
        c.write(Opcode::Return, 4); // 18
        c.write(Opcode::Return, 5); // 19

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
    fn very_long() {
        let len = 2048;

        print!("generate... ");
        std::io::stdout().flush().unwrap();
        let mut source = String::new();
        for i in 0..len {
            source.push_str(&format!("let a{:04x}=\"{}\"\n", i, i));
        }
        for i in 0..len {
            source.push_str(&format!("let b{:04x}=a{:04x}\n", i, i));
        }
        for i in 0..len {
            source.push_str(&format!("a{:04x}=b{:04x}\n", len - i - 1, i));
        }
        for i in 0..len {
            source.push_str(&format!("retn a{:04x}\n", i));
        }

        print!("done\ncompile... ");
        std::io::stdout().flush().unwrap();
        let chunk = crate::compile(Chunk::default(), Scanner::new(&source)).unwrap();

        print!("done\nrun... ");
        std::io::stdout().flush().unwrap();
        Machine::new(chunk).interpret().unwrap();

        println!("done");
    }

    #[test]
    fn comparison() {
        assert!(crate::interpret("1 < 2").unwrap().into::<bool>());
        assert!(crate::interpret("1 <= 2").unwrap().into::<bool>());
        assert!(crate::interpret("2 > 1").unwrap().into::<bool>());
        assert!(crate::interpret("2 >= 1").unwrap().into::<bool>());
        assert!(crate::interpret("2 == 2").unwrap().into::<bool>());
        assert!(crate::interpret("3 != 2").unwrap().into::<bool>());
        assert!(crate::interpret("\"a\" == \"a\"").unwrap().into::<bool>());
        assert!(crate::interpret("\"a\" != \"b\"").unwrap().into::<bool>());
    }

    #[test]
    fn strings() {
        assert!(
            crate::interpret("\"hello \\\n           world\" == \"hello world\"")
                .unwrap()
                .into::<bool>()
        );
        assert!(crate::interpret("\"ye\" + \"et\" == \"yeet\"")
            .unwrap()
            .into::<bool>());
        assert!(!crate::interpret("\"ye\" + \"et\" == \"\\\"yeet\\\"\"")
            .unwrap()
            .into::<bool>());
    }

    #[test]
    fn precedence_ord() {
        assert!(Precedence::And > Precedence::Or);
    }

    #[test]
    fn math() {
        assert!(crate::interpret("1 + 2 == 3").unwrap().into::<bool>());
        assert!(crate::interpret("0.1 + 0.2 == 0.30000000000000004")
            .unwrap()
            .into::<bool>());
        assert!(crate::interpret("3 / 4. == 0.75").unwrap().into::<bool>());
    }
}
