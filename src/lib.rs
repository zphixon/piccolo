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
pub use compiler::Compiler;
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
    match Machine::new(Compiler::compile(
        Chunk::default(),
        &Scanner::new(src).scan_tokens()?,
    )?)
    .interpret()
    {
        Ok(v) => Ok(v),
        Err(e) => Err(vec![e]),
    }
}

#[cfg(feature = "fuzzer")]
pub mod fuzzer {
    extern crate rand;

    use crate::chunk::Chunk;
    use crate::compiler::Compiler;
    use crate::machine::Machine;
    use crate::scanner;
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
        let dummy_lexeme = "id";
        let mut tokens: Vec<Token> = Vec::new();
        let mut r = rand::thread_rng();
        let lines = r.gen_range(min_len, max_len);
        for (_, line) in (1..lines).enumerate() {
            tokens.push(Token::new(r.gen(), dummy_lexeme, line));
        }
        tokens.push(Token::new(TokenKind::Eof, dummy_lexeme, lines));
        if let Ok(chunk) = Compiler::compile(Chunk::default(), &tokens) {
            println!("----- run {} compiles -----", n);
            scanner::print_tokens(&tokens);
            chunk.disassemble("");
            Machine::new(chunk).interpret().ok().map(|_| {})
        } else {
            None
        }
    }

    impl Distribution<TokenKind> for Standard {
        fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> TokenKind {
            match rng.gen_range(0, 25) {
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
                6 => TokenKind::Newline,
                7 => TokenKind::Not,
                8 => TokenKind::Plus,
                9 => TokenKind::Minus,
                10 => TokenKind::Multiply,
                11 => TokenKind::Divide,
                12 => TokenKind::Modulo,
                13 => TokenKind::LogicalAnd,
                14 => TokenKind::LogicalOr,
                //34 => TokenKind::BitwiseAnd,
                //35 => TokenKind::BitwiseOr,
                //36 => TokenKind::BitwiseXor,
                15 => TokenKind::Equal,
                16 => TokenKind::NotEqual,
                17 => TokenKind::Less,
                18 => TokenKind::Greater,
                19 => TokenKind::LessEqual,
                20 => TokenKind::GreaterEqual,
                //43 => TokenKind::ShiftLeft,
                //44 => TokenKind::ShiftRight,
                21 => TokenKind::Identifier,
                22 => TokenKind::String(String::from("yee")),
                23 => TokenKind::True,
                24 => TokenKind::False,
                25 => TokenKind::Double(0.0),
                26 => TokenKind::Integer(1),
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
    use crate::scanner::{Token, TokenKind};
    use crate::value::{Object, Value};
    use crate::{interpret, Compiler, Scanner};

    use slotmap::{DefaultKey, DenseSlotMap};

    use std::cell::RefCell;
    use std::rc::Rc;

    #[test]
    fn comparison() {
        assert!(interpret("1 < 2").unwrap().into::<bool>());
        assert!(interpret("1 <= 2").unwrap().into::<bool>());
        assert!(interpret("2 > 1").unwrap().into::<bool>());
        assert!(interpret("2 >= 1").unwrap().into::<bool>());
        assert!(interpret("2 == 2").unwrap().into::<bool>());
        assert!(interpret("3 != 2").unwrap().into::<bool>());
        assert!(interpret("\"a\" == \"a\"").unwrap().into::<bool>());
        assert!(interpret("\"a\" != \"b\"").unwrap().into::<bool>());
    }

    #[test]
    fn concat() {
        let mut c = Chunk::default();
        let s1 = c.make_constant(Value::String("ye".into()));
        let s2 = c.make_constant(Value::String("et".into()));

        c.write(Opcode::Constant, 1);
        c.write(s1 as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(s2 as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
    }

    #[test]
    fn precedence_ord() {
        assert!(Precedence::And > Precedence::Or);
    }

    #[test]
    fn math_multiply_add() {
        let mut c = Chunk::default();
        let one = c.make_constant(Value::Double(1.0));
        let two = c.make_constant(Value::Double(2.0));
        let three = c.make_constant(Value::Double(3.0));

        // 1 * 2 + 3

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
    }

    #[test]
    fn math_add_multiply() {
        let mut c = Chunk::default();
        let one = c.make_constant(Value::Double(1.0));
        let two = c.make_constant(Value::Double(2.0));
        let three = c.make_constant(Value::Double(3.0));

        // 1 + 2 * 3

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
    }

    #[test]
    fn math_sub_sub() {
        let mut c = Chunk::default();
        let one = c.make_constant(Value::Double(1.0));
        let two = c.make_constant(Value::Double(2.0));
        let three = c.make_constant(Value::Double(3.0));

        // 3 - 2 - 1

        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
    }

    #[test]
    fn math_complex() {
        let mut c = Chunk::default();
        let one = c.make_constant(Value::Double(1.0));
        let two = c.make_constant(Value::Double(2.0));
        let three = c.make_constant(Value::Double(3.0));
        let four = c.make_constant(Value::Double(4.0));
        let five = c.make_constant(Value::Double(5.0));

        // 1 + 2 * 3 - 4 / -5

        c.write(Opcode::Constant, 1);
        c.write(one as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(two as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(three as u8, 1);
        c.write(Opcode::Multiply, 1);
        c.write(Opcode::Constant, 1);
        c.write(four as u8, 1);
        c.write(Opcode::Constant, 1);
        c.write(five as u8, 1);
        c.write(Opcode::Negate, 1);
        c.write(Opcode::Divide, 1);
        c.write(Opcode::Subtract, 1);
        c.write(Opcode::Add, 1);
        c.write(Opcode::Return, 1);

        let mut vm = Machine::new(c);
        vm.interpret().unwrap();
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
}
