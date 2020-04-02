//! # Piccolo
//!
//! Piccolo is a small, light, high-pitched scripting language (eventually) intended
//! for embedding in Rust projects.

extern crate anyhow;
extern crate downcast_rs;
extern crate slotmap;
extern crate thiserror;

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

pub use anyhow::Error;
pub use anyhow::Result;

/// Interprets a Piccolo source and returns its result.
///
/// # Examples
///
/// ```rust
/// # fn main() -> Result<(), piccolo::Error> {
/// let result = piccolo::interpret("1 + 2")?;
/// assert_eq!(3, result.into::<i64>());
/// # Ok(())
/// # }
/// ```
pub fn interpret(src: &str) -> Result<value::Value> {
    Machine::new(Compiler::compile(
        Chunk::default(),
        &Scanner::new(src).scan_tokens()?,
    )?)
    .interpret()
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

    pub fn fuzz(n: usize) {
        for n in 1..=n {
            println!("run {} ---------------", n);
            run();
        }
    }

    fn run() {
        let s = "id";
        let mut v: Vec<Token> = Vec::new();
        let mut r = rand::thread_rng();
        let n = r.gen_range(5, 20);
        // occasionally creates valid programs
        for (_, i) in (1..n).enumerate() {
            v.push(Token::new(r.gen(), s, i));
        }

        #[cfg(feature = "pc-debug")]
        {
            v.push(Token::new(TokenKind::Eof, s, n));
            scanner::print_tokens(&v);
        }
        #[cfg(not(feature = "pc-debug"))]
        {
            compile_error!("fuzzer requires pc-debug feature")
        }
        if let Ok(c) = Compiler::compile(Chunk::default(), &v) {
            c.disassemble("");
            if let Ok(_) = Machine::new(c).interpret() {
                panic!("possibly invalid program compiles and runs");
            }
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
                13 => TokenKind::And,
                14 => TokenKind::Or,
                //34 => TokenKind::BitwiseAnd,
                //35 => TokenKind::BitwiseOr,
                //36 => TokenKind::BitwiseXor,
                15 => TokenKind::Equals,
                16 => TokenKind::NotEquals,
                17 => TokenKind::LessThan,
                18 => TokenKind::GreaterThan,
                19 => TokenKind::LessThanEquals,
                20 => TokenKind::GreaterThanEquals,
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
