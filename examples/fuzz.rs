extern crate piccolo;
extern crate rand;

use rand::Rng;
use piccolo::{Compiler, Chunk, Machine, Token};
use std::env::args;

fn main() {
    let args: Vec<String> = args().collect();
    let n = if args.len() == 1 {
        50
    } else {
        args[1].parse::<usize>().unwrap()
    };
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
            use piccolo::TokenKind;
            v.push(Token::new(TokenKind::Eof, s, n));
            piccolo::print_tokens(&v);
        }
    #[cfg(not(feature = "pc-debug"))]
        {
            compile_error!("fuzzer requires pc-debug feature")
        }
    if let Ok(c) = Compiler::compile(Chunk::default(), &v) {
        c.disassemble("");
        if let Ok(r) = Machine::new(c).interpret() {
            panic!("possibly invalid program compiles and runs");
        }
    } else {
    }
}
