extern crate piccolo;
extern crate rand;

use rand::Rng;
use piccolo::{Compiler, Chunk, Machine, Token};

fn main() {
    //for n in 1..=50 {
        //println!("run {} ---------------", n);
        run();
    //}
}

fn run() {
    let s = "id";
    let mut v: Vec<Token> = Vec::new();
    let mut r = rand::thread_rng();
    let n = r.gen_range(5, 20);
    for (_, i) in (1..n).enumerate() {
        v.push(Token::new(r.gen(), s, i));
    }

    #[cfg(feature = "pc-debug")]
        {
            use piccolo::TokenKind;
            v.push(Token::new(TokenKind::Eof, s, n));
            piccolo::print_tokens(&v);
        }
    let c = Compiler::compile(Chunk::default(), &v);
    if !c.is_err() {
        let mut r = Machine::new(c.unwrap());
        assert!(r.interpret().is_err());
    }
}
