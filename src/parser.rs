
use token::{Token, TokenKind};
use ast::*;

#[derive(Debug)]
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
    line: u64,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            current: 0,
            tokens,
            line: 1,
        }
    }

    pub fn parse(mut self) -> Result<Ast, String> {
        Err("unimplemented".into())
    }
}

