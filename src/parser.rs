
use token::{Token, TokenKind};

#[derive(Debug)]
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
    line: u64,
}

