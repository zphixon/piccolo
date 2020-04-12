#![allow(dead_code)]

pub mod expr;
pub mod stmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    None,
    Multi,
    Some(usize),
}
