
use token::Token;

pub trait Accept {
    fn accept<T: ExprVisitor>(&self, visitor: &mut T) -> T;
}

pub trait ExprVisitor {
    type Output;
    fn visit_binary(&mut self, b: &Binary) -> Self::Output;
    fn visit_unary(&mut self, u: &Unary) -> Self::Output;
    fn visit_paren(&mut self, p: &Paren) -> Self::Output;
    fn visit_literal(&mut self, l: &Literal) -> Self::Output;
    //fn visit_fncall(&mut self, b: FnCall) -> T; // TODO
    //fn visit_me(&mut self, b: Me) -> T;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Range, // TODO
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: Token,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: Token,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Paren(pub Box<Expr>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(Literal),
    Paren(Paren),
}

pub fn walk_expr<T: ExprVisitor>(visitor: &mut T, e: &Expr) -> T::Output {
    match *e {
        Expr::Binary(ref e) => visitor.visit_binary(e),
        Expr::Unary(ref e) => visitor.visit_unary(e),
        Expr::Literal(ref e) => visitor.visit_literal(e),
        Expr::Paren(ref e) => visitor.visit_paren(e),
    }
}

use std::fmt;
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ::AstPrinter::new().print(self))
    }
}

