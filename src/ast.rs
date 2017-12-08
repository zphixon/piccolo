
use token::Token;

pub trait Accept {
    fn accept<T: ExprVisitor>(&self, visitor: &mut T) -> T::Output;
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
    Nil,
    Range, // TODO
}

impl Accept for Literal {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_literal(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: Token,
    pub rhs: Box<Expr>,
}

impl Accept for Binary {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_binary(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: Token,
    pub rhs: Box<Expr>,
}

impl Accept for Unary {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_unary(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Paren(pub Box<Expr>);

impl Accept for Paren {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_paren(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(Literal),
    Paren(Paren),
}

impl Accept for Expr {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        match *self {
            Expr::Binary(ref e) => e.accept(v),
            Expr::Unary(ref e) => e.accept(v),
            Expr::Literal(ref e) => e.accept(v),
            Expr::Paren(ref e) => e.accept(v),
        }
    }
}

use std::fmt;
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", ::AstPrinter::new().print(self))
    }
}

