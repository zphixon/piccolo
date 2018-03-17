//use ::*;
use super::*;

pub trait ExprAccept {
    fn accept<T: ExprVisitor>(&self, visitor: &mut T) -> T::Output;
}

pub trait ExprVisitor {
    type Output;
    fn visit_binary(&mut self, e: &Binary) -> Self::Output;
    fn visit_unary(&mut self, e: &Unary) -> Self::Output;
    fn visit_paren(&mut self, e: &Paren) -> Self::Output;
    fn visit_literal(&mut self, e: &Literal) -> Self::Output;
    fn visit_variable(&mut self, e: &Variable) -> Self::Output;
    fn visit_assign(&mut self, e: &Assignment) -> Self::Output;
    fn visit_logical(&mut self, e: &Logical) -> Self::Output;
    fn visit_call(&mut self, e: &Call) -> Self::Output;
    fn visit_new(&mut self, e: &New) -> Self::Output;
    fn visit_get(&mut self, e: &Get) -> Self::Output;
    fn visit_set(&mut self, e: &Set) -> Self::Output;
    fn visit_index(&mut self, e: &Index) -> Self::Output;
    fn visit_func(&mut self, e: &Func) -> Self::Output;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Array(Array),
    Nil,
}

impl ExprAccept for Literal {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_literal(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
    pub len: usize,
    pub inner: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ERange {
    pub from: Box<Expr>,
    pub to: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IRange {
    pub from: Box<Expr>,
    pub to: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: token::Token,
    pub rhs: Box<Expr>,
}

impl ExprAccept for Binary {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_binary(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: token::Token,
    pub rhs: Box<Expr>,
}

impl ExprAccept for Unary {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_unary(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Paren(pub Box<Expr>);

impl ExprAccept for Paren {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_paren(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable(pub token::Token);

impl ExprAccept for Variable {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_variable(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub name: token::Token,
    pub value: Box<Expr>,
}

impl ExprAccept for Assignment {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_assign(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Logical {
    pub lhs: Box<Expr>,
    pub op: token::Token,
    pub rhs: Box<Expr>,
}

impl ExprAccept for Logical {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_logical(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub paren: token::Token,
    pub arity: func::Arity,
    pub args: Vec<Expr>,
}

impl ExprAccept for Call {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_call(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct New {
    pub name: token::Token,
    pub args: Vec<(String, Expr)>,
}

impl ExprAccept for New {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_new(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Get {
    pub object: Box<Expr>,
    pub name: token::Token,
}

impl ExprAccept for Get {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_get(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: token::Token,
    pub value: Box<Expr>,
}

impl ExprAccept for Set {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_set(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Index {
    pub rb: token::Token,
    pub object: Box<Expr>,
    pub i: Box<Expr>,
}

impl ExprAccept for Index {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_index(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub name: token::Token,
    pub args: Vec<token::Token>,
    pub arity: func::Arity,
    pub body: Vec<stmt::Stmt>,
    pub method: bool,
}

impl ExprAccept for Func {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_func(self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(Literal),
    Paren(Paren),
    Variable(Variable),
    Assignment(Assignment),
    Logical(Logical),
    Call(Call),
    New(New),
    Get(Get),
    Set(Set),
    Index(Index),
    Func(Func),
}

impl ExprAccept for Expr {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        match *self {
            Expr::Binary(ref e) => e.accept(v),
            Expr::Unary(ref e) => e.accept(v),
            Expr::Literal(ref e) => e.accept(v),
            Expr::Paren(ref e) => e.accept(v),
            Expr::Variable(ref e) => e.accept(v),
            Expr::Assignment(ref e) => e.accept(v),
            Expr::Logical(ref e) => e.accept(v),
            Expr::Call(ref e) => e.accept(v),
            Expr::New(ref e) => e.accept(v),
            Expr::Get(ref e) => e.accept(v),
            Expr::Set(ref e) => e.accept(v),
            Expr::Index(ref e) => e.accept(v),
            Expr::Func(ref e) => e.accept(v),
        }
    }
}

use std::fmt;
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", AstPrinter.print_expr(self))
    }
}
