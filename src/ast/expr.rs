use crate::Token;

use super::stmt::Stmt;

/// A visitor tells some node in an AST to accept the visitor, and that node
/// tells the visitor to visit its own node and its child nodes.
///
/// Only Piccolo AST nodes will implement this trait, but you will need to use it
/// in order to walk the AST.
pub trait ExprAccept {
    fn accept<T: ExprVisitor>(&self, visitor: &mut T) -> T::Output;
}

/// A struct wishing to walk the AST will need to implement ExprVisitor. This allows the
/// AST to tell the visitor to visit each node.
pub trait ExprVisitor {
    type Output;
    fn visit_atom(&mut self, token: &Token) -> Self::Output;
    fn visit_paren(&mut self, value: &Expr) -> Self::Output;
    fn visit_variable(&mut self, name: &Token) -> Self::Output;
    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> Self::Output;
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output;
    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output;
    fn visit_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arity: super::Arity,
        args: &[Expr],
    ) -> Self::Output;
    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> Self::Output;
    fn visit_get(&mut self, object: &Expr, name: &Token) -> Self::Output;
    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Self::Output;
    fn visit_index(&mut self, rb: &Token, object: &Expr, idx: &Expr) -> Self::Output;
    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: super::Arity,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;
}

/// Piccolo AST node.
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Atom(Token<'a>),
    Paren(Box<Expr<'a>>),
    Variable(Token<'a>),
    Unary {
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Binary {
        lhs: Box<Expr<'a>>,
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Logical {
        lhs: Box<Expr<'a>>,
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Call {
        callee: Box<Expr<'a>>,
        paren: Token<'a>,
        arity: super::Arity,
        args: Vec<Expr<'a>>,
    },
    New {
        name: Token<'a>,
        args: Vec<(Token<'a>, Box<Expr<'a>>)>,
    },
    Get {
        object: Box<Expr<'a>>,
        name: Token<'a>,
    },
    Set {
        object: Box<Expr<'a>>,
        name: Token<'a>,
        value: Box<Expr<'a>>,
    },
    Index {
        rb: Token<'a>,
        object: Box<Expr<'a>>,
        idx: Box<Expr<'a>>,
    },
    Func {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: super::Arity,
        body: Vec<Stmt<'a>>,
        method: bool,
    },
}

#[rustfmt::skip]
impl ExprAccept for Expr<'_> {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Expr::Atom(token)
                => v.visit_atom(token),
            Expr::Paren(value)
                => v.visit_paren(value),
            Expr::Variable(name)
                => v.visit_variable(name),
            Expr::Unary { op, rhs }
                => v.visit_unary(op, rhs),
            Expr::Binary { lhs, op, rhs }
                => v.visit_binary(lhs, op, rhs),
            Expr::Logical { lhs, op, rhs }
                => v.visit_logical(lhs, op, rhs),
            Expr::Call { callee, paren, arity, args }
                => v.visit_call(callee, paren, *arity, args),
            Expr::New { name, args }
                => v.visit_new(name, args),
            Expr::Get { object, name }
                => v.visit_get(object, name),
            Expr::Set { object, name, value }
                => v.visit_set(object, name, value),
            Expr::Index { rb, object, idx }
                => v.visit_index(rb, object, idx),
            Expr::Func { name, args, arity, body, method }
                => v.visit_func(name, args, *arity, body, *method),
        }
    }
}
