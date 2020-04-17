use crate::Token;

use super::expr::Expr;

/// A visitor tells some node in an AST to accept the visitor, and that node
/// tells the visitor to visit its own node and its child nodes.
///
/// Only Piccolo AST nodes will implement this trait, but you will need to use it
/// in order to walk the AST.
pub trait StmtAccept {
    fn accept<T: StmtVisitor>(&self, visitor: &mut T) -> T::Output;
}

/// A struct wishing to walk the AST will need to implement StmtVisitor. This allows the
/// AST to tell the visitor to visit each node.
pub trait StmtVisitor {
    type Output;
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;
    fn visit_assignment(&mut self, name: &Token, value: &Expr) -> Self::Output;
    fn visit_block(&mut self, stmts: &[Stmt]) -> Self::Output;
    fn visit_if(&mut self, cond: &Expr, then: &[Stmt], else_: Option<&Vec<Stmt>>) -> Self::Output;
    fn visit_while(&mut self, cond: &Expr, body: &[Stmt]) -> Self::Output;
    fn visit_for(&mut self, name: &Token, iter: &Expr, body: &[Stmt]) -> Self::Output;
    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: super::Arity,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;
    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> Self::Output;
    fn visit_data(
        &mut self,
        name: &Token,
        methods: &[Stmt],
        fields: &[(Token, Expr)],
    ) -> Self::Output;
}

/// Piccolo AST node. Roughly corresponds to statements.
#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr {
        expr: Expr<'a>,
    },
    Assignment {
        name: Token<'a>,
        value: Expr<'a>,
    },
    Block {
        stmts: Vec<Stmt<'a>>,
    },
    If {
        cond: Expr<'a>,
        then: Vec<Stmt<'a>>,
        else_: Option<Vec<Stmt<'a>>>,
    },
    While {
        cond: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    For {
        name: Token<'a>,
        iter: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    Func {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: super::Arity,
        body: Vec<Stmt<'a>>,
        method: bool,
    },
    Retn {
        keyword: Token<'a>,
        value: Option<Expr<'a>>,
    },
    Data {
        name: Token<'a>,
        methods: Vec<Stmt<'a>>,
        fields: Vec<(Token<'a>, Expr<'a>)>,
    },
}

impl StmtAccept for Stmt<'_> {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Stmt::Expr { expr } => v.visit_expr(expr),
            Stmt::Assignment { name, value } => v.visit_assignment(name, value),
            Stmt::Block { stmts } => v.visit_block(stmts),
            Stmt::If { cond, then, else_ } => v.visit_if(cond, then, else_.as_ref()),
            Stmt::While { cond, body } => v.visit_while(cond, body),
            Stmt::For { name, iter, body } => v.visit_for(name, iter, body),
            Stmt::Func {
                name,
                args,
                arity,
                body,
                method,
            } => v.visit_func(name, args, *arity, body, *method),
            Stmt::Retn { keyword, value } => v.visit_retn(keyword, value.as_ref()),
            Stmt::Data {
                name,
                methods,
                fields,
            } => v.visit_data(name, methods, fields),
        }
    }
}