use crate::compiler::scanner::Token;

use super::expr::Expr;

pub trait StmtAccept {
    fn accept<T: StmtVisitor>(&self, visitor: &mut T) -> T::Output;
}

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
        args: &[&Token],
        arity: super::Arity,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;
    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> Self::Output;
    fn visit_data(
        &mut self,
        name: &Token,
        methods: &[Stmt],
        fields: &[(&Token, Expr)],
    ) -> Self::Output;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt<'a: 'b, 'b> {
    StmtExpr {
        expr: Expr<'a, 'b>,
    },
    Assignment {
        name: &'b Token<'a>,
        value: Expr<'a, 'b>,
    },
    Block {
        stmts: Vec<Stmt<'a, 'b>>,
    },
    If {
        cond: Expr<'a, 'b>,
        then: Vec<Stmt<'a, 'b>>,
        else_: Option<Vec<Stmt<'a, 'b>>>,
    },
    While {
        cond: Expr<'a, 'b>,
        body: Vec<Stmt<'a, 'b>>,
    },
    For {
        name: &'b Token<'a>,
        iter: Expr<'a, 'b>,
        body: Vec<Stmt<'a, 'b>>,
    },
    Func {
        name: &'b Token<'a>,
        args: Vec<&'b Token<'a>>,
        arity: super::Arity,
        body: Vec<Stmt<'a, 'b>>,
        method: bool,
    },
    Retn {
        keyword: &'b Token<'a>,
        value: Option<Expr<'a, 'b>>,
    },
    Data {
        name: &'b Token<'a>,
        methods: Vec<Stmt<'a, 'b>>,
        fields: Vec<(&'b Token<'a>, Expr<'a, 'b>)>,
    },
}

impl StmtAccept for Stmt<'_, '_> {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Stmt::StmtExpr { expr } => v.visit_expr(expr),
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
