#![allow(dead_code)]

pub mod expr;
pub mod stmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    None,
    Multi,
    Some(usize),
}

use crate::compiler::Token;
use expr::ExprAccept;
use stmt::StmtAccept;

#[derive(Copy, Clone)]
pub struct AstPrinter;

impl AstPrinter {
    pub fn print_expr(&mut self, expr: &expr::Expr) -> String {
        expr.accept(&mut *self)
    }

    pub fn print_stmt(&mut self, stmt: &stmt::Stmt) -> String {
        stmt.accept(&mut *self)
    }

    pub fn print(&mut self, ast: &[stmt::Stmt]) -> String {
        let mut s = String::new();
        for stmt in ast.iter() {
            s.push_str(&stmt.accept(&mut *self));
            s.push_str("\n");
        }
        s
    }

    fn parenthesize(&mut self, name: &str, expressions: &[&expr::Expr]) -> String {
        let mut s = format!("({}", name);
        for expr in expressions {
            s.push_str(" ");
            s.push_str(&expr.accept(&mut *self));
        }
        s.push_str(")");
        s
    }

    fn parenthesize_list(
        &mut self,
        name: &str,
        expr: Option<&expr::Expr>,
        stmts: &[stmt::Stmt],
    ) -> String {
        let mut s = format!("({}", name);
        if expr.is_some() {
            s.push_str(" ");
            s.push_str(&expr.as_ref().unwrap().accept(&mut *self));
        }
        for stmt in stmts {
            s.push_str(" ");
            s.push_str(&stmt.accept(&mut *self));
        }
        s.push_str(")");
        s
    }

    fn parenthesize_lists(
        &mut self,
        name: &str,
        e: Option<&expr::Expr>,
        stmts: &[&[stmt::Stmt]],
    ) -> String {
        let mut s = format!("({}", name);
        if e.is_some() {
            s.push_str(" ");
            s.push_str(&e.as_ref().unwrap().accept(&mut *self));
        }
        for stmt_list in stmts.iter() {
            for stmt in stmt_list.iter() {
                s.push_str(" ");
                s.push_str(&stmt.accept(&mut *self));
            }
        }
        s.push_str(")");
        s
    }
}

impl stmt::StmtVisitor for AstPrinter {
    type Output = String;

    fn visit_expr(&mut self, expr: &expr::Expr) -> String {
        self.parenthesize("expr", &[&expr])
    }

    fn visit_assignment(&mut self, name: &Token, value: &expr::Expr) -> String {
        self.parenthesize(&format!("= {}", name.lexeme), &[value])
    }

    fn visit_block(&mut self, stmts: &[stmt::Stmt]) -> String {
        let mut s = String::from("(block");
        for stmt in stmts.iter() {
            s.push_str(" ");
            s.push_str(&stmt.accept(&mut *self));
        }
        s.push_str(")");
        s
    }

    fn visit_if(
        &mut self,
        cond: &expr::Expr,
        then: &[stmt::Stmt],
        else_: Option<&Vec<stmt::Stmt>>,
    ) -> String {
        if let Some(else_) = else_ {
            self.parenthesize_lists("if-else", Some(cond), &[then, else_])
        } else {
            self.parenthesize_list("if", Some(cond), then)
        }
    }

    fn visit_while(&mut self, cond: &expr::Expr, body: &[stmt::Stmt]) -> String {
        self.parenthesize_list("while", Some(cond), body)
    }

    fn visit_for(&mut self, name: &Token, iter: &expr::Expr, body: &[stmt::Stmt]) -> String {
        self.parenthesize_list(&format!("for {} in ", name.lexeme), Some(iter), body)
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[&Token],
        _arity: Arity,
        body: &[stmt::Stmt],
        _method: bool,
    ) -> String {
        let mut s = format!("fn {} (", name.lexeme);
        for (n, arg) in args.iter().enumerate() {
            if n + 1 != args.len() {
                s.push_str(&format!("{} ", arg.lexeme));
            } else {
                s.push_str(arg.lexeme);
            }
        }
        s.push_str(")");
        self.parenthesize_list(&s, None, body)
    }
    fn visit_retn(&mut self, _keyword: &Token, value: Option<&expr::Expr>) -> String {
        self.parenthesize(
            "retn",
            &[value.unwrap_or(&expr::Expr::Literal(expr::Literal::Nil))],
        )
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[stmt::Stmt],
        _fields: &[(&Token, expr::Expr)],
    ) -> String {
        self.parenthesize("data", &[])
    }
}

impl expr::ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_literal(&mut self, literal: &expr::Literal) -> String {
        match literal {
            &expr::Literal::Nil => "nil".into(),
            expr => format!("{:?}", expr),
        }
    }

    fn visit_unary(&mut self, op: &Token, rhs: &expr::Expr) -> String {
        self.parenthesize(op.lexeme, &[rhs])
    }

    fn visit_binary(&mut self, lhs: &expr::Expr, op: &Token, rhs: &expr::Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_paren(&mut self, value: &expr::Expr) -> String {
        self.parenthesize("paren", &[value])
    }

    fn visit_variable(&mut self, name: &Token) -> String {
        String::from(name.lexeme)
    }

    fn visit_assign(&mut self, name: &Token, value: &expr::Expr) -> String {
        self.parenthesize(&format!("= {}", name.lexeme), &[value])
    }

    fn visit_logical(&mut self, lhs: &expr::Expr, op: &Token, rhs: &expr::Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_call(
        &mut self,
        callee: &expr::Expr,
        _paren: &Token,
        _arity: Arity,
        args: &[expr::Expr],
    ) -> String {
        let s = format!("call {}", callee.accept(&mut *self));
        let args: Vec<&expr::Expr> = args.iter().map(|arg| arg).collect();
        self.parenthesize(&s, &args)
    }

    fn visit_new(&mut self, name: &Token, args: &[(&Token, Box<expr::Expr>)]) -> String {
        let args: Vec<&expr::Expr> = args.iter().map(|tb| tb.1.as_ref()).collect();
        self.parenthesize(&format!("new {}", name.lexeme), &args)
    }

    fn visit_get(&mut self, object: &expr::Expr, name: &Token) -> String {
        self.parenthesize(&format!("get {}", name.lexeme), &[object])
    }

    fn visit_set(&mut self, object: &expr::Expr, name: &Token, _value: &expr::Expr) -> String {
        self.parenthesize(&format!("set {}", name.lexeme), &[object])
    }

    fn visit_index(&mut self, _rb: &Token, object: &expr::Expr, idx: &expr::Expr) -> String {
        self.parenthesize(&format!("index {:?}", idx), &[object])
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[&Token],
        _arity: Arity,
        body: &[stmt::Stmt],
        _method: bool,
    ) -> String {
        let mut s = format!("fn {} (", name.lexeme);
        for (n, arg) in args.iter().enumerate() {
            if n + 1 != args.len() {
                s.push_str(&format!("{} ", arg.lexeme));
            } else {
                s.push_str(arg.lexeme);
            }
        }
        s.push_str(")");
        self.parenthesize_list(&s, None, body)
    }
}
