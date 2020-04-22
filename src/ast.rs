#![allow(dead_code)]

pub mod expr;
pub mod stmt;

pub use expr::{Expr, ExprAccept, ExprVisitor};
pub use stmt::{Stmt, StmtAccept, StmtVisitor};

use crate::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    None,
    Multi,
    Some(usize),
}

/// Pretty-prints a Piccolo AST using the ExprVisitor and StmtVisitor traits.
#[derive(Copy, Clone)]
pub struct AstPrinter {
    indent: usize,
}

impl AstPrinter {
    /// Pretty-print a full AST.
    pub fn print(ast: &[Stmt]) -> String {
        let mut s = String::new();
        for stmt in ast.iter() {
            s.push_str(&stmt.accept(&mut AstPrinter { indent: 0 }));
            s.push_str("\n");
        }
        s
    }

    /// Pretty-print a single expression.
    pub fn print_expr(expr: &Expr) -> String {
        expr.accept(&mut AstPrinter { indent: 0 })
    }

    /// Pretty-print a single statement.
    pub fn print_stmt(stmt: &Stmt) -> String {
        stmt.accept(&mut AstPrinter { indent: 0 })
    }

    fn parenthesize(&mut self, name: &str, expressions: &[&Expr]) -> String {
        let mut s = format!("({}", name);
        for expr in expressions {
            s.push_str(" ");
            s.push_str(&expr.accept(self));
        }
        s.push_str(")");
        s
    }

    fn parenthesize_list(&mut self, name: &str, expr: Option<&Expr>, stmts: &[Stmt]) -> String {
        self.parenthesize_lists(name, expr, &[stmts])
    }

    fn parenthesize_lists(&mut self, name: &str, expr: Option<&Expr>, stmts: &[&[Stmt]]) -> String {
        self.indent += 1;
        let mut s = format!("({}", name);
        if expr.is_some() {
            s.push_str(" ");
            s.push_str(&expr.as_ref().unwrap().accept(self));
        }
        for stmt_list in stmts.iter() {
            for stmt in stmt_list.iter() {
                s.push_str("\n");
                for _ in 0..self.indent {
                    s.push_str("  ");
                }
                s.push_str(&stmt.accept(self));
            }
        }
        s.push_str(")");
        self.indent -= 1;
        s
    }
}

impl StmtVisitor for AstPrinter {
    type Output = String;

    fn visit_expr(&mut self, expr: &Expr) -> String {
        self.parenthesize("expr", &[&expr])
    }

    fn visit_block(&mut self, _do_: &Token, body: &[Stmt]) -> String {
        self.parenthesize_list("do", None, body)
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> String {
        self.parenthesize(&format!("{} {}", op.lexeme, name.lexeme), &[value])
    }

    fn visit_if(&mut self, cond: &Expr, then: &[Stmt], else_: Option<&Vec<Stmt>>) -> String {
        if let Some(else_) = else_ {
            self.parenthesize_lists("if-else", Some(cond), &[then, else_])
        } else {
            self.parenthesize_list("if", Some(cond), then)
        }
    }

    fn visit_while(&mut self, cond: &Expr, body: &[Stmt]) -> String {
        self.parenthesize_list("while", Some(cond), body)
    }

    fn visit_for(&mut self, name: &Token, iter: &Expr, body: &[Stmt]) -> String {
        self.parenthesize_list(&format!("for {} in ", name.lexeme), Some(iter), body)
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: Arity,
        body: &[Stmt],
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

    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> String {
        self.parenthesize(
            "retn",
            &[value.unwrap_or(&Expr::Atom(Token::new(TokenKind::Nil, "nil", keyword.line)))],
        )
    }

    fn visit_assert(&mut self, _keyword: &Token, value: &Expr) -> String {
        self.parenthesize("assert", &[value])
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> String {
        // TODO
        self.parenthesize("data", &[])
    }
}

impl ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_atom(&mut self, token: &Token) -> String {
        format!("{}", token)
    }

    fn visit_paren(&mut self, value: &Expr) -> String {
        self.parenthesize("paren", &[value])
    }

    fn visit_variable(&mut self, name: &Token) -> String {
        String::from(name.lexeme)
    }

    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[rhs])
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        _paren: &Token,
        _arity: Arity,
        args: &[Expr],
    ) -> String {
        let s = format!("call {}", callee.accept(self));
        let args: Vec<&Expr> = args.iter().map(|arg| arg).collect();
        self.parenthesize(&s, &args)
    }

    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> String {
        let args: Vec<&Expr> = args.iter().map(|tb| tb.1.as_ref()).collect();
        self.parenthesize(&format!("new {}", name.lexeme), &args)
    }

    fn visit_get(&mut self, object: &Expr, name: &Token) -> String {
        self.parenthesize(&format!("get {}", name.lexeme), &[object])
    }

    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> String {
        let s = format!("set {} = {}", name.lexeme, value.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_index(&mut self, _rb: &Token, object: &Expr, idx: &Expr) -> String {
        let s = format!("index {}", idx.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: Arity,
        body: &[Stmt],
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
