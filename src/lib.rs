
pub mod parser;
pub mod token;
pub mod scanner;
pub mod expr;
pub mod stmt;
pub mod err;
pub mod interp;
pub mod value;
pub mod env;

use scanner::Scanner;

use std::fs::File;
use std::io::Read;

pub fn parse_file(filename: &str) -> Result<Vec<token::Token>, String> {
    if let Ok(mut file) = File::open(filename) {
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        Scanner::new(data).scan_tokens()
    } else {
        Err("could not read file".into())
    }
}

#[derive(Copy, Clone)]
pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, ast: &[stmt::Stmt]) -> String {
        let mut s = String::new();
        for stmt in ast.iter() {
            s.push_str(&stmt.accept(&mut *self));
            s.push_str(" ");
        }
        s
    }

    fn parenthesize(&mut self, name: &str, expressions: &[&expr::Expr]) -> String {
        let mut s = String::from("(");
        s.push_str(name);
        for expr in expressions {
            s.push_str(" ");
            s.push_str(&expr.accept(&mut *self));
        }
        s.push_str(")");
        s
    }

    fn parenthesize_list(&mut self, name: &str, expr: Option<&expr::Expr>, stmts: &[stmt::Stmt]) -> String {
        let mut s = String::from("(");
        s.push_str(name);
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

    fn parenthesize_lists(&mut self, name: &str, e: Option<&expr::Expr>, stmts: &[&[stmt::Stmt]]) -> String {
        let mut s = String::from("(");
        s.push_str(name);
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

use stmt::StmtAccept;
use expr::ExprAccept;

impl stmt::StmtVisitor for AstPrinter {
    type Output = String;

    fn visit_block(&mut self, s: &stmt::Block) -> String {
        let mut res = String::from("(block");
        for stmt in s.0.iter() {
            res.push_str(" ");
            res.push_str(&stmt.accept(&mut *self));
        }
        res.push_str(")");
        res
    }

    fn visit_expr(&mut self, s: &stmt::StmtExpr) -> String {
        self.parenthesize("expr", &[&s.0])
    }

    fn visit_if(&mut self, s: &stmt::If) -> String {
        if s.else_.is_none() {
            self.parenthesize_list("if", Some(&s.cond), &s.then)
        } else {
            self.parenthesize_lists("if-else", Some(&s.cond), &[&s.then, &s.clone().else_.unwrap()])
        }
    }

    fn visit_me_tmp(&mut self, s: &stmt::MeTmp) -> String {
        self.parenthesize("me", &[&s.0])
    }

    fn visit_assignment(&mut self, s: &stmt::Assignment) -> String {
        let mut name = String::from("= ");
        name.push_str(&s.name.lexeme);
        self.parenthesize(&name, &[&s.value])
    }

    fn visit_while(&mut self, s: &stmt::While) -> String {
        self.parenthesize_list("while", Some(&s.cond), &s.body)
    }

    fn visit_for(&mut self, s: &stmt::For) -> String {
        let mut name = String::from("for ");
        name.push_str(&s.name.lexeme);
        name.push_str(" in ");
        name.push_str(&s.iter.accept(&mut *self));
        self.parenthesize_list(&name, Some(&s.iter), &s.body)
    }
}

impl expr::ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_assign(&mut self, e: &expr::Assignment) -> String {
        let mut name = String::from("= ");
        name.push_str(&e.name.lexeme);
        self.parenthesize(&name, &[&e.value])
    }

    fn visit_binary(&mut self, e: &expr::Binary) -> String {
        self.parenthesize(&e.op.lexeme, &[&e.lhs, &e.rhs])
    }

    fn visit_paren(&mut self, e: &expr::Paren) -> String {
        self.parenthesize("paren", &[&e.0])
    }

    fn visit_literal(&mut self, e: &expr::Literal) -> String {
        match e {
            &expr::Literal::Nil => "nil".into(),
            expr => format!("{:?}", expr)
        }
    }

    fn visit_logical(&mut self, e: &expr::Logical) -> String {
        self.parenthesize(&e.op.lexeme, &[&e.lhs, &e.rhs])
    }

    fn visit_unary(&mut self, e: &expr::Unary) -> String {
        self.parenthesize(&e.op.lexeme, &[&e.rhs])
    }

    fn visit_variable(&mut self, e: &expr::Variable) -> String {
        e.0.lexeme.clone()
    }
}

