#![feature(get_type_id)]
#![allow(unknown_lints)]

#[macro_use]
extern crate piccolo_derive;

pub mod data;
pub mod env;
pub mod err;
pub mod expr;
pub mod foreign;
pub mod func;
pub mod interp;
pub mod parser;
pub mod scanner;
pub mod stdlib;
pub mod stmt;
pub mod token;
pub mod value;

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

pub fn evaluate(data: &str) -> Result<value::Value, Vec<err::PiccoloError>> {
    let s = scanner::Scanner::new(data.into())
        .scan_tokens()
        .map_err(|e| vec![err::PiccoloError::hack(&e)])?;
    let p = parser::Parser::new(s).parse()?;
    interp::Interpreter::new()
        .interpret(&p)
        .map(|o| o.unwrap_or(value::Value::Nil))
        .map_err(|e| vec![e])
}

pub fn new_native_func(_arity: func::Arity, func: func::NativeFuncType) -> value::Value {
    value::Value::Foreign(foreign::ForeignOuter::new(func::ForeignFunc {
        inner: func,
    }))
    //value::Value::Func(func::Func::new_native(arity, func::NativeFunc::new(func)))
}

//pub fn new_native_method(arity: func::Arity, func: func::NativeFuncType) -> value::Value {
//    value::Value::Func(func::Func::new_native(
//        arity,
//        func::NativeFunc::new(func).method(),
//    ))
//}

#[derive(Copy, Clone)]
pub struct AstPrinter;

impl AstPrinter {
    pub fn print_expr(&mut self, ast: &expr::Expr) -> String {
        ast.accept(&mut *self)
    }

    pub fn print_stmt(&mut self, ast: &stmt::Stmt) -> String {
        ast.accept(&mut *self)
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
        let mut s = String::from("(");
        s.push_str(name);
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

    fn parenthesize_lists(
        &mut self,
        name: &str,
        e: Option<&expr::Expr>,
        stmts: &[&[stmt::Stmt]],
    ) -> String {
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

use expr::ExprAccept;
use stmt::StmtAccept;

impl stmt::StmtVisitor for AstPrinter {
    type Output = String;

    fn visit_block(&mut self, s: &stmt::Block) -> String {
        let mut res = String::from("(block");
        for stmt in &s.0 {
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
            self.parenthesize_lists(
                "if-else",
                Some(&s.cond),
                &[&s.then, &s.clone().else_.unwrap()],
            )
        }
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
        self.parenthesize_list(&name, Some(&s.iter), &s.body)
    }

    fn visit_func(&mut self, s: &stmt::Func) -> String {
        let mut name = String::from("fn ");
        name.push_str(&s.name.lexeme);
        name.push_str(" (");
        for (n, arg) in s.args.iter().enumerate() {
            if n + 1 != s.args.len() {
                name.push_str(&format!("{} ", arg.lexeme));
            } else {
                name.push_str(&format!("{}", arg.lexeme));
            }
        }
        name.push_str(")");
        self.parenthesize_list(&name, None, &s.body)
    }

    fn visit_retn(&mut self, s: &stmt::Retn) -> String {
        self.parenthesize(
            "retn",
            &[
                s.value
                    .as_ref()
                    .unwrap_or(&expr::Expr::Literal(expr::Literal::Nil)),
            ],
        )
    }

    fn visit_data(&mut self, _s: &stmt::Data) -> String {
        self.parenthesize("data", &[])
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
            expr => format!("{:?}", expr),
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

    #[allow(map_clone)]
    fn visit_call(&mut self, e: &expr::Call) -> String {
        let args: Vec<&expr::Expr> = e.args.iter().map(|x| x).collect();
        let mut name = String::from("call ");
        name.push_str(&e.callee.accept(&mut *self));
        self.parenthesize(&name, &args)
    }

    fn visit_new(&mut self, e: &expr::New) -> String {
        let mut name = String::from("new ");
        name.push_str(&e.name.lexeme);
        let args: Vec<&expr::Expr> = e.args.iter().map(|x| &x.1).collect();
        self.parenthesize(&name, &args)
    }

    fn visit_get(&mut self, e: &expr::Get) -> String {
        let mut name = String::from("get ");
        name.push_str(&e.name.lexeme);
        self.parenthesize(&name, &[&*e.object])
    }

    fn visit_set(&mut self, e: &expr::Set) -> String {
        let mut name = String::from("set ");
        name.push_str(&e.name.lexeme);
        self.parenthesize(&name, &[&*e.object])
    }

    fn visit_index(&mut self, e: &expr::Index) -> String {
        let name = format!("index {:?}", e.i);
        self.parenthesize(&name, &[&*e.object])
    }

    fn visit_func(&mut self, s: &expr::Func) -> String {
        let mut name = String::from("fn ");
        //name.push_str(&s.name.lexeme);
        name.push_str(" (");
        for (n, arg) in s.args.iter().enumerate() {
            if n + 1 != s.args.len() {
                name.push_str(&format!("{} ", arg.lexeme));
            } else {
                name.push_str(&format!("{}", arg.lexeme));
            }
        }
        name.push_str(")");
        self.parenthesize_list(&name, None, &s.body)
    }
}
