
pub mod parser;
pub mod token;
pub mod scanner;
pub mod ast;
pub mod err;
pub mod interp;
pub mod value;

use scanner::Scanner;
use ast::Accept;

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
    pub fn new() -> Self {
        AstPrinter
    }

    pub fn print(mut self, e: &ast::Expr) -> String {
        e.accept(&mut self)
    }

    fn parenthesize(mut self, name: &str, l: &[&ast::Expr]) -> String {
        let mut s = String::from("(");
        s.push_str(name);

        for expr in l {
            s.push_str(" ");
            s.push_str(&expr.accept(&mut self));
        }
        s.push_str(")");

        s
    }
}

impl ast::ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_binary(&mut self, b: &ast::Binary) -> Self::Output {
        self.parenthesize(&b.op.lexeme, &[&b.lhs, &b.rhs])
    }

    fn visit_paren(&mut self, p: &ast::Paren) -> Self::Output {
        self.parenthesize("paren", &[&p.0])
    }

    fn visit_unary(&mut self, u: &ast::Unary) -> Self::Output {
        self.parenthesize(&u.op.lexeme, &[&u.rhs])
    }

    fn visit_literal(&mut self, l: &ast::Literal) -> Self::Output {
        if *l == ast::Literal::Nil {
            "nil".into()
        } else {
            format!("{:?}", l)
        }
    }
}

