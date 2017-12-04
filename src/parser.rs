
use token::{Token, TokenKind};
use ast::*;

#[derive(Debug)]
pub struct Parser {
    current: usize,
    tokens: Vec<Token>,
    line: u64,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            current: 0,
            tokens,
            line: 1,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, String> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, String> {
        self.math() // TODO: add more
    }

    fn math(&mut self) -> Result<Expr, String> {
        self.and()
    }

    fn and(&mut self) -> Result<Expr, String> {
        let expr = self.or()?;
        let mut r = Ok(expr.clone()); // TODO: figure out error handling
        //let mut r = Err("Expected &&".into());

        while self.matches(&[TokenKind::And]) {
            let op = self.previous();
            let right = self.or()?;
            r = Ok(Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        r
    }

    fn or(&mut self) -> Result<Expr, String> {
        let expr = self.equality()?;
        let mut r = Ok(expr.clone());
        //let mut r = Err("Expected ||".into());

        while self.matches(&[TokenKind::Or]) {
            let op = self.previous();
            let right = self.equality()?;
            r = Ok(Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        r
    }

    fn equality(&mut self) -> Result<Expr, String> {
        let expr = self.comparison()?;
        let mut r = Ok(expr.clone());
        //let mut r = Err("Expected ==, !=".into());

        while self.matches(&[TokenKind::BangEquals, TokenKind::Equals]) {
            let op = self.previous();
            let right = self.comparison()?;
            r = Ok(Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        r
    }

    fn comparison(&mut self) -> Result<Expr, String> {
        let expr = self.addition()?;
        let mut r = Ok(expr.clone());
        //let mut r = Err("Expected >, <, >=, <=".into());

        while self.matches(&[TokenKind::GreaterThan, TokenKind::GreaterThanEquals, TokenKind::LessThan, TokenKind::LessThanEquals]) {
            let op = self.previous();
            let right = self.addition()?;
            r = Ok(Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        r
    }

    fn addition(&mut self) -> Result<Expr, String> {
        let mut expr = self.multiplication()?;

        while self.matches(&[TokenKind::Hyphen, TokenKind::Plus]) {
            let op = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary()?;

        while self.matches(&[TokenKind::FSlash, TokenKind::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, String> {
        if self.matches(&[TokenKind::Bang, TokenKind::Hyphen]) {
            let op = self.previous();
            let rhs = Box::new(self.unary()?);
            Ok(Expr::Unary {
                op,
                rhs
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, String> {
        let t = self.advance();
        match t.kind {
            TokenKind::True => Ok(Expr::Literal(Lit::Bool(true))),
            TokenKind::False => Ok(Expr::Literal(Lit::Bool(false))),
            //TokenKind::Nil => Ok(Expr::Literal(Lit::Nil)),
            TokenKind::Integer(i) => Ok(Expr::Literal(Lit::Integer(i))),
            TokenKind::Double(d) => Ok(Expr::Literal(Lit::Float(d))),
            TokenKind::String => Ok(Expr::Literal(Lit::String(t.lexeme.clone()))),
            TokenKind::LParen => {
                let expr = self.expression()?;
                let l = self.line;
                self.consume(TokenKind::RParen, format!("Line {}: Found {}, expected )", l, t.lexeme.clone()));
                Ok(Expr::Paren(Box::new(expr.clone())))
            }
            _ => Err(format!("Line {}: Found {}, expected literal", self.line, t.lexeme.clone()))
        }
    }

    fn consume(&mut self, tk: TokenKind, msg: String) -> Result<Token, String> {
        if self.matches(&[tk]) {
            Ok(self.advance())
        } else {
            Err(msg)
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().kind == TokenKind::Newline { return }

            match self.peek().kind {
                TokenKind::Data | TokenKind::Fn | TokenKind::For | TokenKind::If
                    | TokenKind::While | TokenKind::Retn | TokenKind::Err
                    | TokenKind::In | TokenKind::Is | TokenKind::Do | TokenKind::End
                    | TokenKind::Me =>
                {
                    return
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn matches(&mut self, list: &[TokenKind]) -> bool {
        for tk in list {
            if self.check(*tk) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, tk: TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind == tk
        }
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn peek(&self) -> Token {
        self.tokens[self.current].clone()
    }

    fn previous(&self) -> Token {
        self.tokens[self.current - 1].clone()
    }

    //pub fn parse(mut self) -> Result<Vec<Statement>, String> {
    //    let mut err = String::new();
    //    let mut stmt = vec![];

    //    while !self.is_at_end() {
    //        let r = self.statement();
    //        if r.is_err() {
    //            err.push_str(&r.err().unwrap());
    //        } else {
    //            stmt.push(r.unwrap());
    //        }
    //    }

    //    Ok(stmt)
    //}

    //fn statement(&mut self) -> Result<Statement, String> {
    //    if self.next_is(TokenKind::Me) {
    //        self.placeholder_print()
    //    } else {
    //        self.expression()
    //    }
    //}

    //fn placeholder_print(&mut self) -> Result<Statement, String> {
    //    let value: Expression = self.expression()?;
    //    self.consume(TokenKind::Newline)?;
    //    Ok(Statement::Me(format!("{:?}", value)))
    //}
}

