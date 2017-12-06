
use err::ErrorKind;
use token::{Token, TokenKind};
use ast::*;

#[derive(Debug)]
pub struct Parser {
    line: u64,
    err: String,
    current: usize,
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            line: 1,
            err: String::new(),
            current: 0,
            tokens,
        }
    }

    pub fn parse(mut self) -> Result<Expr, String> {
        let r = self.start_parse();
        if r.is_some() {
            Ok(r.unwrap())
        } else {
            Err(self.err)
        }
    }

    fn start_parse(&mut self) -> Option<Expr> {
        self.expression()
    }

    fn expression(&mut self) -> Option<Expr> {
        self.math() // TODO: add more
    }

    fn math(&mut self) -> Option<Expr> {
        self.and()
    }

    fn and(&mut self) -> Option<Expr> {
        let expr = self.or()?;
        let mut r = Some(expr.clone()); // TODO: figure out error handling
        //let mut r = Err("Expected &&".into());

        while self.matches(&[TokenKind::And]) {
            let op = self.previous();
            let right = self.or()?;
            r = Some(Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn or(&mut self) -> Option<Expr> {
        let expr = self.equality()?;
        let mut r = Some(expr.clone());
        //let mut r = Err("Expected ||".into());

        while self.matches(&[TokenKind::Or]) {
            let op = self.previous();
            let right = self.equality()?;
            r = Some(Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn equality(&mut self) -> Option<Expr> {
        let expr = self.comparison()?;
        let mut r = Some(expr.clone());
        //let mut r = Err("Expected ==, !=".into());

        while self.matches(&[TokenKind::BangEquals, TokenKind::Equals]) {
            let op = self.previous();
            let right = self.comparison()?;
            r = Some(Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn comparison(&mut self) -> Option<Expr> {
        let expr = self.addition()?;
        let mut r = Some(expr.clone());
        //let mut r = Err("Expected >, <, >=, <=".into());

        while self.matches(&[TokenKind::GreaterThan, TokenKind::GreaterThanEquals, TokenKind::LessThan, TokenKind::LessThanEquals]) {
            let op = self.previous();
            let right = self.addition()?;
            r = Some(Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn addition(&mut self) -> Option<Expr> {
        let mut expr = self.multiplication()?;

        while self.matches(&[TokenKind::Minus, TokenKind::Plus]) {
            let op = self.previous();
            let right = self.multiplication()?;
            expr = Expr::Binary(Binary{
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        Some(expr)
    }

    fn multiplication(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;

        while self.matches(&[TokenKind::FSlash, TokenKind::Star]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary(Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if self.matches(&[TokenKind::Bang, TokenKind::Minus]) {
            let op = self.previous();
            let rhs = Box::new(self.unary()?);
            Some(Expr::Unary(Unary {
                op,
                rhs
            }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        let t = self.advance();
        match t.kind {
            TokenKind::True => Some(Expr::Literal(Literal::Bool(true))),
            TokenKind::False => Some(Expr::Literal(Literal::Bool(false))),
            //TokenKind::Nil => Some(Expr::Literal(Lit::Nil)),
            TokenKind::Integer(i) => Some(Expr::Literal(Literal::Integer(i))),
            TokenKind::Double(d) => Some(Expr::Literal(Literal::Float(d))),
            TokenKind::String => Some(Expr::Literal(Literal::String(t.lexeme.clone()))),
            TokenKind::LParen => {
                let expr = self.expression()?;
                let l = self.line;
                self.consume(TokenKind::RParen)?;
                Some(expr.clone())
            }
            // TokenKind::Ident => {get value of variable}
            tk => {
                self.error(ErrorKind::UnexpectedToken, format!("Found {:?}, expected literal ({:?})", t.lexeme, tk));
                None
            }
        }
    }

    fn consume(&mut self, tk: TokenKind) -> Option<Token> {
        if self.matches(&[tk]) {
            Some(self.advance())
        } else {
            let t = self.peek().kind;
            self.error(ErrorKind::UnexpectedToken, format!("Found {:?}, expected {:?}", t, tk));
            None
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

    fn error(&mut self, kind: ErrorKind, msg: String) {
        self.err.push_str(&format!("Line {} - {:?}: {}\n", self.line, kind, msg));
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

