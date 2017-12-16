
use err::ErrorKind;
use ::*;

#[derive(Debug)]
pub struct Parser {
    line: u64,
    err: String,
    current: usize,
    tokens: Vec<token::Token>,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Parser {
            line: 1,
            err: String::new(),
            current: 0,
            tokens,
        }
    }

    pub fn parse(mut self) -> Result<Vec<stmt::Stmt>, String> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            self.skip_newlines();
            if self.is_at_end() { break; }
            if let Some(s) = self.declaration() {
                statements.push(s);
            } else {
                self.synchronize();
                return Err(self.err)
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Option<stmt::Stmt> {
        if self.is_at_end() { return None }

        let r = if self.lookahead(1).kind == token::TokenKind::Assign {
            self.var_declaration()
        } else {
        //} else class, etc { // TODO
            self.statement()
        };

        r
    }

    fn var_declaration(&mut self) -> Option<stmt::Stmt> {
        let name = self.consume(token::TokenKind::Ident)?;

        let init = if self.matches(&[token::TokenKind::Assign]) {
            self.expression()?
        } else {
            expr::Expr::Literal(expr::Literal::Nil)
        };

        Some(stmt::Stmt::Assignment(stmt::Assignment { name, value: init }))
    }

    fn assignment(&mut self) -> Option<expr::Expr> {
        let expr = self.math()?;

        if self.matches(&[token::TokenKind::Assign]) {
            let equals = self.previous();
            let value = Box::new(self.assignment()?);
            match expr {
                expr::Expr::Variable(v) => {
                    let name = v.0.clone();
                    Some(expr::Expr::Assignment(expr::Assignment{ name, value }))
                }
                _ => {
                    self.error(err::ErrorKind::SyntaxError, format!("expected variable name, got {:?}", equals));
                    None
                }
            }
        } else {
            Some(expr)
        }
    }

    fn statement(&mut self) -> Option<stmt::Stmt> {
        self.skip_newlines();
        if self.matches(&[token::TokenKind::Me]) {
            self.me_statement_tmp()
        } else if self.matches(&[token::TokenKind::Do]) {
            self.block()
        } else if self.matches(&[token::TokenKind::If]) {
            self.if_statement()
        } else if self.matches(&[token::TokenKind::While]) {
            self.while_statement()
        } else if self.matches(&[token::TokenKind::For]) {
            self.for_statement()
        } else {
            self.expression_statement()
        }
    }

    fn me_statement_tmp(&mut self) -> Option<stmt::Stmt> {
        let value = self.expression()?;

        self.skip_newlines();
        Some(stmt::Stmt::MeTmp(stmt::MeTmp(value)))
    }

    fn block(&mut self) -> Option<stmt::Stmt> {
        let mut stmts = Vec::new();

        while !self.check(token::TokenKind::End) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End)?;

        Some(stmt::Stmt::Block(stmt::Block(stmts)))
    }

    fn expression_statement(&mut self) -> Option<stmt::Stmt> {
        let value = self.expression()?;

        self.skip_newlines();
        Some(stmt::Stmt::StmtExpr(stmt::StmtExpr(value)))
    }

    fn if_statement(&mut self) -> Option<stmt::Stmt> {
        let cond = self.expression()?;
        self.consume(token::TokenKind::Do)?;

        let mut then = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End) && !self.check(token::TokenKind::Else) {
            then.push(self.declaration()?);
        }

        let else_ = if self.matches(&[token::TokenKind::Else]) {
            let mut block = Vec::new();
            while !self.is_at_end() && !self.check(token::TokenKind::End) && !self.check(token::TokenKind::Else) {
                block.push(self.declaration()?);
            }
            Some(block)
        } else {
            None
        };

        self.consume(token::TokenKind::End)?;

        Some(stmt::Stmt::If(stmt::If {
            cond, then, else_
        }))
    }

    fn while_statement(&mut self) -> Option<stmt::Stmt> {
        let cond = self.expression()?;
        self.consume(token::TokenKind::Do);

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End) {
            body.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End)?;

        Some(stmt::Stmt::While(stmt::While {
            cond, body
        }))
    }

    fn for_statement(&mut self) -> Option<stmt::Stmt> {
        let name = self.consume(token::TokenKind::Ident)?;
        self.consume(token::TokenKind::In)?;
        let iter = self.expression()?;
        self.consume(token::TokenKind::Do)?;

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End) {
            body.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End)?;

        Some(stmt::Stmt::For(stmt::For {
            name, iter, body
        }))
    }

    fn expression(&mut self) -> Option<expr::Expr> {
        self.assignment()
    }

    fn math(&mut self) -> Option<expr::Expr> {
        self.or()
    }

    fn and(&mut self) -> Option<expr::Expr> {
        let expr = self.equality()?;
        let mut r = Some(expr.clone()); // TODO: figure out error handling

        while self.matches(&[token::TokenKind::And]) {
            let op = self.previous();
            let right = self.or()?;
            r = Some(expr::Expr::Logical(expr::Logical {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn or(&mut self) -> Option<expr::Expr> {
        let expr = self.and()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::Or]) {
            let op = self.previous();
            let right = self.equality()?;
            r = Some(expr::Expr::Logical(expr::Logical {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn equality(&mut self) -> Option<expr::Expr> {
        let expr = self.comparison()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::NotEquals, token::TokenKind::Equals]) {
            let op = self.previous();
            let right = self.comparison()?;
            r = Some(expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn comparison(&mut self) -> Option<expr::Expr> {
        let expr = self.range()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::GreaterThan,
                             token::TokenKind::GreaterThanEquals,
                             token::TokenKind::LessThan,
                             token::TokenKind::LessThanEquals]) {
            let op = self.previous();
            let right = self.addition()?;
            r = Some(expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn range(&mut self) -> Option<expr::Expr> {
        let mut expr = self.addition()?;

        while self.matches(&[token::TokenKind::IRange, token::TokenKind::ERange]) {
            if self.lookahead(1).kind == token::TokenKind::IRange ||
                self.lookahead(1).kind == token::TokenKind::ERange
            {
                self.error(err::ErrorKind::SyntaxError, "range is non-associative".into());
                return None
            }

            let op = self.previous();
            let right = self.addition()?;
            expr = expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            })
        }

        Some(expr)
    }

    fn addition(&mut self) -> Option<expr::Expr> {
        let mut expr = self.multiplication()?;

        while self.matches(&[token::TokenKind::Minus, token::TokenKind::Plus]) {
            let op = self.previous();
            let right = self.multiplication()?;
            expr = expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        Some(expr)
    }

    fn multiplication(&mut self) -> Option<expr::Expr> {
        let mut expr = self.unary()?;

        while self.matches(&[token::TokenKind::Divide, token::TokenKind::Star, token::TokenKind::Mod]) {
            let op = self.previous();
            let right = self.unary()?;
            expr = expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            });
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<expr::Expr> {
        if self.matches(&[token::TokenKind::Not, token::TokenKind::Minus]) {
            let op = self.previous();
            let rhs = Box::new(self.unary()?);
            Some(expr::Expr::Unary(expr::Unary {
                op,
                rhs
            }))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<expr::Expr> {
        let t = self.advance();

        match t.kind {
            token::TokenKind::True => Some(expr::Expr::Literal(expr::Literal::Bool(true))),
            token::TokenKind::False => Some(expr::Expr::Literal(expr::Literal::Bool(false))),
            token::TokenKind::Nil => Some(expr::Expr::Literal(expr::Literal::Nil)),
            token::TokenKind::Integer(i) => Some(expr::Expr::Literal(expr::Literal::Integer(i))),
            token::TokenKind::Double(d) => Some(expr::Expr::Literal(expr::Literal::Float(d))),
            token::TokenKind::String => Some(expr::Expr::Literal(expr::Literal::String(t.lexeme.clone()))),

            token::TokenKind::LParen => {
                let expr = self.expression()?;
                self.consume(token::TokenKind::RParen)?;
                Some(expr.clone())
            },

            token::TokenKind::LBracket => {
                let mut inner = Vec::new();
                while !self.matches(&[token::TokenKind::RBracket]) {
                    inner.push(self.expression()?);
                    if self.matches(&[token::TokenKind::RBracket]) {
                        break
                    } else {
                        self.consume(token::TokenKind::Comma)?;
                    }
                }
                Some(expr::Expr::Literal(expr::Literal::Array(expr::Array {
                    len: inner.len(),
                    inner
                })))
            },

            token::TokenKind::Ident => Some(expr::Expr::Variable(expr::Variable(self.previous()))),

            _ => {
                self.error(err::ErrorKind::SyntaxError, format!("Found {:?}, expected expression", t.lexeme));
                None
            }
        }
    }

    fn consume(&mut self, tk: token::TokenKind) -> Option<token::Token> {
        if self.check(tk) {
            Some(self.advance())
        } else {
            let t = self.peek().kind;
            self.error(err::ErrorKind::UnexpectedToken, format!("Found {:?}, expected {:?}", t, tk));
            None
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().kind == token::TokenKind::Newline { return }

            match self.peek().kind {
                token::TokenKind::Data | token::TokenKind::Fn | token::TokenKind::For | token::TokenKind::If
                    | token::TokenKind::While | token::TokenKind::Retn | token::TokenKind::Err
                    | token::TokenKind::In | token::TokenKind::Is | token::TokenKind::Do | token::TokenKind::End
                    | token::TokenKind::Me =>
                {
                    return
                }
                _ => {}
            }

            self.advance();
        }
    }

    fn skip_newlines(&mut self) {
        while self.matches(&[token::TokenKind::Newline]) {
            self.line += 1;
        }
    }

    fn matches(&mut self, list: &[token::TokenKind]) -> bool {
        for tk in list {
            if self.check(*tk) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&mut self, tk: token::TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            self.peek().kind == tk
        }
    }

    fn advance(&mut self) -> token::Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == token::TokenKind::Eof
    }

    fn peek(&self) -> token::Token {
        self.tokens[self.current].clone()
    }

    fn lookahead(&self, n: usize) -> token::Token {
        self.tokens[self.current + n].clone()
    }

    fn previous(&self) -> token::Token {
        self.tokens[self.current - 1].clone()
    }

    fn error(&mut self, kind: ErrorKind, msg: String) {
        self.err.push_str(&format!("Line {} - {:?}: {}\n", self.line, kind, msg));
    }
}

