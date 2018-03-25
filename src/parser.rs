use super::*;
use err::{ErrorKind, PiccoloError};

#[derive(Debug)]
pub struct Parser {
    err: Vec<PiccoloError>,
    current: usize,
    tokens: Vec<token::Token>,
    in_data: bool,
}

impl Parser {
    pub fn new(tokens: Vec<token::Token>) -> Self {
        Parser {
            err: Vec::new(),
            current: 0,
            tokens: tokens
                .into_iter()
                .filter(|t| t.kind != token::TokenKind::Newline)
                .collect(),
            in_data: false,
        }
    }

    pub fn parse(mut self) -> Result<Vec<stmt::Stmt>, Vec<PiccoloError>> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            if self.is_at_end() {
                break;
            }
            if let Some(s) = self.declaration() {
                statements.push(s);
            } else {
                self.synchronize();
                return Err(self.err);
            }
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Option<stmt::Stmt> {
        if self.is_at_end() {
            return None;
        }

        if self.lookahead(1).kind == token::TokenKind::Assign {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Option<stmt::Stmt> {
        let name = self.consume(token::TokenKind::Ident, Some("Lookng for variable name"))?;

        let init = if self.matches(&[token::TokenKind::Assign]) {
            self.expression()?
        } else {
            expr::Expr::Literal(expr::Literal::Nil)
        };

        Some(stmt::Stmt::Assignment(stmt::Assignment {
            name,
            value: init,
        }))
    }

    fn assignment(&mut self) -> Option<expr::Expr> {
        let expr = self.math()?;

        if self.matches(&[token::TokenKind::Assign]) {
            let equals = self.previous();
            let value = Box::new(self.assignment()?);
            match expr {
                expr::Expr::Variable(v) => {
                    let name = v.0.clone();
                    Some(expr::Expr::Assignment(expr::Assignment { name, value }))
                }
                expr::Expr::Get(g) => Some(expr::Expr::Set(expr::Set {
                    object: g.object,
                    name: g.name,
                    value,
                })),
                expr::Expr::Index(i) => Some(expr::Expr::Set(expr::Set {
                    object: Box::new(expr::Expr::Index(i.clone())),
                    name: equals,
                    value,
                })),
                _ => {
                    self.error(
                        err::ErrorKind::SyntaxError,
                        equals.line,
                        &format!("Expected variable name, got {:?}", expr.clone()),
                        None,
                    );
                    None
                }
            }
        } else {
            Some(expr)
        }
    }

    fn statement(&mut self) -> Option<stmt::Stmt> {
        if self.matches(&[token::TokenKind::Data]) {
            self.data()
        } else if self.matches(&[token::TokenKind::Do]) {
            self.block()
        } else if self.matches(&[token::TokenKind::If]) {
            self.if_statement()
        } else if self.matches(&[token::TokenKind::While]) {
            self.while_statement()
        } else if self.matches(&[token::TokenKind::For]) {
            self.for_statement()
        } else if self.matches(&[token::TokenKind::Fn]) {
            self.func()
        } else if self.matches(&[token::TokenKind::Retn]) {
            self.retn()
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Option<stmt::Stmt> {
        let mut stmts = Vec::new();

        while !self.check(token::TokenKind::End) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End, Some("Unterminated block"))?;

        Some(stmt::Stmt::Block(stmt::Block(stmts)))
    }

    fn expression_statement(&mut self) -> Option<stmt::Stmt> {
        let value = self.expression()?;

        Some(stmt::Stmt::StmtExpr(stmt::StmtExpr(value)))
    }

    fn if_statement(&mut self) -> Option<stmt::Stmt> {
        let cond = self.expression()?;
        self.consume(token::TokenKind::Do, None)?;

        let mut then = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End)
            && !self.check(token::TokenKind::Else)
        {
            then.push(self.declaration()?);
        }

        let else_ = if self.matches(&[token::TokenKind::Else]) {
            let mut block = Vec::new();
            while !self.is_at_end() && !self.check(token::TokenKind::End)
                && !self.check(token::TokenKind::Else)
            {
                block.push(self.declaration()?);
            }
            Some(block)
        } else {
            None
        };

        self.consume(token::TokenKind::End, Some("Unterminated if statement"))?;

        Some(stmt::Stmt::If(stmt::If { cond, then, else_ }))
    }

    fn while_statement(&mut self) -> Option<stmt::Stmt> {
        let cond = self.expression()?;
        let body = self.do_block()?;

        Some(stmt::Stmt::While(stmt::While { cond, body }))
    }

    fn for_statement(&mut self) -> Option<stmt::Stmt> {
        let name = self.consume(
            token::TokenKind::Ident,
            Some("Expected variable name in for block"),
        )?;
        self.consume(token::TokenKind::In, None)?;
        let iter = self.expression()?;
        let body = self.do_block()?;
        Some(stmt::Stmt::For(stmt::For { name, iter, body }))
    }

    fn func(&mut self) -> Option<stmt::Stmt> {
        let name = self.consume(token::TokenKind::Ident, Some("Expected function name"))?;
        self.consume(token::TokenKind::LParen, None)?;

        let mut args = Vec::new();
        let mut multi = false;
        let method = self.in_data;

        //if self.in_data {
        //    args.push(self.consume(token::TokenKind::Me, Some("Methods must have self parameter `me`"))?);
        //}

        if !self.check(token::TokenKind::RParen) {
            'outer: while {
                if args.len() >= 64 {
                    self.error(
                        err::ErrorKind::SyntaxError,
                        name.line,
                        "Cannot have more than 64 parameters",
                        Some("(do you *really* need that many anyway?)"),
                    );
                    return None;
                }

                if !self.is_at_end() && self.lookahead(1).kind == token::TokenKind::IRange {
                    let name = self.consume(token::TokenKind::Ident, Some("Varags needs name"))?;
                    self.consume(token::TokenKind::IRange, None)?;
                    args.push(name);
                    multi = true;
                    break 'outer;
                }

                args.push(self.consume(token::TokenKind::Ident, Some("Arg must have name"))?);
                self.matches(&[token::TokenKind::Comma])
            } {}
        }
        self.consume(token::TokenKind::RParen, Some("Unterminated args list"))?;

        let body = self.is_block()?;

        let arity = if multi {
            func::Arity::Multi
        } else if args.is_empty() {
            func::Arity::None
        } else {
            func::Arity::Some(args.len())
        };

        Some(stmt::Stmt::Func(stmt::Func {
            arity,
            name,
            args,
            body,
            method,
        }))
    }

    fn retn(&mut self) -> Option<stmt::Stmt> {
        let keyword = self.previous();
        let value = if self.check(token::TokenKind::Newline) {
            if !self.check(token::TokenKind::End) {
                self.error(
                    err::ErrorKind::SyntaxError,
                    keyword.line,
                    "Cannot have expressions after return",
                    Some("Move on to the same line if you'd like to return it"),
                );
                return None;
            } else {
                self.consume(
                    token::TokenKind::End,
                    Some("Cannot have expressions after return"),
                )?;
                None
            }
        } else {
            self.expression()
        };
        Some(stmt::Stmt::Retn(stmt::Retn { keyword, value }))
    }

    fn data(&mut self) -> Option<stmt::Stmt> {
        self.in_data = true;
        let name = self.consume(token::TokenKind::Ident, Some("Data needs name"))?;
        self.consume(token::TokenKind::Is, None)?;

        let mut fields = Vec::new();
        while !self.check(token::TokenKind::Fn) && !self.check(token::TokenKind::End)
            && !self.is_at_end()
        {
            let name = self.consume(token::TokenKind::Ident, Some("Field must have name"))?;
            self.consume(token::TokenKind::Assign, None)?;
            let value = self.expression()?;

            fields.push((name, value));
        }

        let mut methods = Vec::new();
        while !self.check(token::TokenKind::End) && !self.is_at_end() {
            self.consume(token::TokenKind::Fn, Some("Expected method to have name"))?;
            if let stmt::Stmt::Func(func) = self.func()? {
                methods.push(func);
            } else {
                panic!("rust broke")
            }
        }

        self.consume(token::TokenKind::End, Some("Unterminated data"))?;
        self.in_data = false;
        Some(stmt::Stmt::Data(stmt::Data {
            name,
            methods,
            fields,
        }))
    }

    #[allow(wrong_self_convention)]
    fn is_block(&mut self) -> Option<Vec<stmt::Stmt>> {
        self.consume(
            token::TokenKind::Is,
            Some("fn, data start with keyword `is`"),
        )?;

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End) {
            body.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End, Some("Unterminated block"))?;
        Some(body)
    }

    fn do_block(&mut self) -> Option<Vec<stmt::Stmt>> {
        self.consume(
            token::TokenKind::Do,
            Some("if, while, for start with keyword `do`"),
        )?;

        let mut body = Vec::new();
        while !self.is_at_end() && !self.check(token::TokenKind::End) {
            body.push(self.declaration()?);
        }

        self.consume(token::TokenKind::End, Some("Unterminated block"))?;
        Some(body)
    }

    fn expression(&mut self) -> Option<expr::Expr> {
        self.assignment()
    }

    fn math(&mut self) -> Option<expr::Expr> {
        self.or()
    }

    fn or(&mut self) -> Option<expr::Expr> {
        let expr = self.and()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::Or]) {
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

    fn and(&mut self) -> Option<expr::Expr> {
        let expr = self.bit_or()?;
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

    fn bit_or(&mut self) -> Option<expr::Expr> {
        let expr = self.bit_xor()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::BOr]) {
            let op = self.previous();
            let right = self.or()?;
            r = Some(expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn bit_xor(&mut self) -> Option<expr::Expr> {
        let expr = self.bit_and()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::BXor]) {
            let op = self.previous();
            let right = self.or()?;
            r = Some(expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            }));
        }

        r
    }

    fn bit_and(&mut self) -> Option<expr::Expr> {
        let expr = self.equality()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::BAnd]) {
            let op = self.previous();
            let right = self.or()?;
            r = Some(expr::Expr::Binary(expr::Binary {
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

        while self.matches(&[
            token::TokenKind::GreaterThan,
            token::TokenKind::GreaterThanEquals,
            token::TokenKind::LessThan,
            token::TokenKind::LessThanEquals,
        ]) {
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
        let mut expr = self.bitshift()?;

        while self.matches(&[token::TokenKind::IRange, token::TokenKind::ERange]) {
            if self.lookahead(1).kind == token::TokenKind::IRange
                || self.lookahead(1).kind == token::TokenKind::ERange
            {
                let l = self.lookahead(1).line;
                self.error(
                    err::ErrorKind::SyntaxError,
                    l,
                    "Range is non-associative",
                    None,
                );
                return None;
            }

            let op = self.previous();
            let right = self.bitshift()?;
            expr = expr::Expr::Binary(expr::Binary {
                lhs: Box::new(expr.clone()),
                op,
                rhs: Box::new(right),
            })
        }

        Some(expr)
    }

    fn bitshift(&mut self) -> Option<expr::Expr> {
        let expr = self.addition()?;
        let mut r = Some(expr.clone());

        while self.matches(&[token::TokenKind::BitRight, token::TokenKind::BitLeft]) {
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

        while self.matches(&[
            token::TokenKind::Divide,
            token::TokenKind::Star,
            token::TokenKind::Mod,
        ]) {
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
            Some(expr::Expr::Unary(expr::Unary { op, rhs }))
        } else {
            self.index()
        }
    }

    fn index(&mut self) -> Option<expr::Expr> {
        let expr = self.call()?;

        if self.matches(&[token::TokenKind::LBracket]) {
            let i = self.expression()?;
            let rb = self.consume(token::TokenKind::RBracket, None)?;
            Some(expr::Expr::Index(expr::Index {
                i: Box::new(i),
                object: Box::new(expr),
                rb,
            }))
        } else {
            Some(expr)
        }
    }

    fn call(&mut self) -> Option<expr::Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.matches(&[token::TokenKind::LParen]) {
                expr = self.finish_call(Box::new(expr))?;
            } else if self.matches(&[token::TokenKind::Dot]) {
                let name = self.consume(token::TokenKind::Ident, Some("Field must be named"))?;
                expr = expr::Expr::Get(expr::Get {
                    name,
                    object: Box::new(expr.clone()),
                })
            } else {
                break;
            }
        }

        Some(expr)
    }

    fn finish_call(&mut self, callee: Box<expr::Expr>) -> Option<expr::Expr> {
        let mut args = Vec::new();

        if !self.check(token::TokenKind::RParen) {
            while {
                if args.len() >= 64 {
                    let l = self.previous().line;
                    self.error(
                        err::ErrorKind::SyntaxError,
                        l,
                        "Cannot have more than 64 arguments to a function",
                        Some("(do you *really* need that many anyway?)"),
                    );
                }
                args.push(self.expression()?);
                self.matches(&[token::TokenKind::Comma])
            } {}
        }

        let paren = self.consume(token::TokenKind::RParen, None)?;

        let arity = if args.is_empty() {
            func::Arity::None
        } else {
            func::Arity::Some(args.len())
        };

        Some(expr::Expr::Call(expr::Call {
            callee,
            paren,
            args,
            arity,
        }))
    }

    fn primary(&mut self) -> Option<expr::Expr> {
        let t = self.advance();

        match t.kind {
            token::TokenKind::True => Some(expr::Expr::Literal(expr::Literal::Bool(true))),
            token::TokenKind::False => Some(expr::Expr::Literal(expr::Literal::Bool(false))),
            token::TokenKind::Nil => Some(expr::Expr::Literal(expr::Literal::Nil)),
            token::TokenKind::Integer(i) => Some(expr::Expr::Literal(expr::Literal::Integer(i))),
            token::TokenKind::Double(d) => Some(expr::Expr::Literal(expr::Literal::Float(d))),
            token::TokenKind::String => {
                Some(expr::Expr::Literal(expr::Literal::String(t.lexeme.clone())))
            }

            token::TokenKind::LParen => {
                if self.is_at_end() {
                    self.error(
                        ErrorKind::SyntaxError,
                        t.line,
                        &format!("Expected expression, found {:?}", t.kind),
                        None,
                    );
                    return None;
                }
                let expr = self.expression()?;
                self.consume(token::TokenKind::RParen, None)?;
                Some(expr.clone())
            }

            token::TokenKind::LBracket => {
                if self.is_at_end() {
                    self.error(
                        ErrorKind::SyntaxError,
                        t.line,
                        &format!("Expected expression, found {:?}", t.kind),
                        None,
                    );
                    return None;
                }
                let mut inner = Vec::new();
                while !self.matches(&[token::TokenKind::RBracket]) {
                    inner.push(self.expression()?);
                    if self.matches(&[token::TokenKind::RBracket]) {
                        break;
                    } else {
                        self.consume(token::TokenKind::Comma, None)?;
                    }
                }
                Some(expr::Expr::Literal(expr::Literal::Array(expr::Array {
                    len: inner.len(),
                    inner,
                })))
            }

            token::TokenKind::New => self.new_expr(),

            token::TokenKind::Me | token::TokenKind::Ident => {
                Some(expr::Expr::Variable(expr::Variable(self.previous())))
            }

            token::TokenKind::Fn => {
                let mut args = Vec::new();
                let mut multi = false;
                let method = self.in_data;
                self.consume(
                    token::TokenKind::LParen,
                    Some("Lambda functions do not have names"),
                )?;
                if !self.check(token::TokenKind::RParen) {
                    'outer: while {
                        if args.len() >= 64 {
                            self.error(
                                err::ErrorKind::SyntaxError,
                                t.line,
                                "Cannot have more than 64 parameters",
                                Some("(do you *really* need that many anyway?)"),
                            );
                            return None;
                        }

                        if !self.is_at_end() && self.lookahead(1).kind == token::TokenKind::IRange {
                            let name =
                                self.consume(token::TokenKind::Ident, Some("Varags needs name"))?;
                            self.consume(token::TokenKind::IRange, None)?;
                            args.push(name);
                            multi = true;
                            break 'outer;
                        }

                        args.push(self.consume(
                            token::TokenKind::Ident,
                            Some("Arg must have name"),
                        )?);
                        self.matches(&[token::TokenKind::Comma])
                    } {}
                }
                self.consume(token::TokenKind::RParen, Some("Unterminated args list"))?;

                let body = self.is_block()?;

                let arity = if multi {
                    func::Arity::Multi
                } else if args.is_empty() {
                    func::Arity::None
                } else {
                    func::Arity::Some(args.len())
                };

                Some(expr::Expr::Func(expr::Func {
                    arity,
                    name: t,
                    args,
                    body,
                    method,
                }))
            }

            _ => {
                self.error(
                    err::ErrorKind::SyntaxError,
                    t.line,
                    &format!("Found {:?}, expected expression", t.lexeme),
                    None,
                );
                None
            }
        }
    }

    fn new_expr(&mut self) -> Option<expr::Expr> {
        let name = self.consume(token::TokenKind::Ident, Some("Instance must have name"))?;
        let mut args = Vec::new();
        if self.consume(token::TokenKind::LParen, None).is_some() {
            loop {
                let inst = self.consume(token::TokenKind::Ident, Some("Data must have name"))?
                    .lexeme;
                self.consume(token::TokenKind::Assign, None)?;
                let value = self.expression()?;
                args.push((inst, value));
                if !self.matches(&[token::TokenKind::RParen]) {
                    self.consume(token::TokenKind::Comma, None)?;
                } else {
                    break;
                }
            }
        }
        Some(expr::Expr::New(expr::New { name, args }))
    }

    fn consume(&mut self, tk: token::TokenKind, extra: Option<&str>) -> Option<token::Token> {
        if self.check(tk) {
            Some(self.advance())
        } else {
            let t = self.peek();
            self.error(
                err::ErrorKind::UnexpectedToken,
                t.line,
                &format!("Found {:?}, expected {:?}", t.kind, tk),
                extra,
            );
            None
        }
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().kind == token::TokenKind::Newline {
                return;
            }

            match self.peek().kind {
                token::TokenKind::Data
                | token::TokenKind::Fn
                | token::TokenKind::For
                | token::TokenKind::If
                | token::TokenKind::While
                | token::TokenKind::Retn
                | token::TokenKind::Err
                | token::TokenKind::In
                | token::TokenKind::Is
                | token::TokenKind::Do
                | token::TokenKind::End
                | token::TokenKind::Me => return,
                _ => {}
            }

            self.advance();
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

    fn error(&mut self, kind: ErrorKind, line: u64, msg: &str, extra: Option<&str>) {
        if let Some(extra) = extra {
            self.err
                .push(PiccoloError::with_info(kind, msg, line, extra));
        } else {
            self.err.push(PiccoloError::new(kind, msg, line));
        }
    }
}
