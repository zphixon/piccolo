use crate::{ErrorKind, PiccoloError, Scanner, Token, TokenKind};

use crate::ast::Expr;
use crate::ast::Stmt;

#[derive(Default)]
pub struct Parser<'a> {
    ast: Vec<Stmt<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser { ast: Vec::new() }
    }

    pub fn parse(&mut self, scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, Vec<PiccoloError>> {
        let mut errors = Vec::new();
        while scanner.peek_token(0)?.kind != TokenKind::Eof {
            match self.declaration(scanner) {
                Ok(stmt) => self.ast.push(stmt),
                Err(e) => errors.push(e),
            }
        }

        if errors.is_empty() {
            Ok(std::mem::take(&mut self.ast))
        } else {
            Err(errors)
        }
    }

    fn declaration(&self, scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
        if scanner.peek_token(1)?.kind == TokenKind::Assign
            || scanner.peek_token(1)?.kind == TokenKind::Declare
        {
            let name = self.consume(scanner, TokenKind::Identifier)?;
            let op = scanner.next_token()?;
            let value = self.expr_bp(scanner, BindingPower::Assignment)?;
            Ok(Stmt::Assignment { name, op, value })
        } else if scanner.peek_token(0)?.kind == TokenKind::Retn {
            let keyword = scanner.next_token()?;
            let value = Some(self.expr_bp(scanner, BindingPower::Assignment)?);
            Ok(Stmt::Retn { keyword, value })
        } else if scanner.peek_token(0)?.kind == TokenKind::Assert {
            let keyword = scanner.next_token()?;
            let value = self.expr_bp(scanner, BindingPower::Assignment)?;
            Ok(Stmt::Assert { keyword, value })
        } else if scanner.peek_token(0)?.kind == TokenKind::Do {
            scanner.next_token()?;
            let body = self.block(scanner)?;
            let end = self.consume(scanner, TokenKind::End)?;
            Ok(Stmt::Block { end, body })
        } else {
            let expr = self.expr_bp(scanner, BindingPower::Assignment)?;
            Ok(Stmt::Expr(expr))
        }
    }

    fn block(&self, scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError> {
        let mut stmts = Vec::new();

        while scanner.peek_token(0)?.kind != TokenKind::End {
            stmts.push(self.declaration(scanner)?);
        }

        Ok(stmts)
    }

    fn expr_bp(
        &self,
        scanner: &mut Scanner<'a>,
        min_bp: BindingPower,
    ) -> Result<Expr<'a>, PiccoloError> {
        let lhs_token = scanner.next_token()?;
        let mut lhs = if lhs_token.is_value() {
            Expr::Atom(lhs_token)
        } else if lhs_token.kind == TokenKind::Identifier {
            Expr::Variable(lhs_token)
        } else if lhs_token.kind == TokenKind::LeftParen {
            let e = Expr::Paren(Box::new(self.expr_bp(scanner, BindingPower::Assignment)?));
            self.consume(scanner, TokenKind::RightParen).map_err(|e| {
                e.msg_string(format!("in expression starting on line {}", lhs_token.line))
            })?;
            e
        } else {
            let pbp = prefix_binding_power(lhs_token.kind);
            if pbp != BindingPower::None {
                let rhs = self.expr_bp(scanner, pbp)?;
                Expr::Unary {
                    op: lhs_token,
                    rhs: Box::new(rhs),
                }
            } else {
                return Err(PiccoloError::new(ErrorKind::MalformedExpression {
                    from: lhs_token.to_string(),
                })
                .line(lhs_token.line));
            }
        };

        loop {
            let op_token = scanner.peek_token(0)?;
            if op_token.kind == TokenKind::Eof {
                break;
            }

            let op_prec = infix_binding_power(op_token.kind);

            if op_prec < min_bp {
                break;
            }

            let op = scanner.next_token()?;
            let rhs = self.expr_bp(scanner, op_prec + 1)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }

    fn consume(
        &self,
        scanner: &mut Scanner<'a>,
        kind: TokenKind,
    ) -> Result<Token<'a>, PiccoloError> {
        let tok = scanner.next_token()?;
        if tok.kind == kind {
            Ok(tok)
        } else {
            Err(PiccoloError::new(ErrorKind::UnexpectedToken {
                exp: format!("{:?}", kind),
                got: format!("{:?}", tok.kind),
            })
            .line(tok.line))
        }
    }
}

fn prefix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Minus => BindingPower::Unary,
        TokenKind::Not => BindingPower::Unary,
        TokenKind::Identifier => BindingPower::None,
        _ => BindingPower::None,
    }
}

fn infix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Plus => BindingPower::Term,
        TokenKind::Minus => BindingPower::Term,
        TokenKind::Multiply => BindingPower::Factor,
        TokenKind::Divide => BindingPower::Factor,
        TokenKind::Modulo => BindingPower::Factor,
        TokenKind::Equal => BindingPower::Equality,
        TokenKind::NotEqual => BindingPower::Equality,
        TokenKind::Less => BindingPower::Comparison,
        TokenKind::Greater => BindingPower::Comparison,
        TokenKind::LessEqual => BindingPower::Comparison,
        TokenKind::GreaterEqual => BindingPower::Comparison,
        TokenKind::Retn => BindingPower::None,
        TokenKind::Identifier => BindingPower::None,
        _ => BindingPower::None,
    }
}

macro_rules! prec {
    ($name:ident => $($item:ident = $num:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
        pub(crate) enum $name {
            $($item = $num,)*
        }

        impl Into<u8> for $name {
            fn into(self) -> u8 {
                match self {
                    $($name::$item => $num,)*
                }
            }
        }

        impl From<u8> for $name {
            fn from(v: u8) -> $name {
                match v {
                    $($num => $name::$item,)*
                    _ => panic!("{} does not correspond to any item in {}", v, stringify!($name))
                }
            }
        }

        impl std::ops::Add<u8> for $name {
            type Output = $name;
            fn add(self, rhs: u8) -> $name {
                let lhs: u8 = self.into();
                (lhs + rhs).into()
            }
        }
    };
}

prec!(BindingPower =>
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
);
