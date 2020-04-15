use crate::ast::expr::Expr;
use crate::ast::stmt::Stmt;
use crate::compiler::{scanner::Scanner, Token, TokenKind};
use crate::error::{ErrorKind, PiccoloError};
use crate::Value;
use std::convert::TryInto;

/*
pub struct Parser/*<'a: 'b, 'b>*/ {
    err: Vec<PiccoloError>,
    //scanner: &'b mut Scanner<'a>,
}

impl/*<'a, 'b>*/ Parser/*<'a, 'b>*/ {
    pub fn new(/*scanner: &'b mut Scanner<'a>*/) -> Self {
        Parser {
            err: vec![],
            //scanner,
        }
    }

    fn expr<'a, 'b>(&mut self, scanner: &'b mut Scanner<'a>) -> Result<Expr<'a, 'b>, PiccoloError> {
        self.expr_bp(scanner, 0)
    }

    fn expr_bp<'a, 'b>(&mut self, scanner: &'b mut Scanner<'a>, min_bp: u8) -> Result<Expr<'a, 'b>, PiccoloError> {
        let mut lhs = Expr::Atom(Value::String(scanner.next_token()?.lexeme.to_owned()));

        loop {
            let op = scanner.current();
            if op.kind == TokenKind::Eof { break; }

            let (lbp, rbp) = infix_binding_power(op.kind);
            if lbp < min_bp {
                break;
            }

            let _ = scanner.next_token()?;
            let rhs = self.expr_bp(scanner, rbp)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }
}
*/

fn expr_bp<'a>(scanner: &mut Scanner<'a>, min_bp: u8) -> Result<Expr<'a>, PiccoloError> {
    let _ = scanner.next_token()?;
    let lhs = Value::try_from(scanner.take_current())?;
    let _ = scanner.next_token()?;
    let op = scanner.take_current();
    let _ = scanner.next_token()?;
    let rhs = Value::try_from(scanner.take_current())?;
    Ok(Expr::Binary {
        lhs: Box::new(Expr::Atom(lhs)),
        op,
        rhs: Box::new(Expr::Atom(rhs)),
    })
}

fn infix_binding_power(op: TokenKind) -> (u8, u8) {
    match op {
        TokenKind::Plus | TokenKind::Minus => (1, 2),
        TokenKind::Multiply | TokenKind::Divide => (3, 4),
        _ => panic!("ibp {:?}", op),
    }
}

#[test]
fn take() {
    let src = "1+2";
    let expr = expr_bp(&mut Scanner::new(src), 0).unwrap();
    assert_eq!(expr, Expr::Binary {
        lhs: Box::new(Expr::Atom(Value::Integer(1))),
        op: Token::new(TokenKind::Plus, "+", 1),
        rhs: Box::new(Expr::Atom(Value::Integer(2))),
    })
}
