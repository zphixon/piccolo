//! Contains `Parser`, which converts a source of tokens into a Piccolo AST.

use crate::{ErrorKind, PiccoloError, Scanner, Token, TokenKind};

use super::ast::Expr;
use super::ast::Stmt;

/// Parse a stream of tokens into an AST. This method collects errors on statement
/// boundaries, continuing until the end of the file.
pub fn parse<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, Vec<PiccoloError>> {
    let mut ast = Vec::new();
    let mut errors = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::Eof {
        trace!("statement");

        match statement(scanner) {
            Ok(stmt) => ast.push(stmt),
            Err(e) => errors.push(e),
        }
    }

    if errors.is_empty() {
        Ok(ast)
    } else {
        Err(errors)
    }
}

fn statement<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    if scanner.peek_token(1)?.kind == TokenKind::Assign {
        assign(scanner)
    } else if scanner.peek_token(1)?.kind == TokenKind::Declare {
        declare(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Break {
        break_(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Continue {
        continue_(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Retn {
        retn(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Assert {
        assert(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Do {
        do_(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::If {
        if_(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::While {
        while_(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::For {
        for_(scanner)
    } else {
        trace!("declaration, expr");

        let expr = expr_bp(scanner, BindingPower::ExpressionBoundary)?;

        Ok(Stmt::Expr { expr })
    }
}

fn assign<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("assign");

    let name = consume(scanner, TokenKind::Identifier)?;
    let op = consume(scanner, TokenKind::Assign)?;
    let value = expr_bp(scanner, BindingPower::ExpressionBoundary)?;

    Ok(Stmt::Assignment { name, op, value })
}

fn declare<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("declare");

    let name = consume(scanner, TokenKind::Identifier)?;
    let op = consume(scanner, TokenKind::Declare)?;
    let value = expr_bp(scanner, BindingPower::ExpressionBoundary)?;

    Ok(Stmt::Declaration { name, op, value })
}

fn break_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("break");

    let break_ = scanner.next_token()?;

    Ok(Stmt::Break { break_ })
}

fn continue_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("continue");

    let continue_ = scanner.next_token()?;

    Ok(Stmt::Continue { continue_ })
}

fn retn<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("retn");

    let retn = scanner.next_token()?;
    let value = Some(expr_bp(scanner, BindingPower::ExpressionBoundary)?);

    Ok(Stmt::Retn { retn, value })
}

fn assert<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("assert");

    let assert = scanner.next_token()?;
    let value = expr_bp(scanner, BindingPower::ExpressionBoundary)?;

    Ok(Stmt::Assert { assert, value })
}

fn do_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("do");

    scanner.next_token()?;
    let body = block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::Block { end, body })
}

fn if_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("if");

    let if_ = scanner.next_token()?;
    let cond = expr_bp(scanner, BindingPower::ExpressionBoundary)?;
    consume(scanner, TokenKind::Do)?;
    let then_block = block_until_else_or_end(scanner)?;

    let (else_, else_block) = if scanner.peek_token(0)?.kind == TokenKind::Else {
        (
            Some(consume(scanner, TokenKind::Else)?),
            Some(block(scanner)?),
        )
    } else {
        (None, None)
    };

    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::If {
        if_,
        cond,
        then_block,
        else_,
        else_block,
        end,
    })
}

fn while_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("while");
    let while_ = scanner.next_token()?;
    let cond = expr_bp(scanner, BindingPower::ExpressionBoundary)?;
    consume(scanner, TokenKind::Do)?;
    let body = block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::While {
        while_,
        cond,
        body,
        end,
    })
}

fn for_<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("for");
    let for_ = scanner.next_token()?;

    let init = Box::new(declare(scanner)?);

    consume(scanner, TokenKind::Comma)?;
    let cond = expr_bp(scanner, BindingPower::ExpressionBoundary)?;

    consume(scanner, TokenKind::Comma)?;
    let inc = Box::new(assign(scanner)?);

    consume(scanner, TokenKind::Do)?;
    let body = block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::For {
        for_,
        init,
        cond,
        inc,
        body,
        end,
    })
}

fn block_until_else_or_end<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    let mut stmts = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::End
        && scanner.peek_token(0)?.kind != TokenKind::Else
    {
        trace!("declaration in block until else");
        stmts.push(statement(scanner)?);
    }

    Ok(stmts)
}

fn block<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    let mut stmts = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::End {
        trace!("declaration in block");
        stmts.push(statement(scanner)?);
    }

    Ok(stmts)
}

fn expr_bp<'a>(scanner: &mut Scanner<'a>, min_bp: BindingPower) -> Result<Expr<'a>, PiccoloError> {
    trace!("expr_bp {:?}", min_bp);
    let lhs_token = scanner.next_token()?;
    let mut lhs = if lhs_token.is_value() {
        trace!("atom");
        Expr::Literal { literal: lhs_token }
    } else if lhs_token.kind == TokenKind::Identifier {
        trace!("variable");
        Expr::Variable {
            variable: lhs_token,
        }
    } else if lhs_token.kind == TokenKind::LeftParen {
        trace!("grouping");
        let expr = Box::new(expr_bp(scanner, BindingPower::ExpressionBoundary)?);
        let right_paren = consume(scanner, TokenKind::RightParen).map_err(|e| {
            e.msg_string(format!("in expression starting on line {}", lhs_token.line))
        })?;
        Expr::Paren { right_paren, expr }
    } else if lhs_token.kind == TokenKind::Eof {
        trace!("eof");
        return Err(PiccoloError::new(ErrorKind::ExpectedExpression {
            got: lhs_token.to_string(),
        })
        .line(lhs_token.line));
    } else {
        trace!("prefix");
        let pbp = prefix_binding_power(lhs_token.kind);
        if pbp != BindingPower::None {
            let rhs = expr_bp(scanner, pbp)?;
            Expr::Unary {
                op: lhs_token,
                rhs: Box::new(rhs),
            }
        } else {
            return Err(PiccoloError::new(ErrorKind::ExpectedExpression {
                got: lhs_token.to_string(),
            })
            .line(lhs_token.line));
        }
    };

    loop {
        let op_token = scanner.peek_token(0)?;
        if op_token.kind == TokenKind::Eof {
            break;
        }

        let op_bp = infix_binding_power(op_token.kind);

        if op_bp < min_bp {
            trace!("end of infix at {:?}", op_token.kind);
            break;
        }

        let op = scanner.next_token()?;
        let rhs = expr_bp(scanner, op_bp + 1)?;
        lhs = if matches!(op_bp, BindingPower::LogicalAnd | BindingPower::LogicalOr) {
            trace!("logical");
            Expr::Logical {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        } else {
            trace!("infix");
            Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }
    }

    Ok(lhs)
}

fn consume<'a>(scanner: &mut Scanner<'a>, kind: TokenKind) -> Result<Token<'a>, PiccoloError> {
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

fn prefix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Minus => BindingPower::Unary,
        TokenKind::Not => BindingPower::Unary,

        TokenKind::Identifier => BindingPower::None,
        _ => BindingPower::None,
    }
}

pub(crate) fn infix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Multiply => BindingPower::Factor,
        TokenKind::Divide => BindingPower::Factor,
        TokenKind::Modulo => BindingPower::Factor,

        TokenKind::Plus => BindingPower::Term,
        TokenKind::Minus => BindingPower::Term,

        TokenKind::Less => BindingPower::Comparison,
        TokenKind::Greater => BindingPower::Comparison,
        TokenKind::LessEqual => BindingPower::Comparison,
        TokenKind::GreaterEqual => BindingPower::Comparison,

        TokenKind::Equal => BindingPower::Equality,
        TokenKind::NotEqual => BindingPower::Equality,

        TokenKind::LogicalAnd => BindingPower::LogicalAnd,
        TokenKind::LogicalOr => BindingPower::LogicalOr,

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
    ExpressionBoundary = 1,
    LogicalOr = 2,
    LogicalAnd = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
);

#[cfg(test)]
mod test {
    use super::BindingPower;

    #[test]
    fn precedence_ord() {
        assert!(BindingPower::LogicalAnd > BindingPower::LogicalOr);
        assert!(BindingPower::LogicalAnd < BindingPower::Comparison);
        assert!(BindingPower::LogicalOr < BindingPower::Comparison);
    }
}
