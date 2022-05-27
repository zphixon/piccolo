//! Contains `Parser`, which converts a source of tokens into a Piccolo AST.

use crate::{ErrorKind, Expr, PiccoloError, Scanner, Stmt, Token, TokenKind};

/// Parse a stream of tokens into an AST. This method collects errors on statement
/// boundaries, continuing until the end of the file.
pub fn parse<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, Vec<PiccoloError>> {
    let mut ast = Vec::new();
    let mut errors = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::Eof {
        trace!("statement");

        match parse_statement(scanner) {
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

fn parse_statement<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    if scanner.peek_token(1)?.is_assign() {
        parse_assignment(scanner)
    } else if scanner.peek_token(1)?.kind == TokenKind::Declare {
        parse_declaration(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Break {
        parse_break(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Continue {
        parse_continue(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Retn {
        parse_retn(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Assert {
        parse_assert(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Do {
        parse_do(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::If {
        parse_if(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::While {
        parse_while(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::For {
        parse_for(scanner)
    } else if scanner.peek_token(0)?.kind == TokenKind::Fn {
        parse_fn(scanner)
    } else {
        trace!("expr");

        let token = *scanner.peek_token(0)?;
        let expr = parse_expression(scanner)?;

        Ok(Stmt::Expr { token, expr })
    }
}

fn parse_assignment<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("assign");

    let name = consume(scanner, TokenKind::Identifier)?;
    let op = scanner.next_token()?;
    let value = parse_expression(scanner)?;

    Ok(Stmt::Assignment { name, op, value })
}

fn parse_declaration<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("declare");

    let name = consume(scanner, TokenKind::Identifier)?;
    let op = consume(scanner, TokenKind::Declare)?;
    let value = parse_expression(scanner)?;

    Ok(Stmt::Declaration { name, op, value })
}

fn parse_break<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("break");

    let break_ = scanner.next_token()?;

    Ok(Stmt::Break { break_ })
}

fn parse_continue<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("continue");

    let continue_ = scanner.next_token()?;

    Ok(Stmt::Continue { continue_ })
}

fn parse_retn<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("retn");

    let retn = scanner.next_token()?;

    let value = if scanner.peek_token(0)?.kind == TokenKind::End {
        None
    } else {
        Some(parse_expression(scanner)?)
    };

    Ok(Stmt::Retn { retn, value })
}

fn parse_assert<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("assert");

    let assert = scanner.next_token()?;
    let value = parse_expression(scanner)?;

    Ok(Stmt::Assert { assert, value })
}

fn parse_do<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("do");

    scanner.next_token()?;
    let body = parse_block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::Block { end, body })
}

fn parse_if<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("if");

    let if_ = scanner.next_token()?;
    let cond = parse_expression(scanner)?;
    consume(scanner, TokenKind::Do)?;
    let then_block = block_until_else_or_end(scanner)?;

    let (else_, else_block) = if scanner.peek_token(0)?.kind == TokenKind::Else {
        (
            Some(consume(scanner, TokenKind::Else)?),
            Some(parse_block(scanner)?),
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

fn parse_while<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("while");
    let while_ = scanner.next_token()?;
    let cond = parse_expression(scanner)?;
    consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::While {
        while_,
        cond,
        body,
        end,
    })
}

fn parse_for<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("for");
    let for_ = scanner.next_token()?;

    let init = Box::new(parse_declaration(scanner)?);

    consume(scanner, TokenKind::Comma)?;
    let cond = parse_expression(scanner)?;

    consume(scanner, TokenKind::Comma)?;
    let inc = Box::new(parse_assignment(scanner)?);

    consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner)?;
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

fn parse_fn<'a>(scanner: &mut Scanner<'a>) -> Result<Stmt<'a>, PiccoloError> {
    trace!("fn");
    scanner.next_token()?;
    let name = consume(scanner, TokenKind::Identifier)?;

    consume(scanner, TokenKind::LeftParen)?;
    let args = parse_parameters(scanner)?;
    let arity = args.len();
    consume(scanner, TokenKind::RightParen)?;

    consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::Fn {
        name,
        args,
        arity,
        body,
        method: false,
        end,
    })
}

fn block_until_else_or_end<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    let mut stmts = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::End
        && scanner.peek_token(0)?.kind != TokenKind::Else
    {
        trace!("declaration in block until else");
        stmts.push(parse_statement(scanner)?);
    }

    Ok(stmts)
}

fn parse_block<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    let mut stmts = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::End {
        trace!("declaration in block");
        stmts.push(parse_statement(scanner)?);
    }

    Ok(stmts)
}

fn consume<'a>(scanner: &mut Scanner<'a>, kind: TokenKind) -> Result<Token<'a>, PiccoloError> {
    let tok = scanner.next_token()?;
    if tok.kind == kind {
        Ok(tok)
    } else {
        Err(PiccoloError::new(ErrorKind::UnexpectedToken {
            was_eof: tok.kind == TokenKind::Eof,
            exp: format!("{:?}", kind),
            got: format!("{:?}", tok.kind),
        })
        .line(tok.line))
    }
}

fn parse_expression<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("expression");
    parse_logic_or(scanner)
}

fn parse_logic_or<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("logic_or");
    let mut logic_and = parse_logic_and(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LogicalOr {
            let lhs = Box::new(logic_and);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_logic_or(scanner)?);
            logic_and = Expr::Logical { lhs, op, rhs };
        } else {
            return Ok(logic_and);
        }
    }
}

fn parse_logic_and<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("logic_and");
    let mut bit_or = parse_bit_or(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LogicalAnd {
            let lhs = Box::new(bit_or);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_logic_and(scanner)?);
            bit_or = Expr::Logical { lhs, op, rhs }
        } else {
            return Ok(bit_or);
        }
    }
}

// TODO: probably move these below equality
fn parse_bit_or<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("bit_or");
    let mut bit_xor = parse_bit_xor(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseOr {
            let lhs = Box::new(bit_xor);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_or(scanner)?);
            bit_xor = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_xor);
        }
    }
}

fn parse_bit_xor<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("bit_xor");
    let mut bit_and = parse_bit_and(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseXor {
            let lhs = Box::new(bit_and);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_xor(scanner)?);
            bit_and = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_and);
        }
    }
}

fn parse_bit_and<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("bit_and");
    let mut equality = parse_equality(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseAnd {
            let lhs = Box::new(equality);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_and(scanner)?);
            equality = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(equality);
        }
    }
}

fn parse_equality<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("equality");
    let mut comparison = parse_comparison(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::Equal | TokenKind::NotEqual) {
            let lhs = Box::new(comparison);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_equality(scanner)?);
            comparison = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(comparison);
        }
    }
}

fn parse_comparison<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("comparison");
    let mut bit_shift = parse_bit_shift(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if matches!(
            t.kind,
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual
        ) {
            let lhs = Box::new(bit_shift);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_comparison(scanner)?);
            bit_shift = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_shift);
        }
    }
}

fn parse_bit_shift<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("bit_shift");
    let mut term = parse_term(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::ShiftLeft | TokenKind::ShiftRight) {
            let lhs = Box::new(term);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_shift(scanner)?);
            term = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(term);
        }
    }
}

fn parse_term<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("term");
    let mut factor = parse_factor(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::Plus | TokenKind::Minus) {
            let lhs = Box::new(factor);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_factor(scanner)?);
            factor = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(factor);
        }
    }
}

fn parse_factor<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("factor");
    let mut unary = parse_unary(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if matches!(
            t.kind,
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo
        ) {
            let lhs = Box::new(unary);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_unary(scanner)?);
            unary = Expr::Binary { lhs, op, rhs }
        } else {
            return Ok(unary);
        }
    }
}

fn parse_unary<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("unary");
    let t = scanner.peek_token(0)?;
    if matches!(t.kind, TokenKind::Not | TokenKind::Minus) {
        let op = scanner.next_token()?;
        let rhs = Box::new(parse_unary(scanner)?);
        Ok(Expr::Unary { op, rhs })
    } else {
        parse_call(scanner)
    }
}

fn parse_call<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("call");
    let mut primary = parse_primary(scanner)?;

    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LeftParen {
            let callee = Box::new(primary);
            let paren = consume(scanner, TokenKind::LeftParen)?;
            let args = parse_arguments(scanner)?;
            consume(scanner, TokenKind::RightParen)?;
            let arity = args.len();
            primary = Expr::Call {
                callee,
                paren,
                arity,
                args,
            };
        } else if t.kind == TokenKind::Period {
            consume(scanner, TokenKind::Period)?;
            let object = Box::new(primary);
            let name = consume(scanner, TokenKind::Identifier)?;
            primary = Expr::Get { object, name };
        } else {
            return Ok(primary);
        }
    }
}

fn parse_primary<'a>(scanner: &mut Scanner<'a>) -> Result<Expr<'a>, PiccoloError> {
    trace!("primary");
    let t = scanner.peek_token(0)?;
    if t.kind == TokenKind::LeftParen {
        consume(scanner, TokenKind::LeftParen)?;

        let expr = Box::new(parse_expression(scanner)?);
        let right_paren = consume(scanner, TokenKind::RightParen)?;

        Ok(Expr::Paren { right_paren, expr })
    } else if t.is_value() {
        let literal = scanner.next_token()?;

        Ok(Expr::Literal { literal })
    } else if t.kind == TokenKind::Identifier {
        let variable = scanner.next_token()?;

        Ok(Expr::Variable { variable })
    } else if t.kind == TokenKind::Fn {
        let fn_ = consume(scanner, TokenKind::Fn)?;

        consume(scanner, TokenKind::LeftParen)?;
        let params = parse_parameters(scanner)?;
        let arity = params.len();
        consume(scanner, TokenKind::RightParen)?;

        consume(scanner, TokenKind::Do)?;
        let body = parse_block(scanner)?;
        let end = consume(scanner, TokenKind::End)?;

        Ok(Expr::Fn {
            fn_,
            args: params, // TODO
            arity,
            body,
            end,
        })
    } else if t.kind == TokenKind::Me {
        todo!()
    } else {
        let t = scanner.next_token()?;
        Err(PiccoloError::new(ErrorKind::ExpectedExpression {
            was_eof: t.kind == TokenKind::Eof,
            got: format!("{}", t.lexeme),
        })
        .line(t.line))
    }
}

fn parse_parameters<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Token<'a>>, PiccoloError> {
    trace!("parameters");
    let mut params = vec![];
    if scanner.peek_token(0)?.kind == TokenKind::Identifier {
        params.push(consume(scanner, TokenKind::Identifier)?);
        while scanner.peek_token(0)?.kind == TokenKind::Comma {
            consume(scanner, TokenKind::Comma)?;
            params.push(consume(scanner, TokenKind::Identifier)?);
        }
    }
    Ok(params)
}

fn parse_arguments<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Expr<'a>>, PiccoloError> {
    trace!("arguments");
    let mut args = vec![];
    if scanner.peek_token(0)?.kind != TokenKind::RightParen {
        args.push(parse_expression(scanner)?);
        while scanner.peek_token(0)?.kind == TokenKind::Comma {
            consume(scanner, TokenKind::Comma)?;
            args.push(parse_expression(scanner)?);
        }
    }
    Ok(args)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::debug::*;

    #[test]
    fn assign() {
        let src = "a += 3";
        let ast = parse(&mut Scanner::new(src)).unwrap();
        assert_eq!(
            ast,
            &[Stmt::Assignment {
                name: Token::new(TokenKind::Identifier, "a", 1),
                op: Token::new(TokenKind::PlusAssign, "+=", 1),
                value: Expr::Literal {
                    literal: Token::new(TokenKind::Integer(3), "3", 1),
                }
            }]
        );
    }

    #[test]
    #[ignore]
    fn path() {
        // TODO think about modules more
        let src = "a:b:c:d";
        let ast = parse(&mut Scanner::new(src)).unwrap();
        assert_eq!(
            ast,
            &[Stmt::Expr {
                token: Token::new(TokenKind::Identifier, "a", 1),
                expr: Expr::Path {
                    names: vec![
                        Token::new(TokenKind::Identifier, "a", 1),
                        Token::new(TokenKind::Identifier, "b", 1),
                        Token::new(TokenKind::Identifier, "c", 1),
                        Token::new(TokenKind::Identifier, "d", 1),
                    ],
                }
            }]
        );

        let src = "a:3";
        assert!(parse(&mut Scanner::new(src)).is_err());

        let src = "a:";
        assert!(parse(&mut Scanner::new(src)).is_err());

        let src = ":a";
        assert!(parse(&mut Scanner::new(src)).is_err());
    }

    #[test]
    fn pexp2() {
        let src = &[
            "(a)",
            "fn(a, b, c) do end",
            "a.b().c()(d)",
            "a()()()",
            "!-true",
            "--------1",
            "1*-3",
            "1*2*3*4",
            "a.b--c*a.d",
            "1<<-3*a()<<3",
            "\"\"+1<<3==8+3==4",
            "3&5==1",
            "3^5&1",
            "3^5|1",
            "1+2==3+4&&5+6==(7^8)",
        ];

        for src in src {
            let mut scanner = Scanner::new(src);
            println!(
                "{} -> {}",
                src,
                print_expression(&parse_expression(&mut scanner).unwrap())
            );
        }
    }
}
