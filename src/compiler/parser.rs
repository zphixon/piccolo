//! Contains `Parser`, which converts a source of tokens into a Piccolo AST.

use crate::{
    compiler::{
        ast::{Expr, Stmt},
        Scanner, Token, TokenKind, MAX_DEPTH,
    },
    error::PiccoloError,
    make_error, trace,
};

macro_rules! check_depth {
    ($scanner:expr, $depth:expr) => {
        if $depth > MAX_DEPTH {
            return Err(make_error!(SyntaxError)
                .msg("Maximum recursion depth reached")
                .pos($scanner.peek_token(0)?.pos));
        }
    };
}

pub fn parse(src: &str) -> Result<Vec<Stmt<'_>>, Vec<PiccoloError>> {
    let mut scanner = Scanner::new(src);
    parse_with(&mut scanner)
}

/// Parse a stream of tokens into an AST. This method collects errors on statement
/// boundaries, continuing until the end of the file.
pub fn parse_with<'a>(scanner: &mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, Vec<PiccoloError>> {
    let mut ast = Vec::new();
    let mut errors = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::Eof {
        trace!("statement {:?}", scanner.peek_token(0)?);

        match parse_statement(scanner, 0) {
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

fn parse_statement<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    if scanner.peek_token(1)?.kind == TokenKind::Declare {
        parse_declaration(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Break {
        parse_break(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Continue {
        parse_continue(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Return {
        parse_return(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Assert {
        parse_assert(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Do {
        parse_do(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::If {
        parse_if(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::While {
        parse_while(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::For {
        parse_for(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Fn {
        parse_fn(scanner, depth + 1)
    } else if scanner.peek_token(0)?.kind == TokenKind::Data {
        parse_data(scanner, depth + 1)
    } else {
        trace!("expr {:?}", scanner.peek_token(0)?);

        let token = *scanner.peek_token(0)?;
        let expr = parse_expression(scanner, depth + 1)?;

        if scanner.peek_token(0)?.is_assign() {
            let op = scanner.next_token()?;
            let value = parse_expression(scanner, depth + 1)?;
            Ok(Stmt::Assignment {
                lval: expr,
                op,
                rval: value,
            })
        } else {
            Ok(Stmt::Expr { token, expr })
        }
    }
}

fn parse_declaration<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("declare {:?}", scanner.peek_token(0)?);

    let name = consume(scanner, TokenKind::Identifier)?;
    let op = consume(scanner, TokenKind::Declare)?;
    let value = parse_expression(scanner, depth + 1)?;

    Ok(Stmt::Declaration { name, op, value })
}

fn parse_break<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("break {:?}", scanner.peek_token(0)?);

    let break_ = scanner.next_token()?;

    Ok(Stmt::Break { break_ })
}

fn parse_continue<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("continue {:?}", scanner.peek_token(0)?);

    let continue_ = scanner.next_token()?;

    Ok(Stmt::Continue { continue_ })
}

fn parse_return<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("return {:?}", scanner.peek_token(0)?);

    let return_ = scanner.next_token()?;

    let value = if scanner.peek_token(0)?.kind == TokenKind::End {
        None
    } else {
        Some(parse_expression(scanner, depth + 1)?)
    };

    Ok(Stmt::Return { return_, value })
}

fn parse_assert<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("assert {:?}", scanner.peek_token(0)?);

    let assert = scanner.next_token()?;
    let value = parse_expression(scanner, depth + 1)?;

    Ok(Stmt::Assert { assert, value })
}

fn parse_do<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("do {:?}", scanner.peek_token(0)?);

    let do_ = consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner, depth + 1)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::Block { do_, body, end })
}

fn parse_if<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("if {:?}", scanner.peek_token(0)?);

    let if_ = scanner.next_token()?;
    let cond = parse_expression(scanner, depth + 1)?;
    let do_ = consume(scanner, TokenKind::Do)?;
    let then_block = block_until_else_elseif_or_end(scanner, depth + 1)?;

    let (else_, else_block) = if scanner.peek_token(0)?.kind == TokenKind::Else {
        (
            Some(consume(scanner, TokenKind::Else)?),
            Some(parse_block(scanner, depth + 1)?),
        )
    } else if scanner.peek_token(0)?.kind == TokenKind::ElseIf {
        (
            Some(*scanner.peek_token(0)?),
            Some(parse_else_ifs(scanner, depth + 1)?),
        )
    } else {
        (None, None)
    };

    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::If {
        if_,
        cond,
        do_,
        then_block,
        else_,
        else_block,
        end,
    })
}

fn parse_else_ifs<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("elseif {:?}", scanner.peek_token(0)?);

    let elseif = consume(scanner, TokenKind::ElseIf)?;
    let cond = parse_expression(scanner, depth + 1)?;
    let do_ = consume(scanner, TokenKind::Do)?;
    let then_block = block_until_else_elseif_or_end(scanner, depth + 1)?;

    let (else_, else_block) = if scanner.peek_token(0)?.kind == TokenKind::Else {
        (
            Some(consume(scanner, TokenKind::Else)?),
            Some(parse_block(scanner, depth + 1)?),
        )
    } else if scanner.peek_token(0)?.kind == TokenKind::ElseIf {
        (
            Some(*scanner.peek_token(0)?),
            Some(parse_else_ifs(scanner, depth + 1)?),
        )
    } else {
        (None, None)
    };

    Ok(vec![Stmt::If {
        if_: elseif,
        cond,
        do_,
        then_block,
        else_,
        else_block,
        end: *scanner.peek_token(0)?,
    }])
}

fn parse_while<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("while {:?}", scanner.peek_token(0)?);

    let while_ = scanner.next_token()?;
    let cond = parse_expression(scanner, depth + 1)?;
    let do_ = consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner, depth + 1)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::While {
        while_,
        cond,
        do_,
        body,
        end,
    })
}

fn parse_for<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("for {:?}", scanner.peek_token(0)?);

    let for_ = scanner.next_token()?;

    if scanner.peek_token(1)?.kind == TokenKind::In {
        return parse_for_each(scanner, depth + 1, for_);
    }

    let init = Box::new(parse_declaration(scanner, depth + 1)?);

    consume(scanner, TokenKind::Comma)?;
    let cond = parse_expression(scanner, depth + 1)?;

    consume(scanner, TokenKind::Comma)?;

    let name = consume(scanner, TokenKind::Identifier)?;

    let inc_op = scanner.next_token()?;
    if !inc_op.is_assign() {
        return Err(make_error!(SyntaxError)
            .msg("Final clause of a `for` statement must be an assignment")
            .pos(inc_op.pos));
    }

    let inc_expr = parse_expression(scanner, depth + 1)?;

    let do_ = consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner, depth + 1)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::For {
        for_,
        init,
        cond,
        name,
        inc_op,
        inc_expr,
        do_,
        body,
        end,
    })
}

fn parse_for_each<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
    for_: Token<'a>,
) -> Result<Stmt<'a>, PiccoloError> {
    let item = consume(scanner, TokenKind::Identifier)?;
    consume(scanner, TokenKind::In)?;
    let iter = consume(scanner, TokenKind::Identifier)?;
    let do_ = consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner, depth + 1)?;
    let end = consume(scanner, TokenKind::End)?;
    Ok(Stmt::ForEach {
        for_,
        item,
        iter,
        do_,
        body,
        end,
    })
}

fn parse_fn<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("fn {:?}", scanner.peek_token(0)?);

    scanner.next_token()?;
    let name = consume(scanner, TokenKind::Identifier)?;

    consume(scanner, TokenKind::LeftParen)?;
    let args = parse_parameters(scanner, depth + 1)?;
    let arity = args.len();
    consume(scanner, TokenKind::RightParen)?;
    let method = args.first().is_some() && args.first().as_ref().unwrap().kind == TokenKind::Me;

    consume(scanner, TokenKind::Do)?;
    let body = parse_block(scanner, depth + 1)?;
    let end = consume(scanner, TokenKind::End)?;

    Ok(Stmt::Fn {
        name,
        args,
        arity,
        body,
        method,
        end,
    })
}

fn parse_data<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Stmt<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("data {:?}", scanner.peek_token(0)?);

    scanner.next_token()?;
    let name = consume(scanner, TokenKind::Identifier)?;
    consume(scanner, TokenKind::Do)?;

    let mut fields = Vec::new();
    let mut methods = Vec::new();

    while scanner.peek_token(0)?.kind != TokenKind::End
        && scanner.peek_token(0)?.kind != TokenKind::Fn
    {
        fields.push(parse_declaration(scanner, depth + 1)?);
    }

    while scanner.peek_token(0)?.kind != TokenKind::End {
        methods.push(parse_fn(scanner, depth + 1)?);
    }

    consume(scanner, TokenKind::End)?;
    Ok(Stmt::Data {
        name,
        methods,
        fields,
    })
}

fn block_until_else_elseif_or_end<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut stmts = Vec::new();
    while scanner.peek_token(0)?.kind != TokenKind::End
        && scanner.peek_token(0)?.kind != TokenKind::Else
        && scanner.peek_token(0)?.kind != TokenKind::ElseIf
    {
        trace!(
            "declaration in block until else {:?}",
            scanner.peek_token(0)?
        );
        stmts.push(parse_statement(scanner, depth + 1)?);
    }

    Ok(stmts)
}

fn parse_block<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Vec<Stmt<'a>>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut stmts = Vec::new();
    while scanner.peek_token(0)?.kind != TokenKind::End {
        trace!("declaration in block {:?}", scanner.peek_token(0)?);
        stmts.push(parse_statement(scanner, depth + 1)?);
    }

    Ok(stmts)
}

fn consume<'a>(scanner: &mut Scanner<'a>, kind: TokenKind) -> Result<Token<'a>, PiccoloError> {
    let tok = scanner.next_token()?;
    if tok.kind == kind {
        Ok(tok)
    } else {
        Err(make_error!(UnexpectedToken {
            was_eof: tok.kind == TokenKind::Eof,
            exp: format!("{kind:?}"),
            got: format!("{:?}", tok.kind),
        })
        .pos(tok.pos))
    }
}

fn parse_expression<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("expression {:?}", scanner.peek_token(0)?);

    parse_logic_or(scanner, depth + 1)
}

fn parse_logic_or<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut logic_and = parse_logic_and(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LogicalOr {
            trace!("logic_or {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(logic_and);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_logic_or(scanner, depth + 1)?);
            logic_and = Expr::Logical { lhs, op, rhs };
        } else {
            return Ok(logic_and);
        }
    }
}

fn parse_logic_and<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut bit_or = parse_bit_or(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LogicalAnd {
            trace!("logic_and {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(bit_or);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_logic_and(scanner, depth + 1)?);
            bit_or = Expr::Logical { lhs, op, rhs }
        } else {
            return Ok(bit_or);
        }
    }
}

// TODO: probably move these below equality
fn parse_bit_or<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut bit_xor = parse_bit_xor(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseOr {
            trace!("bit_or {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(bit_xor);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_or(scanner, depth + 1)?);
            bit_xor = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_xor);
        }
    }
}

fn parse_bit_xor<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut bit_and = parse_bit_and(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseXor {
            trace!("bit_xor {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(bit_and);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_xor(scanner, depth + 1)?);
            bit_and = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_and);
        }
    }
}

fn parse_bit_and<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut equality = parse_equality(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::BitwiseAnd {
            trace!("bit_and {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(equality);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_and(scanner, depth + 1)?);
            equality = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(equality);
        }
    }
}

fn parse_equality<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut comparison = parse_comparison(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::Equal | TokenKind::NotEqual) {
            trace!("equality {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(comparison);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_equality(scanner, depth + 1)?);
            comparison = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(comparison);
        }
    }
}

fn parse_comparison<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut bit_shift = parse_bit_shift(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if matches!(
            t.kind,
            TokenKind::Less | TokenKind::Greater | TokenKind::LessEqual | TokenKind::GreaterEqual
        ) {
            trace!("comparison {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(bit_shift);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_comparison(scanner, depth + 1)?);
            bit_shift = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(bit_shift);
        }
    }
}

fn parse_bit_shift<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut term = parse_term(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::ShiftLeft | TokenKind::ShiftRight) {
            trace!("bit_shift {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(term);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_bit_shift(scanner, depth + 1)?);
            term = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(term);
        }
    }
}

fn parse_term<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut factor = parse_factor(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if matches!(t.kind, TokenKind::Plus | TokenKind::Minus) {
            trace!("term {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(factor);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_factor(scanner, depth + 1)?);
            factor = Expr::Binary { lhs, op, rhs };
        } else {
            return Ok(factor);
        }
    }
}

fn parse_factor<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut unary = parse_unary(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if matches!(
            t.kind,
            TokenKind::Multiply | TokenKind::Divide | TokenKind::Modulo
        ) {
            trace!("factor {:?}", scanner.peek_token(0)?);

            let lhs = Box::new(unary);
            let op = scanner.next_token()?;
            let rhs = Box::new(parse_unary(scanner, depth + 1)?);
            unary = Expr::Binary { lhs, op, rhs }
        } else {
            return Ok(unary);
        }
    }
}

fn parse_unary<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let t = scanner.peek_token(0)?;
    if matches!(t.kind, TokenKind::Not | TokenKind::Minus) {
        trace!("unary {:?}", scanner.peek_token(0)?);

        let op = scanner.next_token()?;
        let rhs = Box::new(parse_unary(scanner, depth + 1)?);
        Ok(Expr::Unary { op, rhs })
    } else {
        parse_call(scanner, depth + 1)
    }
}

fn parse_call<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let mut primary = parse_primary(scanner, depth + 1)?;
    loop {
        let t = scanner.peek_token(0)?;
        if t.kind == TokenKind::LeftParen {
            trace!("call {:?}", scanner.peek_token(0)?);

            let callee = Box::new(primary);
            let paren = consume(scanner, TokenKind::LeftParen)?;
            let args = parse_arguments(scanner, depth + 1, TokenKind::RightParen)?;
            consume(scanner, TokenKind::RightParen)?;
            let arity = args.len();
            primary = Expr::Call {
                callee,
                paren,
                arity,
                args,
            };
        } else if t.kind == TokenKind::Period {
            trace!("get {:?}", scanner.peek_token(0)?);

            consume(scanner, TokenKind::Period)?;
            let object = Box::new(primary);
            let name = consume(scanner, TokenKind::Identifier)?;
            primary = Expr::Get { object, name };
        } else if t.kind == TokenKind::LeftBracket {
            trace!("index {:?}", scanner.peek_token(0)?);

            consume(scanner, TokenKind::LeftBracket)?;
            let object = Box::new(primary);
            let index = Box::new(parse_expression(scanner, depth + 1)?);
            let right_bracket = consume(scanner, TokenKind::RightBracket)?;
            primary = Expr::Index {
                right_bracket,
                object,
                index,
            }
        } else {
            return Ok(primary);
        }
    }
}

fn parse_primary<'a>(scanner: &mut Scanner<'a>, depth: usize) -> Result<Expr<'a>, PiccoloError> {
    check_depth!(scanner, depth);

    let t = scanner.peek_token(0)?;
    if t.kind == TokenKind::LeftParen {
        trace!("parenthetical {:?}", scanner.peek_token(0)?);

        consume(scanner, TokenKind::LeftParen)?;

        let expr = Box::new(parse_expression(scanner, depth + 1)?);
        let right_paren = consume(scanner, TokenKind::RightParen)?;

        Ok(Expr::Paren { right_paren, expr })
    } else if t.is_value() {
        trace!("literal {:?}", scanner.peek_token(0)?);
        let literal = scanner.next_token()?;

        Ok(Expr::Literal { literal })
    } else if t.kind == TokenKind::LeftBracket {
        trace!("array literal {:?}", scanner.peek_token(0)?);

        consume(scanner, TokenKind::LeftBracket)?;
        let values = parse_arguments(scanner, depth + 1, TokenKind::RightBracket)?;
        let right_bracket = consume(scanner, TokenKind::RightBracket)?;
        Ok(Expr::ArrayLiteral {
            right_bracket,
            values,
        })
    } else if t.kind == TokenKind::Identifier {
        trace!("variable {:?}", scanner.peek_token(0)?);

        let variable = scanner.next_token()?;
        Ok(Expr::Variable { variable })
    } else if t.kind == TokenKind::Fn {
        trace!("lambda {:?}", scanner.peek_token(0)?);

        let fn_ = consume(scanner, TokenKind::Fn)?;

        consume(scanner, TokenKind::LeftParen)?;
        let args = parse_parameters(scanner, depth + 1)?;
        let arity = args.len();
        consume(scanner, TokenKind::RightParen)?;

        consume(scanner, TokenKind::Do)?;
        let body = parse_block(scanner, depth + 1)?;
        let end = consume(scanner, TokenKind::End)?;

        Ok(Expr::Fn {
            fn_,
            args,
            arity,
            body,
            end,
        })
    } else if t.kind == TokenKind::Me {
        trace!("me {:?}", scanner.peek_token(0)?);

        let me = consume(scanner, TokenKind::Me)?;
        Ok(Expr::Variable { variable: me })
    } else {
        let t = scanner.next_token()?;
        Err(make_error!(ExpectedExpression {
            was_eof: t.kind == TokenKind::Eof,
            got: t.lexeme.to_string(),
        })
        .pos(t.pos))
    }
}

fn parse_parameters<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
) -> Result<Vec<Token<'a>>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("parameters {:?}", scanner.peek_token(0)?);

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

fn parse_arguments<'a>(
    scanner: &mut Scanner<'a>,
    depth: usize,
    end: TokenKind,
) -> Result<Vec<Expr<'a>>, PiccoloError> {
    check_depth!(scanner, depth);
    trace!("arguments {:?}", scanner.peek_token(0)?);

    let mut args = vec![];
    if scanner.peek_token(0)?.kind != end {
        args.push(parse_expression(scanner, depth + 1)?);
        while scanner.peek_token(0)?.kind == TokenKind::Comma {
            consume(scanner, TokenKind::Comma)?;
            args.push(parse_expression(scanner, depth + 1)?);
        }
    }
    Ok(args)
}

#[cfg(test)]
mod test_parser {
    use super::*;

    #[test]
    fn parse_op_assign() {
        let src = "a += 3";
        let ast = parse(src).unwrap();
        assert_eq!(
            ast,
            &[Stmt::Assignment {
                lval: Expr::Variable {
                    variable: Token::identifier("a")
                },
                op: Token::test(TokenKind::PlusAssign),
                rval: Expr::Literal {
                    literal: Token::test(TokenKind::Integer(3)),
                }
            }]
        );
    }

    #[test]
    #[ignore]
    fn parse_path() {
        // TODO think about modules more
        let src = "a:b:c:d";
        let ast = parse(src).unwrap();
        assert_eq!(
            ast,
            &[Stmt::Expr {
                token: Token::identifier("a"),
                expr: Expr::Path {
                    names: vec![
                        Token::identifier("a"),
                        Token::identifier("b"),
                        Token::identifier("c"),
                        Token::identifier("d"),
                    ],
                }
            }]
        );

        let src = "a:3";
        assert!(parse(src).is_err());

        let src = "a:";
        assert!(parse(src).is_err());

        let src = ":a";
        assert!(parse(src).is_err());
    }

    #[test]
    fn parse_many_expressions() {
        let src = [
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
            let _ast = parse_expression(&mut scanner, 0).unwrap();
        }
    }
}
