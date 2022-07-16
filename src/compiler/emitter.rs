//! Bytecode compiler.
#![allow(clippy::too_many_arguments)]

use crate::{
    compiler::{
        ast::{Ast, Expr, Stmt},
        Pos, Token, TokenKind, Variable, MAX_DEPTH,
    },
    error::PiccoloError,
    make_error,
    runtime::{
        chunk::{Chunk, Module},
        interner::Interner,
        op::Opcode,
        value::{Constant, Function},
        Arity,
    },
    trace,
};
use fnv::FnvHashMap;

macro_rules! check_depth {
    ($depth:tt, $pos_haver:expr) => {
        if $depth > MAX_DEPTH {
            return Err(make_error!(SyntaxError)
                .msg("Maximum recursion depth reached")
                .pos($pos_haver.pos));
        }
    };
}

// top level compile into chunk index 0
// encounter a function, start compiling into a new chunk index
// after we end the function collect upvalues

pub fn compile(interner: &mut Interner, ast: &Ast) -> Result<Module, Vec<PiccoloError>> {
    let mut emitter = Emitter::new();
    compile_with(&mut emitter, interner, ast)?;
    Ok(emitter.into_module())
}

pub fn compile_with(
    emitter: &mut Emitter,
    interner: &mut Interner,
    ast: &Ast,
) -> Result<(), Vec<PiccoloError>> {
    let errors: Vec<_> = ast
        .iter()
        .map(|stmt| compile_stmt(emitter, interner, 0, stmt))
        .filter_map(Result::err)
        .collect();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[rustfmt::skip]
fn compile_stmt(emitter: &mut Emitter, interner: &mut Interner, depth: usize, stmt: &Stmt) -> Result<(), PiccoloError> {
    check_depth!(depth, stmt.token());

    match stmt {
        Stmt::Expr { token, expr }
            => compile_expr_stmt(emitter, interner, depth + 1, *token, expr),
        Stmt::Block { do_, body, end }
            => compile_block(emitter, interner, depth + 1, *do_, body, *end),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(emitter, interner, depth + 1, *name, value),
        Stmt::Assignment { lval, op, rval }
            => compile_assignment(emitter, interner, depth + 1, lval, *op, rval),
        Stmt::If { if_, cond, do_, then_block, else_, else_block, end }
            => compile_if(emitter, interner, depth + 1, *if_, cond, *do_, then_block, else_.as_ref(), else_block.as_ref(), *end),
        Stmt::While { while_, cond, do_, body, end }
            => compile_while(emitter, interner, depth + 1, *while_, cond, *do_, body, *end),
        Stmt::For { for_, init, cond, name, inc_op, inc_expr, do_, body, end }
            => compile_for(emitter, interner, depth + 1, *for_, init.as_ref(), cond, *name, *inc_op, inc_expr, *do_, body, *end),
        Stmt::ForEach { for_, item, iter, do_, body, end }
            => compile_for_each(emitter, interner, depth + 1, *for_, *item, *iter, *do_, body, *end),
        Stmt::Fn { name, args, arity, body, method, end }
            => compile_fn(emitter, interner, depth + 1, *name, args, *arity, body, *method, *end),
        Stmt::Break { break_ }
            => compile_break(emitter, depth + 1, *break_),
        Stmt::Continue { continue_ }
            => compile_continue(emitter, depth + 1, *continue_),
        Stmt::Return { return_, value }
            => compile_return(emitter, interner, depth + 1, *return_, value.as_ref()),
        Stmt::Assert { assert, value }
            => compile_assert(emitter, interner, depth + 1, *assert, value),
        Stmt::Data { name, methods, fields }
            => compile_data(emitter, interner, depth + 1, *name, methods, fields),
    }
}

#[rustfmt::skip]
fn compile_expr(emitter: &mut Emitter, interner: &mut Interner, depth: usize, expr: &Expr) -> Result<(), PiccoloError> {
    check_depth!(depth, expr.token());

    match expr {
        Expr::Literal { literal }
            => compile_literal(emitter, interner, depth + 1, *literal),
        Expr::ArrayLiteral { right_bracket, values }
            => compile_array_literal(emitter, interner, depth + 1, *right_bracket, values),
        Expr::Paren { right_paren, expr }
            => compile_paren(emitter, interner, depth + 1, *right_paren, expr),
        Expr::Variable { variable }
            => compile_variable(emitter, interner, depth + 1, *variable),
        Expr::Unary { op, rhs }
            => compile_unary(emitter, interner, depth + 1, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => compile_binary(emitter, interner, depth + 1, lhs, *op, rhs),
        Expr::Logical { lhs, op, rhs }
            => compile_logical(emitter, interner, depth + 1, lhs, *op, rhs),
        Expr::Call { callee, paren, arity, args }
            => compile_call(emitter, interner, depth + 1, callee, *paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(emitter, interner, depth + 1, name, args),
        Expr::Get { object, name }
            => compile_get(emitter, interner, depth + 1, object, *name),
        Expr::Index { right_bracket, object, index }
            => compile_index(emitter, interner, depth + 1, *right_bracket, object, index),
        Expr::Fn { fn_, args, arity, body, end }
            => compile_lambda(emitter, interner, depth + 1, *fn_, args, *arity, body, *end),
        _ => Err(PiccoloError::todo(format!("compile_expr: {expr:#?}"))),
    }
}

fn compile_expr_stmt(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    token: Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile expr stmt", token.pos);
    check_depth!(depth, token);

    compile_expr(emitter, interner, depth + 1, expr)?;
    emitter.add_instruction(Opcode::Pop, token.pos);
    Ok(())
}

fn compile_block(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    _do_: Token,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile block", _do_.pos);
    check_depth!(depth, end);

    emitter.begin_scope();
    for stmt in body {
        compile_stmt(emitter, interner, depth + 1, stmt)?;
    }
    emitter.end_scope(end.pos);
    Ok(())
}

fn compile_declaration(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    name: Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile decl {}", name.pos, name.lexeme);
    check_depth!(depth, name);

    compile_expr(emitter, interner, depth + 1, value)?;
    emitter.make_variable(interner, name)
}

fn compile_assignment(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    lval: &Expr,
    op: Token,
    rval: &Expr,
) -> Result<(), PiccoloError> {
    trace!(
        "{} compile assign {} {} {}",
        op.pos,
        lval.token().lexeme,
        op.lexeme,
        rval.token().lexeme
    );
    check_depth!(depth, lval.token());

    if let Expr::Variable { variable: name } = lval {
        let name = *name;
        if let Some(opcode) = op.assign_by_mutate_op() {
            // if this is an assignment-by-mutation operator, first get the value of the variable
            compile_variable(emitter, interner, depth + 1, name)?;

            // calculate the value to mutate with
            compile_expr(emitter, interner, depth + 1, rval)?;

            // then mutate
            emitter.add_instruction(opcode, op.pos);
        } else {
            // otherwise just calculate the value
            compile_expr(emitter, interner, depth + 1, rval)?;
        }

        // then assign
        emitter.add_set_variable(name)?;
    } else if let Expr::Get { object, name } = lval {
        compile_expr(emitter, interner, depth + 1, rval)?;
        compile_expr(emitter, interner, depth + 1, object)?;
        let ptr = interner.allocate_str(name.lexeme);
        emitter.add_constant(Constant::StringPtr(ptr), name.pos);
        emitter.add_instruction(Opcode::Set, op.pos);
    } else if let Expr::Index { object, index, .. } = lval {
        compile_expr(emitter, interner, depth + 1, rval)?;
        compile_expr(emitter, interner, depth + 1, object)?;
        compile_expr(emitter, interner, depth + 1, index)?;
        emitter.add_instruction(Opcode::Set, op.pos);
    } else {
        return Err(make_error!(SyntaxError)
            .msg_string(format!(
                "Cannot use {} expression as left-hand of assignment",
                lval.token().lexeme
            ))
            .pos(lval.token().pos));
    }

    Ok(())
}

fn compile_if(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    if_: Token,
    cond: &Expr,
    do_: Token,
    then_block: &[Stmt],
    else_: Option<&Token>,
    else_block: Option<&Vec<Stmt>>,
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile if", if_.pos);
    check_depth!(depth, if_);

    // compile the condition
    compile_expr(emitter, interner, depth + 1, cond)?;

    if let (Some(else_), Some(else_block)) = (else_, else_block) {
        // if the condition is false, jump to patch_jump(jump_else)
        let jump_else = emitter.start_jump(Opcode::JumpFalse(0), if_.pos);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.pos);
        // compile the then block
        compile_block(emitter, interner, depth + 1, do_, then_block, *else_)?;
        // jump unconditionally past the else block to patch_jump(end_jump)
        let end_jump = emitter.start_jump(Opcode::JumpForward(0), else_.pos);

        // jump here if the condition is false
        emitter.patch_jump(jump_else);
        // pop the condition
        emitter.add_instruction(Opcode::Pop, else_.pos);
        // compile the else block
        compile_block(emitter, interner, depth + 1, *else_, else_block, end)?;

        emitter.patch_jump(end_jump);
    } else {
        // there is no else block, jump to patch_jump(jump_end) if false
        let jump_end = emitter.start_jump(Opcode::JumpFalse(0), if_.pos);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.pos);

        // compile then block
        compile_block(emitter, interner, depth + 1, do_, then_block, end)?;

        // jump over the condition pop if we jumped over the else block
        let jump_pop = emitter.start_jump(Opcode::JumpForward(0), end.pos);
        emitter.patch_jump(jump_end);
        emitter.add_instruction(Opcode::Pop, end.pos);
        emitter.patch_jump(jump_pop);
    }

    Ok(())
}

fn compile_while(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    while_: Token,
    cond: &Expr,
    do_: Token,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile while", while_.pos);
    check_depth!(depth, while_);

    // loop condition
    let loop_start = emitter.start_loop_jumps();
    compile_expr(emitter, interner, depth + 1, cond)?;

    // jump to the end if the condition was false, pop if true
    let exit_jump = emitter.start_jump(Opcode::JumpFalse(0), while_.pos);
    emitter.add_instruction(Opcode::Pop, while_.pos);

    // loop body
    compile_block(emitter, interner, depth + 1, do_, body, end)?;

    // here after the body if we encounter a continue
    emitter.patch_continue_jumps();

    // jump back to the loop condition
    emitter.add_jump_back(loop_start, end.pos);

    // pop the condition after false
    emitter.patch_jump(exit_jump);
    emitter.add_instruction(Opcode::Pop, end.pos);

    // here after the whole thing if we encounter a break
    emitter.patch_break_jumps();

    Ok(())
}

fn compile_for(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    for_: Token,
    init: &Stmt,
    cond: &Expr,
    name: Token,
    inc_op: Token,
    inc_expr: &Expr,
    do_: Token,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile for", for_.pos);
    check_depth!(depth, for_);

    // initializer
    emitter.begin_scope();
    compile_stmt(emitter, interner, depth + 1, init)?;

    // condition
    let start_offset = emitter.start_loop_jumps();
    compile_expr(emitter, interner, depth + 1, cond)?;

    // if false jump to the end
    let end_jump = emitter.start_jump(Opcode::JumpFalse(0), for_.pos);
    emitter.add_instruction(Opcode::Pop, for_.pos);

    // loop body
    compile_block(emitter, interner, depth + 1, do_, body, end)?;

    // here if we encounter a continue
    emitter.patch_continue_jumps();

    // increment
    compile_assignment(
        emitter,
        interner,
        depth + 1,
        &Expr::Variable { variable: name },
        inc_op,
        inc_expr,
    )?;

    // unconditional jump back to condition
    emitter.add_jump_back(start_offset, end.pos);

    // here if condition is false
    emitter.patch_jump(end_jump);
    emitter.add_instruction(Opcode::Pop, end.pos);

    // here if we encounter a break
    emitter.patch_break_jumps();

    // pop loop variable (local variable)
    emitter.end_scope(end.pos);

    Ok(())
}

fn compile_for_each(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    for_: Token,
    item: Token,
    iter: Token,
    do_: Token,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile for_each", for_.pos);
    check_depth!(depth, for_);

    let index_name = format!("idx_of_{}_in_{}", item.lexeme, iter.lexeme);
    let index = Token::new(TokenKind::Identifier, &index_name, item.pos);

    emitter.begin_scope();
    emitter.add_constant(Constant::Integer(0), for_.pos);
    emitter.make_variable(interner, index)?;

    let start = emitter.start_loop_jumps();

    emitter.add_get_variable(interner, index)?;
    compile_variable(emitter, interner, depth, iter)?;
    let ptr = interner.allocate_str("len");
    emitter.add_constant(Constant::StringPtr(ptr), for_.pos);
    emitter.add_instruction(Opcode::Get, for_.pos);
    emitter.add_instruction(Opcode::Less, for_.pos);

    let end_jump = emitter.start_jump(Opcode::JumpFalse(0), for_.pos);
    emitter.add_instruction(Opcode::Pop, for_.pos);

    emitter.begin_scope();
    compile_declaration(
        emitter,
        interner,
        depth + 1,
        item,
        &Expr::Index {
            right_bracket: Token::new(TokenKind::RightBracket, "[", item.pos),
            object: Box::new(Expr::Variable { variable: iter }),
            index: Box::new(Expr::Variable { variable: index }),
        },
    )?;
    compile_block(emitter, interner, depth + 1, do_, body, end)?;
    emitter.end_scope(end.pos);

    emitter.patch_continue_jumps();

    emitter.add_constant(Constant::Integer(1), end.pos);
    emitter.add_instruction(Opcode::Add, end.pos);

    emitter.add_jump_back(start, end.pos);
    emitter.patch_jump(end_jump);
    emitter.add_instruction(Opcode::Pop, end.pos);
    emitter.patch_break_jumps();
    emitter.end_scope(end.pos);

    Ok(())
}

fn compile_fn(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    name: Token,
    args: &[Token],
    arity: usize,
    body: &[Stmt],
    _method: bool,
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile fn {}", name.pos, name.lexeme);
    check_depth!(depth, name);

    // if the function is global, define a global variable
    if !emitter.current_context().is_local() {
        emitter.make_global_ident(interner, name);
    }

    emitter.begin_context();
    emitter.begin_scope();

    // will always be local
    emitter.make_variable(interner, name)?;
    for arg in args {
        emitter.make_variable(interner, *arg)?;
    }

    // don't use compile_block because we've already started a new scope
    for stmt in body {
        compile_stmt(emitter, interner, depth + 1, stmt)?;
    }

    emitter.add_instruction(Opcode::Nil, end.pos);
    emitter.add_instruction(Opcode::Return, end.pos);

    let _locals = emitter.end_scope(end.pos);
    let chunk_index = emitter.end_context();

    let function = Function {
        arity: Arity::Exact(arity),
        name: interner.allocate_str(name.lexeme),
        chunk: chunk_index,
    };

    emitter.add_constant(Constant::Function(function), name.pos);

    emitter.make_variable(interner, name)?;

    Ok(())
}

fn compile_break(emitter: &mut Emitter, depth: usize, break_: Token) -> Result<(), PiccoloError> {
    trace!("{} compile break", break_.pos);
    check_depth!(depth, break_);

    let offset = emitter.start_jump(Opcode::JumpForward(0), break_.pos);
    emitter.add_break(offset, break_)?;

    Ok(())
}

fn compile_continue(
    emitter: &mut Emitter,
    depth: usize,
    continue_: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile continue", continue_.pos);
    check_depth!(depth, continue_);

    let offset = emitter.start_jump(Opcode::JumpForward(0), continue_.pos);
    emitter.add_continue(offset, continue_)?;

    Ok(())
}

fn compile_return(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    return_: Token,
    expr: Option<&Expr>,
) -> Result<(), PiccoloError> {
    trace!("{} compile return", return_.pos);
    check_depth!(depth, return_);

    if let Some(expr) = expr {
        compile_expr(emitter, interner, depth + 1, expr)?;
    } else {
        emitter.add_instruction(Opcode::Nil, return_.pos);
    }

    emitter.add_instruction(Opcode::Return, return_.pos);

    Ok(())
}

fn compile_assert(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    assert: Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile assert", assert.pos);
    check_depth!(depth, assert);

    compile_expr(emitter, interner, depth + 1, value)?;

    let expr = super::ast::print_expression(value);
    let ptr = interner.allocate_string(expr);
    let c = emitter.make_constant(Constant::StringPtr(ptr));
    emitter.add_instruction(Opcode::Assert(c), assert.pos);

    Ok(())
}

fn compile_data(
    _emitter: &mut Emitter,
    _interner: &mut Interner,
    depth: usize,
    name: Token,
    _methods: &[Stmt],
    _fields: &[Stmt],
) -> Result<(), PiccoloError> {
    check_depth!(depth, name);

    Err(PiccoloError::todo(format!("compile_data {}", name.lexeme)))
}

fn compile_literal(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    literal: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile literal", literal.pos);
    check_depth!(depth, literal);

    match literal.kind {
        TokenKind::True => emitter.add_instruction(Opcode::Bool(true), literal.pos),
        TokenKind::False => emitter.add_instruction(Opcode::Bool(false), literal.pos),
        TokenKind::Integer(v) if u16::try_from(v).is_ok() => {
            emitter.add_instruction(Opcode::Integer(v.try_into().unwrap()), literal.pos)
        }
        TokenKind::Nil => emitter.add_instruction(Opcode::Nil, literal.pos),
        TokenKind::String => {
            let string = crate::compiler::escape_string(literal.lexeme)?;
            let ptr = interner.allocate_string(string);
            emitter.add_constant(Constant::StringPtr(ptr), literal.pos);
        }
        _ => emitter.add_constant(Constant::try_from(interner, literal)?, literal.pos),
    }

    Ok(())
}

fn compile_array_literal(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    right_bracket: Token,
    values: &[Expr],
) -> Result<(), PiccoloError> {
    check_depth!(depth, right_bracket);

    for value in values.iter() {
        compile_expr(emitter, interner, depth + 1, value)?;
    }

    emitter.add_instruction(
        Opcode::Array(values.len().try_into().unwrap()),
        right_bracket.pos,
    );

    Ok(())
}

fn compile_paren(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    right_paren: Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile paren", right_paren.pos);
    check_depth!(depth, right_paren);

    compile_expr(emitter, interner, depth + 1, expr)
        .map_err(|e| e.msg_string(format!("in expression starting on pos {}", right_paren.pos)))
}

fn compile_variable(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    variable: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile variable {}", variable.pos, variable.lexeme);
    check_depth!(depth, variable);

    emitter.add_get_variable(interner, variable)
}

fn compile_unary(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile unary {}", op.pos, op.lexeme);
    check_depth!(depth, op);

    compile_expr(emitter, interner, depth + 1, rhs)?;

    match op.kind {
        TokenKind::Minus => emitter.add_instruction(Opcode::Negate, op.pos),
        TokenKind::Not => emitter.add_instruction(Opcode::Not, op.pos),
        _ => unreachable!("unary {:?}", op),
    };

    Ok(())
}

fn compile_binary(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    lhs: &Expr,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile binary {}", op.pos, op.lexeme);
    check_depth!(depth, lhs.token());

    compile_expr(emitter, interner, depth + 1, lhs)?;
    compile_expr(emitter, interner, depth + 1, rhs)?;

    match op.kind {
        TokenKind::Plus => emitter.add_instruction(Opcode::Add, op.pos),
        TokenKind::Minus => emitter.add_instruction(Opcode::Subtract, op.pos),
        TokenKind::Divide => emitter.add_instruction(Opcode::Divide, op.pos),
        TokenKind::Multiply => emitter.add_instruction(Opcode::Multiply, op.pos),
        TokenKind::Equal => emitter.add_instruction(Opcode::Equal, op.pos),
        TokenKind::NotEqual => {
            emitter.add_instruction(Opcode::Equal, op.pos);
            emitter.add_instruction(Opcode::Not, op.pos);
        }
        TokenKind::Greater => emitter.add_instruction(Opcode::Greater, op.pos),
        TokenKind::GreaterEqual => emitter.add_instruction(Opcode::GreaterEqual, op.pos),
        TokenKind::Less => emitter.add_instruction(Opcode::Less, op.pos),
        TokenKind::LessEqual => emitter.add_instruction(Opcode::LessEqual, op.pos),
        TokenKind::Modulo => emitter.add_instruction(Opcode::Modulo, op.pos),

        TokenKind::ShiftLeft => emitter.add_instruction(Opcode::ShiftLeft, op.pos),
        TokenKind::ShiftRight => emitter.add_instruction(Opcode::ShiftRight, op.pos),
        TokenKind::BitwiseAnd => emitter.add_instruction(Opcode::BitAnd, op.pos),
        TokenKind::BitwiseOr => emitter.add_instruction(Opcode::BitOr, op.pos),
        TokenKind::BitwiseXor => emitter.add_instruction(Opcode::BitXor, op.pos),

        _ => unreachable!("binary {:?}", op),
    }

    Ok(())
}

fn compile_logical(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    lhs: &Expr,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile logical {}", op.pos, op.lexeme);
    check_depth!(depth, lhs.token());

    let jump_op = match op.kind {
        TokenKind::LogicalAnd => Opcode::JumpFalse(0),
        TokenKind::LogicalOr => Opcode::JumpTrue(0),
        _ => unreachable!("op {:?} for logical", op),
    };

    compile_expr(emitter, interner, depth + 1, lhs)?;

    let short = emitter.start_jump(jump_op, op.pos);
    emitter.add_instruction(Opcode::Pop, op.pos);

    compile_expr(emitter, interner, depth + 1, rhs)?;

    emitter.patch_jump(short);

    Ok(())
}

fn compile_call(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    callee: &Expr,
    paren: Token,
    arity: usize,
    args: &[Expr],
) -> Result<(), PiccoloError> {
    trace!("{} compile call", paren.pos);
    check_depth!(depth, callee.token());

    compile_expr(emitter, interner, depth + 1, callee)?;
    for arg in args {
        compile_expr(emitter, interner, depth + 1, arg)?;
    }
    emitter.add_instruction(Opcode::Call(arity as u16), paren.pos);
    Ok(())
}

fn compile_get(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    object: &Expr,
    name: Token,
) -> Result<(), PiccoloError> {
    check_depth!(depth, object.token());

    compile_expr(emitter, interner, depth + 1, object)?;
    interner.allocate_str(name.lexeme);
    emitter.add_constant(
        Constant::StringPtr(interner.allocate_str(name.lexeme)),
        name.pos,
    );
    emitter.add_instruction(Opcode::Get, name.pos);
    Ok(())
}

fn compile_index(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    right_bracket: Token,
    object: &Expr,
    index: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} compile index", right_bracket.pos);
    check_depth!(depth, right_bracket);

    compile_expr(emitter, interner, depth + 1, object)?;
    compile_expr(emitter, interner, depth + 1, index)?;
    emitter.add_instruction(Opcode::Get, right_bracket.pos);
    Ok(())
}

fn compile_lambda(
    emitter: &mut Emitter,
    interner: &mut Interner,
    depth: usize,
    fn_: Token,
    args: &[Token],
    arity: usize,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} compile lambda", fn_.pos);
    check_depth!(depth, fn_);

    emitter.begin_context();
    emitter.begin_scope();

    // ??? since recursion is a hack we need a fake local so that the slots are all correct
    emitter
        .current_context_mut()
        .add_local(Token::new(TokenKind::Fn, "", fn_.pos));
    for arg in args {
        emitter.make_variable(interner, *arg)?;
    }

    for stmt in body {
        // don't use compile_block because we've already started a new scope
        compile_stmt(emitter, interner, depth + 1, stmt)?;
    }

    emitter.add_instruction(Opcode::Nil, end.pos);
    emitter.add_instruction(Opcode::Return, end.pos);

    let chunk_index = emitter.end_context();

    let function = Function {
        arity: Arity::Exact(arity),
        name: interner.allocate_str("<anon>"),
        chunk: chunk_index,
    };

    let constant = emitter.make_constant(Constant::Function(function));
    emitter.add_instruction(Opcode::Constant(constant), fn_.pos);

    Ok(())
}

// represents more of a scope
// TODO refactor to an enum?
#[derive(Default, Debug)]
struct EmitterContext {
    //context_type: ContextType,
    chunk_index: usize,
    locals: Vec<Variable>,
    depth: u16,
    //upvalues: Vec<Upvalue>,
}

impl EmitterContext {
    fn new(chunk_index: usize) -> Self {
        Self {
            chunk_index,
            locals: Vec::new(),
            depth: 0,
        }
    }

    fn add_local(&mut self, name: Token) {
        self.locals.push(Variable::Local {
            name: name.lexeme.to_string(),
            depth: self.scope_depth(),
            slot: self
                .locals
                .len()
                .try_into()
                .expect("too many local variables"),
        });
    }

    fn is_local(&self) -> bool {
        self.depth > 0
    }

    fn scope_depth(&self) -> u16 {
        self.depth
    }

    fn begin_scope(&mut self) {
        self.depth += 1;
    }

    fn end_scope(&mut self) -> Vec<Variable> {
        self.depth -= 1;
        let index = self
            .locals
            .iter()
            .enumerate()
            .find_map(|(i, l)| match l {
                Variable::Local { depth, .. } if *depth > self.depth => Some(i),
                _ => None,
            })
            .unwrap_or(self.locals.len());
        self.locals.split_off(index)
    }

    fn get_variable(&self, name: Token) -> Option<Variable> {
        trace!("{} get variable {}", name.pos, name.lexeme);
        for local in self.locals.iter().rev() {
            if local.name() == name.lexeme {
                return Some(local.clone());
            }
        }

        None
    }

    fn get_local_depth(&self, name: Token) -> Option<u16> {
        trace!("{} get local depth {}", name.pos, name.lexeme);
        for local in self.locals.iter().rev() {
            if local.name() == name.lexeme {
                if let Variable::Local { depth, .. } = local {
                    return Some(*depth);
                }
            }
        }

        None
    }
}

/// Bytecode compiler object
///
/// Construct an Emitter, interner: &mut Interner, and pass a `&mut` reference to `compile_ast` along with
/// the abstract syntax tree. Extract the `Chunk` with `current_chunk{_mut}()` or
/// `into_chunk()`.
pub struct Emitter {
    module: Module,
    children: Vec<EmitterContext>,
    //errors: Vec<PiccoloError>,
    global_identifiers: FnvHashMap<String, Variable>,
    continue_offsets: Vec<Vec<usize>>,
    break_offsets: Vec<Vec<usize>>,
}

impl Emitter {
    pub fn new() -> Self {
        Emitter {
            module: Module::new(),
            children: vec![EmitterContext::default()],
            global_identifiers: FnvHashMap::default(),
            continue_offsets: vec![],
            break_offsets: vec![],
        }
    }

    fn current_context(&self) -> &EmitterContext {
        self.children.last().unwrap()
    }

    fn current_context_mut(&mut self) -> &mut EmitterContext {
        self.children.last_mut().unwrap()
    }

    pub fn reset_after_errors(&mut self) {
        while self.current_context().is_local() {
            self.current_context_mut().end_scope();
        }
    }

    pub fn current_chunk(&self) -> &Chunk {
        self.module().chunk(self.current_context().chunk_index)
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        let index = self.current_context().chunk_index;
        self.module_mut().chunk_mut(index)
    }

    pub fn module(&self) -> &Module {
        &self.module
    }

    pub(crate) fn module_mut(&mut self) -> &mut Module {
        &mut self.module
    }

    pub fn into_module(self) -> Module {
        self.module
    }

    fn add_instruction(&mut self, op: Opcode, pos: Pos) {
        self.current_chunk_mut().write(op, pos);
    }

    // skibop
    fn find_variable(&self, name: Token) -> Option<Variable> {
        // IF SCOPE IS FUNCTION AND SCOPE DEPTH DOESN'T MATCH THIS IS A CAPTURE
        trace!("{} find variable {}", name.pos, name.lexeme);

        for (i, ctx) in self.children.iter().rev().enumerate() {
            if let Some(local) = ctx.get_variable(name) {
                if i == 0 {
                    trace!("found in current context");
                    return Some(local);
                }

                // TODO problem: we declare a dummy variable with the name of a function inside its
                // body so we can access it for recursion, but when we call get_variable we end up
                // finding that dummy variable when we shouldn't. this litle hack isn't really
                // correct anyway, since the variable should be inserted into the context as a
                // Capture from the beginning.
                if ctx.is_local() && !matches!(local, Variable::Global { .. }) {
                    trace!("found in context {i}");
                    return Some(Variable::Capture {
                        name: name.lexeme.to_string(),
                    });
                }
            }
            trace!("not in {i}");
        }

        self.global_identifiers.get(name.lexeme).cloned()
    }

    fn add_get_variable(
        &mut self,
        interner: &mut Interner,
        variable: Token,
    ) -> Result<(), PiccoloError> {
        interner.allocate_str(variable.lexeme);

        match self.find_variable(variable) {
            Some(Variable::Local { slot, .. }) => {
                self.add_instruction(Opcode::GetLocal(slot), variable.pos);
            }

            Some(Variable::Global { .. }) => {
                let global = self.get_global_ident(variable)?;
                self.add_instruction(Opcode::GetGlobal(global), variable.pos);
            }

            Some(Variable::Capture { .. }) => {
                return Err(
                    PiccoloError::todo(format!("get closed over {}", variable.lexeme))
                        .pos(variable.pos),
                )
            }

            None => {
                return Err(make_error!(UndefinedVariable {
                    name: variable.lexeme.to_string(),
                })
                .pos(variable.pos))
            }
        }

        Ok(())
    }

    fn add_set_variable(&mut self, variable: Token) -> Result<(), PiccoloError> {
        match self.find_variable(variable) {
            Some(Variable::Local { slot, .. }) => {
                self.add_instruction(Opcode::SetLocal(slot), variable.pos);
            }

            Some(Variable::Global { .. }) => {
                let global = self.get_global_ident(variable)?;
                self.add_instruction(Opcode::SetGlobal(global), variable.pos);
            }

            Some(Variable::Capture { .. }) => {
                return Err(
                    PiccoloError::todo(format!("set closed over {}", variable.lexeme))
                        .pos(variable.pos),
                )
            }

            None => {
                return Err(make_error!(UndefinedVariable {
                    name: variable.lexeme.to_string(),
                })
                .pos(variable.pos))
            }
        }

        Ok(())
    }

    fn add_jump_back(&mut self, offset: usize, pos: Pos) {
        self.current_chunk_mut().write_jump_back(offset, pos);
    }

    fn add_constant(&mut self, value: Constant, pos: Pos) {
        let index = self.make_constant(value);
        self.current_chunk_mut().write(Opcode::Constant(index), pos);
    }

    fn make_constant(&mut self, c: Constant) -> u16 {
        if let Some((idx, _)) = self
            .module()
            .constants()
            .iter()
            .enumerate()
            .find(|(_, constant)| **constant == c)
        {
            idx.try_into()
                .expect("number of constants should fit into u16")
        } else {
            self.module_mut().make_constant(c)
        }
    }

    pub(crate) fn make_global_ident(&mut self, interner: &mut Interner, name: Token) -> u16 {
        trace!("{} make global {}", name.pos, name.lexeme);

        if self.global_identifiers.contains_key(name.lexeme) {
            match self.global_identifiers[name.lexeme] {
                Variable::Global { index, .. } => index,
                _ => unreachable!(),
            }
        } else {
            interner.allocate_str(name.lexeme);
            let index = self.make_constant(Constant::StringPtr(interner.allocate_str(name.lexeme)));
            self.global_identifiers.insert(
                name.lexeme.to_owned(),
                Variable::Global {
                    name: name.lexeme.to_string(),
                    index,
                },
            );
            index
        }
    }

    fn get_global_ident(&self, name: Token) -> Result<u16, PiccoloError> {
        trace!("{} get global {}", name.pos, name.lexeme);

        match self.global_identifiers.get(name.lexeme).ok_or_else(|| {
            make_error!(UndefinedVariable {
                name: name.lexeme.to_owned(),
            })
            .pos(name.pos)
        })? {
            Variable::Global { index, .. } => Ok(*index),
            _ => unreachable!(),
        }
    }

    fn make_variable(&mut self, interner: &mut Interner, name: Token) -> Result<(), PiccoloError> {
        trace!("{} make variable {}", name.pos, name.lexeme);

        // are we in global scope?
        if self.current_context().is_local() {
            interner.allocate_str(name.lexeme);

            // check if we have a local with this name
            if let Some(index) = self.current_context().get_local_depth(name) {
                // if we do,
                if index != self.current_context().scope_depth() {
                    trace!("{} new local in sub-scope {}", name.pos, name.lexeme);
                    // create a new local if we're in a different scope
                    self.current_context_mut().add_local(name);
                } else {
                    // error if we're in the same scope
                    return Err(make_error!(SyntaxError).pos(name.pos).msg_string(format!(
                        "variable with name '{}' already exists",
                        name.lexeme,
                    )));
                }
            } else {
                trace!("{} new local {}", name.pos, name.lexeme);
                // if we don't, create a new local with this name
                self.current_context_mut().add_local(name);
            }
        } else {
            trace!("{} new global {}", name.pos, name.lexeme);
            // yes, make a global
            let index = self.make_global_ident(interner, name);
            self.add_instruction(Opcode::DeclareGlobal(index), name.pos);
        }

        Ok(())
    }

    fn begin_context(&mut self) {
        let index = self.module_mut().add_chunk();
        trace!("begin context {}", index);
        self.children.push(EmitterContext::new(index));
    }

    fn end_context(&mut self) -> usize {
        let context = self.children.pop().unwrap();
        trace!("end context {}", self.children.len());
        context.chunk_index
    }

    fn begin_scope(&mut self) {
        self.current_context_mut().begin_scope();
    }

    fn end_scope(&mut self, pos: Pos) -> Vec<Variable> {
        let locals = self.current_context_mut().end_scope();
        for _ in locals.iter() {
            self.add_instruction(Opcode::Pop, pos);
        }
        locals
    }

    fn start_jump(&mut self, op: Opcode, pos: Pos) -> usize {
        self.current_chunk_mut().start_jump(op, pos)
    }

    fn patch_jump(&mut self, offset: usize) {
        self.current_chunk_mut().patch_jump(offset);
    }

    fn start_loop_jumps(&mut self) -> usize {
        self.continue_offsets.push(Vec::new());
        self.break_offsets.push(Vec::new());
        self.current_chunk().ops.len()
    }

    fn patch_continue_jumps(&mut self) {
        for offset in self.continue_offsets.pop().unwrap() {
            self.patch_jump(offset);
        }
    }

    fn patch_break_jumps(&mut self) {
        for offset in self.break_offsets.pop().unwrap() {
            self.patch_jump(offset);
        }
    }

    fn add_break(&mut self, offset: usize, break_: Token) -> Result<(), PiccoloError> {
        self.break_offsets
            .last_mut()
            .ok_or_else(|| {
                make_error!(SyntaxError)
                    .msg("cannot break outside of a loop")
                    .pos(break_.pos)
            })?
            .push(offset);

        Ok(())
    }

    fn add_continue(&mut self, offset: usize, continue_: Token) -> Result<(), PiccoloError> {
        self.continue_offsets
            .last_mut()
            .ok_or_else(|| {
                make_error!(SyntaxError)
                    .msg("cannot continue outside of a loop")
                    .pos(continue_.pos)
            })?
            .push(offset);

        Ok(())
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Emitter::new()
    }
}

#[cfg(test)]
mod test {
    use crate::Environment;

    #[test]
    fn emitter() {
        use crate::{
            compiler::{emitter, parser},
            runtime::{chunk, interner::Interner, memory::Heap, vm::Machine, ContextMut},
        };

        let ast = parser::parse(
            "x =: 32\n\
            return x",
        )
        .unwrap();

        let mut interner = Interner::new();
        let module = emitter::compile(&mut interner, &ast).unwrap();
        println!("{}", chunk::disassemble(&interner, &module, "jioew"));
        let mut heap = Heap::new();
        let mut interner = Interner::new();
        let mut ctx = ContextMut {
            heap: &mut heap,
            interner: &mut interner,
        };
        Machine::new().interpret(&mut ctx, &module).unwrap();
    }

    #[test]
    fn reentrant() {
        use crate::compiler::parser::parse;

        let ast1 = parse("x=:3").unwrap();
        let ast2 = parse("assert x == 3").unwrap();
        let ast3 = parse("fn z(a) do\n  print(\"a is\", a)\n  end\n").unwrap();
        let ast4 = parse("z(x)").unwrap();

        let mut env = Environment::new();
        env.compile(&ast1).unwrap();
        println!("{}", env.disassemble(""));
        env.compile(&ast2).unwrap();
        println!("{}", env.disassemble(""));
        env.compile(&ast3).unwrap();
        println!("{}", env.disassemble(""));
        env.compile(&ast4).unwrap();
        println!("{}", env.disassemble(""));
    }
}
