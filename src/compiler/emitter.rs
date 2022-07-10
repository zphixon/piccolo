//! Bytecode compiler.
#![allow(clippy::too_many_arguments)]

use crate::{
    compiler::{
        ast::{Ast, Expr, Stmt},
        Local, SourcePos, Token, TokenKind, MAX_DEPTH,
    },
    error::{ErrorKind, PiccoloError},
    runtime::{
        chunk::{Chunk, Module},
        op::Opcode,
        value::{Constant, ConstantFunction},
        Arity,
    },
    trace,
};
use fnv::FnvHashMap;

macro_rules! check_depth {
    ($depth:tt, $pos_haver:expr) => {
        if $depth > MAX_DEPTH {
            return Err(PiccoloError::new(ErrorKind::SyntaxError)
                .msg("Maximum recursion depth reached")
                .pos($pos_haver.pos));
        }
    };
}

// top level compile into chunk index 0
// encounter a function, start compiling into a new chunk index
// after we end the function collect upvalues

pub fn compile(ast: &Ast) -> Result<Module, Vec<PiccoloError>> {
    let mut emitter = Emitter::new();
    compile_with(&mut emitter, ast)?;
    Ok(emitter.into_module())
}

pub fn compile_with(emitter: &mut Emitter, ast: &Ast) -> Result<(), Vec<PiccoloError>> {
    let errors: Vec<_> = ast
        .iter()
        .map(|stmt| compile_stmt(emitter, 0, stmt))
        .filter_map(Result::err)
        .collect();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[rustfmt::skip]
fn compile_stmt(emitter: &mut Emitter, depth: usize, stmt: &Stmt) -> Result<(), PiccoloError> {
    check_depth!(depth, stmt.token());

    match stmt {
        Stmt::Expr { token, expr }
            => compile_expr_stmt(emitter, depth + 1, *token, expr),
        Stmt::Block { end, body }
            => compile_block(emitter, depth + 1, *end, body),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(emitter, depth + 1, *name, value),
        Stmt::Assignment { lval, op, rval }
            => compile_assignment(emitter, depth + 1, lval, *op, rval),
        Stmt::If { if_, cond, then_block, else_, else_block, end }
            => compile_if(emitter, depth + 1, *if_, cond, then_block, else_.as_ref(), else_block.as_ref(), *end),
        Stmt::While { while_, cond, body, end }
            => compile_while(emitter, depth + 1, *while_, cond, body, *end),
        Stmt::For { for_, init, cond, name, inc_op, inc_expr, body, end }
            => compile_for(emitter, depth + 1, *for_, init.as_ref(), cond, *name, *inc_op, inc_expr, body, *end),
        Stmt::ForEach { for_, item, iter, body, end }
            => compile_for_each(emitter, depth + 1, *for_, *item, *iter, body, *end),
        Stmt::Fn { name, args, arity, body, method, end }
            => compile_fn(emitter, depth + 1, *name, args, *arity, body, *method, *end),
        Stmt::Break { break_ }
            => compile_break(emitter, depth + 1, *break_),
        Stmt::Continue { continue_ }
            => compile_continue(emitter, depth + 1, *continue_),
        Stmt::Retn { retn, value }
            => compile_retn(emitter, depth + 1, *retn, value.as_ref()),
        Stmt::Assert { assert, value }
            => compile_assert(emitter, depth + 1, *assert, value),
        Stmt::Data { name, methods, fields }
            => compile_data(emitter, depth + 1, *name, methods, fields),
    }
}

#[rustfmt::skip]
fn compile_expr(emitter: &mut Emitter, depth: usize, expr: &Expr) -> Result<(), PiccoloError> {
    check_depth!(depth, expr.token());

    match expr {
        Expr::Literal { literal }
            => compile_literal(emitter, depth + 1, *literal),
        Expr::ArrayLiteral { right_bracket, values }
            => compile_array_literal(emitter, depth + 1, *right_bracket, values),
        Expr::Paren { right_paren, expr }
            => compile_paren(emitter, depth + 1, *right_paren, expr),
        Expr::Variable { variable }
            => compile_variable(emitter, depth + 1, *variable),
        Expr::Unary { op, rhs }
            => compile_unary(emitter, depth + 1, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => compile_binary(emitter, depth + 1, lhs, *op, rhs),
        Expr::Logical { lhs, op, rhs }
            => compile_logical(emitter, depth + 1, lhs, *op, rhs),
        Expr::Call { callee, paren, arity, args }
            => compile_call(emitter, depth + 1, callee, *paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(emitter, depth + 1, name, args),
        Expr::Get { object, name }
            => compile_get(emitter, depth + 1, object, *name),
        // Expr::Set { object, name, value }
        //     => compile_set(emitter, depth + 1, object, name, value),
        Expr::Index { right_bracket, object, index }
            => compile_index(emitter, depth + 1, *right_bracket, object, index),
        Expr::Fn { fn_, args, arity, body, end }
            => compile_lambda(emitter, depth + 1, *fn_, args, *arity, body, *end),
        _ => Err(PiccoloError::todo(format!("compile_expr: {expr:#?}"))),
    }
}

fn compile_expr_stmt(
    emitter: &mut Emitter,
    depth: usize,
    token: Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} expr stmt", token.pos);
    check_depth!(depth, token);

    compile_expr(emitter, depth + 1, expr)?;
    emitter.add_instruction(Opcode::Pop, token.pos);
    Ok(())
}

fn compile_block(
    emitter: &mut Emitter,
    depth: usize,
    end: Token,
    body: &[Stmt],
) -> Result<(), PiccoloError> {
    trace!("{} block", end.pos);
    check_depth!(depth, end);

    emitter.begin_scope();
    for stmt in body {
        compile_stmt(emitter, depth + 1, stmt)?;
    }
    emitter.end_scope(end.pos);
    Ok(())
}

fn compile_declaration(
    emitter: &mut Emitter,
    depth: usize,
    name: Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} decl {}", name.pos, name.lexeme);
    check_depth!(depth, name);

    compile_expr(emitter, depth + 1, value)?;
    emitter.make_variable(name)
}

fn compile_assignment(
    emitter: &mut Emitter,
    depth: usize,
    lval: &Expr,
    op: Token,
    rval: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} assign {lval:?} {}, {rval:?}", op.pos, op.lexeme);
    check_depth!(depth, lval.token());

    if let Expr::Variable { variable: name } = lval {
        let name = *name;
        if let Some(opcode) = op.assign_by_mutate_op() {
            // if this is an assignment-by-mutation operator, first get the value of the variable
            if let Some(index) = emitter.current_context().get_local_slot(name) {
                emitter.add_instruction(Opcode::GetLocal(index), op.pos);
            } else {
                let index = emitter.get_global_ident(name)?;
                emitter.add_instruction(Opcode::GetGlobal(index), op.pos);
            }

            // calculate the value to mutate with
            compile_expr(emitter, depth + 1, rval)?;

            // then mutate
            emitter.add_instruction(opcode, op.pos);
        } else {
            // otherwise just calculate the value
            compile_expr(emitter, depth + 1, rval)?;
        }

        // then assign
        if emitter.current_context().is_local() {
            if let Some(index) = emitter.current_context().get_local_slot(name) {
                emitter.add_instruction(Opcode::SetLocal(index), op.pos);
            } else {
                let index = emitter.get_global_ident(name)?;
                emitter.add_instruction(Opcode::SetGlobal(index), op.pos);
            }
        } else {
            let index = emitter.get_global_ident(name)?;
            emitter.add_instruction(Opcode::SetGlobal(index), op.pos);
        }
    } else if let Expr::Get { object, name } = lval {
        compile_expr(emitter, depth + 1, rval)?;
        compile_expr(emitter, depth + 1, object)?;
        emitter.add_constant(Constant::String(name.lexeme.to_string()), name.pos);
        emitter.add_instruction(Opcode::Set, op.pos);
    } else if let Expr::Index { object, index, .. } = lval {
        compile_expr(emitter, depth + 1, rval)?;
        compile_expr(emitter, depth + 1, object)?;
        compile_expr(emitter, depth + 1, index)?;
        emitter.add_instruction(Opcode::Set, op.pos);
    } else {
        return Err(PiccoloError::new(ErrorKind::SyntaxError)
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
    depth: usize,
    if_: Token,
    cond: &Expr,
    then_block: &[Stmt],
    else_: Option<&Token>,
    else_block: Option<&Vec<Stmt>>,
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} if", if_.pos);
    check_depth!(depth, if_);

    // compile the condition
    compile_expr(emitter, depth + 1, cond)?;

    if let (Some(else_), Some(else_block)) = (else_, else_block) {
        // if the condition is false, jump to patch_jump(jump_else)
        let jump_else = emitter.start_jump(Opcode::JumpFalse(0), if_.pos);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.pos);
        // compile the then block
        compile_block(emitter, depth + 1, end, then_block)?;
        // jump unconditionally past the else block to patch_jump(end_jump)
        let end_jump = emitter.start_jump(Opcode::JumpForward(0), else_.pos);

        // jump here if the condition is false
        emitter.patch_jump(jump_else);
        // pop the condition
        emitter.add_instruction(Opcode::Pop, else_.pos);
        // compile the else block
        compile_block(emitter, depth + 1, *else_, else_block)?;

        emitter.patch_jump(end_jump);
    } else {
        // there is no else block, jump to patch_jump(jump_end) if false
        let jump_end = emitter.start_jump(Opcode::JumpFalse(0), if_.pos);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.pos);

        // compile then block
        compile_block(emitter, depth + 1, end, then_block)?;

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
    depth: usize,
    while_: Token,
    cond: &Expr,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} while", while_.pos);
    check_depth!(depth, while_);

    // loop condition
    let loop_start = emitter.start_loop_jumps();
    compile_expr(emitter, depth + 1, cond)?;

    // jump to the end if the condition was false, pop if true
    let exit_jump = emitter.start_jump(Opcode::JumpFalse(0), while_.pos);
    emitter.add_instruction(Opcode::Pop, while_.pos);

    // loop body
    compile_block(emitter, depth + 1, end, body)?;

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
    depth: usize,
    for_: Token,
    init: &Stmt,
    cond: &Expr,
    name: Token,
    inc_op: Token,
    inc_expr: &Expr,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} for", for_.pos);
    check_depth!(depth, for_);

    // initializer
    emitter.begin_scope();
    compile_stmt(emitter, depth + 1, init)?;

    // condition
    let start_offset = emitter.start_loop_jumps();
    compile_expr(emitter, depth + 1, cond)?;

    // if false jump to the end
    let end_jump = emitter.start_jump(Opcode::JumpFalse(0), for_.pos);
    emitter.add_instruction(Opcode::Pop, for_.pos);

    // loop body
    compile_block(emitter, depth + 1, end, body)?;

    // here if we encounter a continue
    emitter.patch_continue_jumps();

    // increment
    compile_assignment(
        emitter,
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
    depth: usize,
    for_: Token,
    item: Token,
    iter: Token,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} for_each", for_.pos);
    check_depth!(depth, for_);

    let index_name = format!("idx_of_{}_in_{}", item.lexeme, iter.lexeme);
    let index = Token::new(TokenKind::Identifier, &index_name, item.pos);

    emitter.begin_scope();
    emitter.add_constant(Constant::Integer(0), for_.pos);
    emitter.make_variable(index)?;

    let start = emitter.start_loop_jumps();

    emitter.add_instruction(
        Opcode::GetLocal(emitter.current_context().get_local_slot(index).unwrap()),
        for_.pos,
    );
    compile_variable(emitter, depth, iter)?;
    emitter.add_constant(Constant::String(String::from("len")), iter.pos);
    emitter.add_instruction(Opcode::Get, for_.pos);
    emitter.add_instruction(Opcode::Less, for_.pos);

    let end_jump = emitter.start_jump(Opcode::JumpFalse(0), for_.pos);
    emitter.add_instruction(Opcode::Pop, for_.pos);

    emitter.begin_scope();
    compile_declaration(
        emitter,
        depth + 1,
        item,
        &Expr::Index {
            right_bracket: Token::new(TokenKind::RightBracket, "[", item.pos),
            object: Box::new(Expr::Variable { variable: iter }),
            index: Box::new(Expr::Variable { variable: index }),
        },
    )?;
    compile_block(emitter, depth + 1, end, body)?;
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
    depth: usize,
    name: Token,
    args: &[Token],
    arity: usize,
    body: &[Stmt],
    _method: bool,
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} fn {}", name.pos, name.lexeme);
    check_depth!(depth, name);

    //if let Some(_) = emitter.current_context().get_local_slot(name) {
    //    return Err(PiccoloError::new(ErrorKind::SyntaxError)
    //        .pos(name.pos)
    //        .msg_string(format!(
    //            "Function/variable with name '{}' already exists",
    //            name.lexeme
    //        )));
    //} else if emitter.current_context().is_local() {
    //    emitter.make_variable(name)?;
    //} else if !emitter.current_context().is_local() {
    //    emitter.make_global_ident(name);
    //}

    emitter.begin_context();
    emitter.begin_scope();

    // will always be local
    emitter.make_variable(name)?;
    for arg in args {
        emitter.make_variable(*arg)?;
    }

    // don't use compile_block because we've already started a new scope
    for stmt in body {
        compile_stmt(emitter, depth + 1, stmt)?;
    }

    emitter.add_instruction(Opcode::Nil, end.pos);
    emitter.add_instruction(Opcode::Return, end.pos);

    let chunk_index = emitter.end_context();

    let function = ConstantFunction {
        arity: Arity::Exact(arity),
        name: name.lexeme.to_string(),
        chunk: chunk_index,
    };

    let constant = emitter.make_constant(Constant::Function(function));
    emitter.add_instruction(Opcode::Constant(constant), name.pos);

    //if !emitter.current_context().is_local() {
    // make_variable doesn't check if a global exists before declaring it
    emitter.make_variable(name)?;
    //}

    Ok(())
}

fn compile_break(emitter: &mut Emitter, depth: usize, break_: Token) -> Result<(), PiccoloError> {
    trace!("{} break", break_.pos);
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
    trace!("{} continue", continue_.pos);
    check_depth!(depth, continue_);

    let offset = emitter.start_jump(Opcode::JumpForward(0), continue_.pos);
    emitter.add_continue(offset, continue_)?;

    Ok(())
}

fn compile_retn(
    emitter: &mut Emitter,
    depth: usize,
    retn: Token,
    expr: Option<&Expr>,
) -> Result<(), PiccoloError> {
    trace!("{} retn", retn.pos);
    check_depth!(depth, retn);

    if let Some(expr) = expr {
        compile_expr(emitter, depth + 1, expr)?;
    } else {
        emitter.add_instruction(Opcode::Nil, retn.pos);
    }

    emitter.add_instruction(Opcode::Return, retn.pos);

    Ok(())
}

fn compile_assert(
    emitter: &mut Emitter,
    depth: usize,
    assert: Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} assert", assert.pos);
    check_depth!(depth, assert);

    compile_expr(emitter, depth + 1, value)?;

    let c = emitter.make_constant(Constant::String(super::ast::print_expression(value)));
    emitter.add_instruction(Opcode::Assert(c), assert.pos);

    Ok(())
}

fn compile_data(
    _emitter: &mut Emitter,
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
    depth: usize,
    literal: Token,
) -> Result<(), PiccoloError> {
    trace!("{} literal", literal.pos);
    check_depth!(depth, literal);

    match literal.kind {
        TokenKind::True => emitter.add_instruction(Opcode::Bool(true), literal.pos),
        TokenKind::False => emitter.add_instruction(Opcode::Bool(false), literal.pos),
        TokenKind::Integer(v) if u16::try_from(v).is_ok() => {
            emitter.add_instruction(Opcode::Integer(v.try_into().unwrap()), literal.pos)
        }
        TokenKind::Nil => emitter.add_instruction(Opcode::Nil, literal.pos),
        _ => emitter.add_constant(Constant::try_from(literal)?, literal.pos),
    }

    Ok(())
}

fn compile_array_literal(
    emitter: &mut Emitter,
    depth: usize,
    right_bracket: Token,
    values: &[Expr],
) -> Result<(), PiccoloError> {
    check_depth!(depth, right_bracket);

    for value in values.iter() {
        compile_expr(emitter, depth + 1, value)?;
    }

    emitter.add_instruction(
        Opcode::Array(values.len().try_into().unwrap()),
        right_bracket.pos,
    );

    Ok(())
}

fn compile_paren(
    emitter: &mut Emitter,
    depth: usize,
    right_paren: Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} paren", right_paren.pos);
    check_depth!(depth, right_paren);

    compile_expr(emitter, depth + 1, expr)
        .map_err(|e| e.msg_string(format!("in expression starting on pos {}", right_paren.pos)))
}

fn compile_variable(
    emitter: &mut Emitter,
    depth: usize,
    variable: Token,
) -> Result<(), PiccoloError> {
    trace!("{} variable {}", variable.pos, variable.lexeme);
    check_depth!(depth, variable);

    if let Some(local) = emitter.current_context().get_local_slot(variable) {
        emitter.add_instruction(Opcode::GetLocal(local), variable.pos);
    } else {
        let global = emitter.get_global_ident(variable)?;
        emitter.add_instruction(Opcode::GetGlobal(global), variable.pos);
    }
    Ok(())
}

fn compile_unary(
    emitter: &mut Emitter,
    depth: usize,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} unary {}", op.pos, op.lexeme);
    check_depth!(depth, op);

    compile_expr(emitter, depth + 1, rhs)?;

    match op.kind {
        TokenKind::Minus => emitter.add_instruction(Opcode::Negate, op.pos),
        TokenKind::Not => emitter.add_instruction(Opcode::Not, op.pos),
        _ => unreachable!("unary {:?}", op),
    };

    Ok(())
}

fn compile_binary(
    emitter: &mut Emitter,
    depth: usize,
    lhs: &Expr,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} binary {}", op.pos, op.lexeme);
    check_depth!(depth, lhs.token());

    compile_expr(emitter, depth + 1, lhs)?;
    compile_expr(emitter, depth + 1, rhs)?;

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
    depth: usize,
    lhs: &Expr,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} logical {}", op.pos, op.lexeme);
    check_depth!(depth, lhs.token());

    let jump_op = match op.kind {
        TokenKind::LogicalAnd => Opcode::JumpFalse(0),
        TokenKind::LogicalOr => Opcode::JumpTrue(0),
        _ => unreachable!("op {:?} for logical", op),
    };

    compile_expr(emitter, depth + 1, lhs)?;

    let short = emitter.start_jump(jump_op, op.pos);
    emitter.add_instruction(Opcode::Pop, op.pos);

    compile_expr(emitter, depth + 1, rhs)?;

    emitter.patch_jump(short);

    Ok(())
}

fn compile_call(
    emitter: &mut Emitter,
    depth: usize,
    callee: &Expr,
    paren: Token,
    arity: usize,
    args: &[Expr],
) -> Result<(), PiccoloError> {
    trace!("{} call", paren.pos);
    check_depth!(depth, callee.token());

    compile_expr(emitter, depth + 1, callee)?;
    for arg in args {
        compile_expr(emitter, depth + 1, arg)?;
    }
    emitter.add_instruction(Opcode::Call(arity as u16), paren.pos);
    Ok(())
}

fn compile_get(
    emitter: &mut Emitter,
    depth: usize,
    object: &Expr,
    name: Token,
) -> Result<(), PiccoloError> {
    check_depth!(depth, object.token());

    compile_expr(emitter, depth + 1, object)?;
    emitter.add_constant(Constant::String(name.lexeme.to_string()), name.pos);
    emitter.add_instruction(Opcode::Get, name.pos);
    Ok(())
}

fn compile_index(
    emitter: &mut Emitter,
    depth: usize,
    right_bracket: Token,
    object: &Expr,
    index: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} index", right_bracket.pos);
    check_depth!(depth, right_bracket);

    compile_expr(emitter, depth + 1, object)?;
    compile_expr(emitter, depth + 1, index)?;
    emitter.add_instruction(Opcode::Get, right_bracket.pos);
    Ok(())
}

fn compile_lambda(
    emitter: &mut Emitter,
    depth: usize,
    fn_: Token,
    args: &[Token],
    arity: usize,
    body: &[Stmt],
    end: Token,
) -> Result<(), PiccoloError> {
    trace!("{} lambda", fn_.pos);
    check_depth!(depth, fn_);

    emitter.begin_context();
    emitter.begin_scope();

    // ??? since recursion is a hack we need a fake local so that the slots are all correct
    emitter
        .current_context_mut()
        .add_local(Token::new(TokenKind::Fn, "", fn_.pos));
    for arg in args {
        emitter.make_variable(*arg)?;
    }

    for stmt in body {
        // don't use compile_block because we've already started a new scope
        compile_stmt(emitter, depth + 1, stmt)?;
    }

    emitter.add_instruction(Opcode::Nil, end.pos);
    emitter.add_instruction(Opcode::Return, end.pos);

    let chunk_index = emitter.end_context();

    let function = ConstantFunction {
        arity: Arity::Exact(arity),
        name: String::from("<anon>"),
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
    locals: Vec<Local>,
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

    #[allow(dead_code)]
    fn get_local(&self, index: u16) -> &Local {
        &self.locals[index as usize]
    }

    fn add_local(&mut self, name: Token) {
        self.locals
            .push(Local::new(name.lexeme.to_owned(), self.scope_depth()));
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

    fn end_scope(&mut self) -> Vec<Local> {
        self.depth -= 1;
        let index = self
            .locals
            .iter()
            .enumerate()
            .find_map(|(i, l)| if l.depth > self.depth { Some(i) } else { None })
            .unwrap_or(self.locals.len());
        self.locals.split_off(index)
    }

    fn get_local_slot(&self, name: Token) -> Option<u16> {
        // IF SCOPE IS FUNCTION AND SCOPE DEPTH DOESN'T MATCH THIS IS A CAPTURE
        // mark as captured
        trace!("{} get local slot {}", name.pos, name.lexeme);
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name.lexeme {
                return Some(i as u16);
            }
        }

        None
    }

    fn get_local_depth(&self, name: Token) -> Option<u16> {
        trace!("{} get local depth {}", name.pos, name.lexeme);
        for local in self.locals.iter().rev() {
            if local.name == name.lexeme {
                return Some(local.depth);
            }
        }

        None
    }
}

/// Bytecode compiler object
///
/// Construct an Emitter, and pass a `&mut` reference to `compile_ast` along with
/// the abstract syntax tree. Extract the `Chunk` with `current_chunk{_mut}()` or
/// `into_chunk()`.
#[derive(Debug)]
pub struct Emitter {
    module: Module,
    children: Vec<EmitterContext>,
    //errors: Vec<PiccoloError>,
    global_identifiers: FnvHashMap<String, u16>,
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

    fn add_instruction(&mut self, op: Opcode, pos: SourcePos) {
        self.current_chunk_mut().write(op, pos);
    }

    fn add_jump_back(&mut self, offset: usize, pos: SourcePos) {
        self.current_chunk_mut().write_jump_back(offset, pos);
    }

    fn add_constant(&mut self, value: Constant, pos: SourcePos) {
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

    pub(crate) fn make_global_ident(&mut self, name: Token) -> u16 {
        trace!("{} make global {}", name.pos, name.lexeme);

        if self.global_identifiers.contains_key(name.lexeme) {
            self.global_identifiers[name.lexeme]
        } else {
            let index = self.make_constant(Constant::String(name.lexeme.to_owned()));
            self.global_identifiers
                .insert(name.lexeme.to_owned(), index);
            index
        }
    }

    fn get_global_ident(&self, name: Token) -> Result<u16, PiccoloError> {
        trace!("{} get global {}", name.pos, name.lexeme);

        self.global_identifiers
            .get(name.lexeme)
            .copied()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::UndefinedVariable {
                    name: name.lexeme.to_owned(),
                })
                .pos(name.pos)
            })
    }

    fn make_variable(&mut self, name: Token) -> Result<(), PiccoloError> {
        trace!("{} make variable {}", name.pos, name.lexeme);

        // are we in global scope?
        if self.current_context().is_local() {
            // check if we have a local with this name
            if let Some(index) = self.current_context().get_local_depth(name) {
                // if we do,
                if index != self.current_context().scope_depth() {
                    trace!("{} new local in sub-scope {}", name.pos, name.lexeme);
                    // create a new local if we're in a different scope
                    self.current_context_mut().add_local(name);
                } else {
                    // error if we're in the same scope
                    return Err(PiccoloError::new(ErrorKind::SyntaxError)
                        .pos(name.pos)
                        .msg_string(format!(
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
            let index = self.make_global_ident(name);
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

    fn end_scope(&mut self, pos: SourcePos) {
        for _ in self.current_context_mut().end_scope() {
            self.add_instruction(Opcode::Pop, pos);
        }
    }

    fn start_jump(&mut self, op: Opcode, pos: SourcePos) -> usize {
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
                PiccoloError::new(ErrorKind::SyntaxError)
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
                PiccoloError::new(ErrorKind::SyntaxError)
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
    #[test]
    fn emitter() {
        use crate::{
            compiler::{emitter, parser},
            runtime::{chunk, memory::Heap, vm::Machine},
        };

        let ast = parser::parse(
            "x =: 32\n\
            retn x",
        )
        .unwrap();

        let module = emitter::compile(&ast).unwrap();
        println!("{}", chunk::disassemble(&module, "jioew"));
        let mut heap = Heap::new();
        Machine::new().interpret(&mut heap, &module).unwrap();
    }

    #[test]
    fn reentrant() {
        use crate::{
            compiler::{emitter, parser},
            runtime::chunk,
        };

        let ast1 = parser::parse("x=:3").unwrap();
        let ast2 = parser::parse("assert x == 3").unwrap();
        let ast3 = parser::parse("fn z(a) do\n  print(\"a is\", a)\n  end\n").unwrap();
        let ast4 = parser::parse("z(x)").unwrap();

        let (mut emitter, _, _) = crate::make_environment();
        emitter::compile_with(&mut emitter, &ast1).unwrap();
        println!("{}", chunk::disassemble(emitter.module(), ""));
        emitter::compile_with(&mut emitter, &ast2).unwrap();
        println!("{}", chunk::disassemble(emitter.module(), ""));
        emitter::compile_with(&mut emitter, &ast3).unwrap();
        println!("{}", chunk::disassemble(emitter.module(), ""));
        emitter::compile_with(&mut emitter, &ast4).unwrap();
        println!("{}", chunk::disassemble(emitter.module(), ""));

        println!("{:?}", emitter);
    }
}
