use super::ast::{Expr, Stmt};
use super::Local;
use crate::compiler::{Token, TokenKind};
use crate::error::{ErrorKind, PiccoloError};
use crate::runtime::chunk::Chunk;
use crate::runtime::op::Opcode;
use crate::runtime::value::Constant;

use fnv::FnvHashMap;

type Ast<'a> = [Stmt<'a>];

pub fn compile_ast(emitter: &mut Emitter, ast: &Ast) -> Result<(), Vec<PiccoloError>> {
    let errors: Vec<_> = ast
        .iter()
        .map(|stmt| compile_stmt(emitter, stmt))
        .filter_map(Result::err)
        .collect();

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

#[rustfmt::skip]
fn compile_stmt(emitter: &mut Emitter, stmt: &Stmt) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { token, expr }
            => compile_expr_stmt(emitter, token, expr),
        Stmt::Block { end, body }
            => compile_block(emitter, end, body),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(emitter, name, value),
        Stmt::Assignment { name, op, value }
            => compile_assignment(emitter, name, op, value),
        Stmt::If { if_, cond, then_block, else_, else_block, end }
            => compile_if(emitter, if_, cond, then_block, else_.as_ref(), else_block.as_ref(), end),
        Stmt::While { while_, cond, body, end }
            => compile_while(emitter, while_, cond, body, end),
        Stmt::For { for_, init, cond, inc, body, end }
            => compile_for(emitter, for_, init.as_ref(), cond, inc.as_ref(), body, end),
        // Stmt::Func { name, args, arity, body, method }
        //     => compile_func(emitter, name, args, *arity, body, *method),
        Stmt::Break { break_ }
            => compile_break(emitter, break_),
        Stmt::Continue { continue_ }
            => compile_continue(emitter, continue_),
        Stmt::Retn { retn, value }
            => compile_retn(emitter, retn, value.as_ref()),
        Stmt::Assert { assert, value }
            => compile_assert(emitter, assert, value),
        // Stmt::Data { name, methods, fields }
        //     => compile_data(emitter, name, methods, fields),
        _ => todo!("{:?}", stmt),
    }
}

#[rustfmt::skip]
fn compile_expr(emitter: &mut Emitter, expr: &Expr) -> Result<(), PiccoloError> {
    match expr {
        Expr::Literal { literal }
            => compile_literal(emitter, literal),
        Expr::Paren { right_paren, expr }
            => compile_paren(emitter, right_paren, expr),
        Expr::Variable { variable }
            => compile_variable(emitter, variable),
        Expr::Unary { op, rhs }
            => compile_unary(emitter, op, rhs),
        Expr::Binary { lhs, op, rhs }
            => compile_binary(emitter, lhs, op, rhs),
        Expr::Logical { lhs, op, rhs }
            => compile_logical(emitter, lhs, op, rhs),
        // Expr::Call { callee, paren, arity, args }
        //     => compile_call(emitter, callee, paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(emitter, name, args),
        // Expr::Get { object, name }
        //     => compile_get(emitter, object, name),
        // Expr::Set { object, name, value }
        //     => compile_set(emitter, object, name, value),
        // Expr::Index { right_bracket, object, idx }
        //     => compile_index(emitter, right_bracket, object, idx),
        // Expr::Func { name, args, arity, body, method }
        //     => compile_func(emitter, name, args, *arity, body, *method),
        _ => todo!("{:?}", expr),
    }
}

fn compile_expr_stmt(
    emitter: &mut Emitter,
    token: &Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, expr)?;
    emitter.add_instruction(Opcode::Pop, token.line);
    Ok(())
}

fn compile_block(emitter: &mut Emitter, end: &Token, body: &[Stmt]) -> Result<(), PiccoloError> {
    emitter.begin_scope();
    for stmt in body {
        compile_stmt(emitter, stmt)?;
    }
    emitter.end_scope(end.line);
    Ok(())
}

fn compile_declaration(
    emitter: &mut Emitter,
    name: &Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, value)?;
    emitter.make_variable(name)
}

fn compile_assignment(
    emitter: &mut Emitter,
    name: &Token,
    op: &Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, value)?;

    if emitter.is_local() {
        if let Some(idx) = emitter.get_local_slot(name) {
            emitter.add_instruction_arg(Opcode::SetLocal, idx, op.line);
        } else {
            let idx = emitter.get_global_ident(name)?;
            emitter.add_instruction_arg(Opcode::SetGlobal, idx, op.line);
        }
    } else {
        let idx = emitter.get_global_ident(name)?;
        emitter.add_instruction_arg(Opcode::SetGlobal, idx, op.line);
    }

    Ok(())
}

fn compile_if(
    emitter: &mut Emitter,
    if_: &Token,
    cond: &Expr,
    then_block: &[Stmt],
    else_: Option<&Token>,
    else_block: Option<&Vec<Stmt>>,
    end: &Token,
) -> Result<(), PiccoloError> {
    // compile the condition
    compile_expr(emitter, cond)?;

    if let (Some(else_), Some(else_block)) = (else_, else_block) {
        // if the condition is false, jump to patch_jump(jump_else)
        let jump_else = emitter.start_jump(Opcode::JumpFalse, if_.line);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.line);
        // compile the then block
        compile_block(emitter, end, then_block)?;
        // jump unconditionally past the else block to patch_jump(end_jump)
        let end_jump = emitter.start_jump(Opcode::JumpForward, else_.line);

        // jump here if the condition is false
        emitter.patch_jump(jump_else);
        // pop the condition
        emitter.add_instruction(Opcode::Pop, else_.line);
        // compile the else block
        compile_block(emitter, else_, else_block)?;

        emitter.patch_jump(end_jump);
    } else {
        // there is no else block, jump to patch_jump(jump_end) if false
        let jump_end = emitter.start_jump(Opcode::JumpFalse, if_.line);
        // pop the condition after skipping the jump instruction
        emitter.add_instruction(Opcode::Pop, if_.line);

        // compile then block
        compile_block(emitter, end, then_block)?;

        // jump over the condition pop if we jumped over the else block
        let jump_pop = emitter.start_jump(Opcode::JumpForward, end.line);
        emitter.patch_jump(jump_end);
        emitter.add_instruction(Opcode::Pop, end.line);
        emitter.patch_jump(jump_pop);
    }

    Ok(())
}

fn compile_while(
    emitter: &mut Emitter,
    while_: &Token,
    cond: &Expr,
    body: &[Stmt],
    end: &Token,
) -> Result<(), PiccoloError> {
    // loop condition
    let loop_start = emitter.start_loop_jumps();
    compile_expr(emitter, cond)?;

    // jump to the end if the condition was false, pop if true
    let exit_jump = emitter.start_jump(Opcode::JumpFalse, while_.line);
    emitter.add_instruction(Opcode::Pop, while_.line);

    // loop body
    compile_block(emitter, end, body)?;

    // here after the body if we encounter a continue
    emitter.patch_continue_jumps();

    // jump back to the loop condition
    emitter.add_jump_back(loop_start, end.line);

    // pop the condition after false
    emitter.patch_jump(exit_jump);
    emitter.add_instruction(Opcode::Pop, end.line);

    // here after the whole thing if we encounter a break
    emitter.patch_break_jumps();

    Ok(())
}

fn compile_for(
    emitter: &mut Emitter,
    for_: &Token,
    init: &Stmt,
    cond: &Expr,
    inc: &Stmt,
    body: &[Stmt],
    end: &Token,
) -> Result<(), PiccoloError> {
    // initializer
    emitter.begin_scope();
    compile_stmt(emitter, init)?;

    // condition
    let start_offset = emitter.start_loop_jumps();
    compile_expr(emitter, cond)?;

    // if false jump to the end
    let end_jump = emitter.start_jump(Opcode::JumpFalse, for_.line);
    emitter.add_instruction(Opcode::Pop, for_.line);

    // loop body
    compile_block(emitter, end, body)?;

    // here if we encounter a continue
    emitter.patch_continue_jumps();

    // increment
    compile_stmt(emitter, inc)?;

    // unconditional jump back to condition
    emitter.add_jump_back(start_offset, end.line);

    // here if condition is false
    emitter.patch_jump(end_jump);
    emitter.add_instruction(Opcode::Pop, end.line);

    // here if we encounter a break
    emitter.patch_break_jumps();

    // pop loop variable (local variable)
    emitter.end_scope(end.line);

    Ok(())
}

fn compile_break(emitter: &mut Emitter, break_: &Token) -> Result<(), PiccoloError> {
    let offset = emitter.start_jump(Opcode::JumpForward, break_.line);
    emitter.add_break(offset, break_)?;

    Ok(())
}

fn compile_continue(emitter: &mut Emitter, continue_: &Token) -> Result<(), PiccoloError> {
    let offset = emitter.start_jump(Opcode::JumpForward, continue_.line);
    emitter.add_continue(offset, continue_)?;

    Ok(())
}

fn compile_retn(
    emitter: &mut Emitter,
    retn: &Token,
    expr: Option<&Expr>,
) -> Result<(), PiccoloError> {
    if let Some(expr) = expr {
        compile_expr(emitter, expr)?;
    }

    emitter.add_instruction(Opcode::Return, retn.line);

    Ok(())
}

fn compile_assert(emitter: &mut Emitter, assert: &Token, value: &Expr) -> Result<(), PiccoloError> {
    compile_expr(emitter, value)?;
    emitter.add_instruction(Opcode::Assert, assert.line);
    Ok(())
}

fn compile_literal(emitter: &mut Emitter, literal: &Token) -> Result<(), PiccoloError> {
    // TODO: use constant ops instead of Constant ops
    emitter.add_constant(Constant::try_from(*literal)?, literal.line);
    Ok(())
}

fn compile_paren(
    emitter: &mut Emitter,
    right_paren: &Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, expr).map_err(|e| {
        e.msg_string(format!(
            "in expression starting on line {}",
            right_paren.line
        ))
    })
}

fn compile_variable(emitter: &mut Emitter, variable: &Token) -> Result<(), PiccoloError> {
    if let Some(local) = emitter.get_local_slot(variable) {
        emitter.add_instruction_arg(Opcode::GetLocal, local, variable.line);
    } else {
        let global = emitter.get_global_ident(&variable)?;
        emitter.add_instruction_arg(Opcode::GetGlobal, global, variable.line);
    }
    Ok(())
}

fn compile_unary(emitter: &mut Emitter, op: &Token, rhs: &Expr) -> Result<(), PiccoloError> {
    compile_expr(emitter, rhs)?;

    match op.kind {
        TokenKind::Minus => emitter.add_instruction(Opcode::Negate, op.line),
        TokenKind::Not => emitter.add_instruction(Opcode::Not, op.line),
        _ => unreachable!("unary {:?}", op),
    }

    Ok(())
}

fn compile_binary(
    emitter: &mut Emitter,
    lhs: &Expr,
    op: &Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, lhs)?;
    compile_expr(emitter, rhs)?;

    match op.kind {
        TokenKind::Plus => emitter.add_instruction(Opcode::Add, op.line),
        TokenKind::Minus => emitter.add_instruction(Opcode::Subtract, op.line),
        TokenKind::Divide => emitter.add_instruction(Opcode::Divide, op.line),
        TokenKind::Multiply => emitter.add_instruction(Opcode::Multiply, op.line),
        TokenKind::Equal => emitter.add_instruction(Opcode::Equal, op.line),
        TokenKind::NotEqual => {
            emitter.add_instruction(Opcode::Equal, op.line);
            emitter.add_instruction(Opcode::Not, op.line);
        }
        TokenKind::Greater => emitter.add_instruction(Opcode::Greater, op.line),
        TokenKind::GreaterEqual => emitter.add_instruction(Opcode::GreaterEqual, op.line),
        TokenKind::Less => emitter.add_instruction(Opcode::Less, op.line),
        TokenKind::LessEqual => emitter.add_instruction(Opcode::LessEqual, op.line),
        TokenKind::Modulo => emitter.add_instruction(Opcode::Modulo, op.line),
        _ => unreachable!("binary {:?}", op),
    }

    Ok(())
}

fn compile_logical(
    emitter: &mut Emitter,
    lhs: &Expr,
    op: &Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    let jump_op = match op.kind {
        TokenKind::LogicalAnd => Opcode::JumpFalse,
        TokenKind::LogicalOr => Opcode::JumpTrue,
        _ => unreachable!("op {:?} for logical", op),
    };

    compile_expr(emitter, lhs)?;

    let short = emitter.start_jump(jump_op, op.line);
    emitter.add_instruction(Opcode::Pop, op.line);

    compile_expr(emitter, rhs)?;

    emitter.patch_jump(short);

    Ok(())
}

pub struct Emitter {
    chunk: Chunk,
    locals: Vec<Local>,
    global_identifiers: FnvHashMap<String, u16>,
    scope_depth: u16,
    continue_offsets: Vec<Vec<usize>>,
    break_offsets: Vec<Vec<usize>>,
}

impl Default for Emitter {
    fn default() -> Emitter {
        Emitter::new()
    }
}

impl Emitter {
    pub fn new() -> Self {
        Self {
            chunk: Chunk::default(),
            locals: Vec::new(),
            global_identifiers: FnvHashMap::default(),
            scope_depth: 0,
            continue_offsets: Vec::with_capacity(0),
            break_offsets: Vec::with_capacity(0),
        }
    }

    fn is_local(&self) -> bool {
        self.scope_depth > 0
    }

    pub fn into_chunk(self) -> Chunk {
        self.chunk
    }

    pub fn current_chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn add_instruction(&mut self, op: Opcode, line: usize) {
        self.current_chunk_mut().write_u8(op, line);
    }

    fn add_instruction_arg(&mut self, op: Opcode, arg: u16, line: usize) {
        self.current_chunk_mut().write_arg_u16(op, arg, line);
    }

    fn add_jump_back(&mut self, offset: usize, line: usize) {
        self.current_chunk_mut().write_jump_back(offset, line);
    }

    fn add_constant(&mut self, value: Constant, line: usize) {
        let idx = self.current_chunk_mut().make_constant(value);
        self.current_chunk_mut()
            .write_arg_u16(Opcode::Constant, idx, line);
    }

    fn make_global_ident(&mut self, name: &Token) -> u16 {
        if self.global_identifiers.contains_key(name.lexeme) {
            self.global_identifiers[name.lexeme]
        } else {
            let idx = self
                .chunk
                .make_constant(Constant::String(name.lexeme.to_owned()));
            self.global_identifiers.insert(name.lexeme.to_owned(), idx);
            idx
        }
    }

    fn get_global_ident(&self, name: &Token) -> Result<u16, PiccoloError> {
        self.global_identifiers
            .get(name.lexeme)
            .copied()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::UndefinedVariable {
                    name: name.lexeme.to_owned(),
                })
                .line(name.line)
            })
    }

    fn get_local_slot(&self, name: &Token) -> Option<u16> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name.lexeme {
                return Some(i as u16);
            }
        }

        None
    }

    fn get_local_depth(&self, name: &Token) -> Option<u16> {
        for local in self.locals.iter().rev() {
            if local.name == name.lexeme {
                return Some(local.depth);
            }
        }

        None
    }

    fn make_variable(&mut self, name: &Token) -> Result<(), PiccoloError> {
        // are we in global scope?
        if self.scope_depth > 0 {
            // check if we have a local with this name
            if let Some(idx) = self.get_local_depth(name) {
                // if we do,
                if idx != self.scope_depth {
                    // create a new local if we're in a different scope
                    self.locals
                        .push(Local::new(name.lexeme.to_owned(), self.scope_depth));
                } else {
                    // error if we're in the same scope
                    return Err(PiccoloError::new(ErrorKind::SyntaxError)
                        .line(name.line)
                        .msg_string(format!(
                            "variable with name '{}' already exists",
                            self.locals[idx as usize - 1].name
                        )));
                }
            } else {
                // if we don't, create a new local with this name
                self.locals
                    .push(Local::new(name.lexeme.to_owned(), self.scope_depth));
            }
        } else {
            // yes, make a global
            let idx = self.make_global_ident(name);
            self.add_instruction_arg(Opcode::DeclareGlobal, idx, name.line);
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, line: usize) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth > self.scope_depth
        {
            self.add_instruction(Opcode::Pop, line);
            self.locals.pop().unwrap();
        }
    }

    fn start_jump(&mut self, op: Opcode, line: usize) -> usize {
        self.current_chunk_mut().start_jump(op, line)
    }

    fn patch_jump(&mut self, offset: usize) {
        self.current_chunk_mut().patch_jump(offset);
    }

    fn start_loop_jumps(&mut self) -> usize {
        self.continue_offsets.push(Vec::new());
        self.break_offsets.push(Vec::new());
        self.current_chunk().data.len()
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

    fn add_break(&mut self, offset: usize, break_: &Token) -> Result<(), PiccoloError> {
        self.break_offsets
            .last_mut()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::SyntaxError)
                    .msg("cannot break outside of a loop")
                    .line(break_.line)
            })?
            .push(offset);

        Ok(())
    }

    fn add_continue(&mut self, offset: usize, continue_: &Token) -> Result<(), PiccoloError> {
        self.continue_offsets
            .last_mut()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::SyntaxError)
                    .msg("cannot continue outside of a loop")
                    .line(continue_.line)
            })?
            .push(offset);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn emitter() {
        let ast = crate::parse(&mut crate::Scanner::new(
            "x =: 32\n\
             retn x",
        ))
        .unwrap();

        let mut e = Emitter::new();
        compile_ast(&mut e, &ast).unwrap();
        println!("{}", e.current_chunk().disassemble("jioew"));

        crate::runtime::vm::Machine::new()
            .interpret(e.current_chunk())
            .unwrap();
    }
}
