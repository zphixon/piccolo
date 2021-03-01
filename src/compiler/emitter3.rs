use super::ast::{Expr, Stmt};
use super::Local;
use crate::compiler::{Token, TokenKind};
use crate::error::{ErrorKind, PiccoloError};
use crate::runtime::chunk::Chunk;
use crate::runtime::op::Opcode;
use crate::runtime::value::Constant;

use fnv::FnvHashMap;

type Ast<'a> = Vec<Stmt<'a>>;

pub fn compile_ast(emitter: &mut Emitter3, ast: &Ast) -> Result<(), Vec<PiccoloError>> {
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
fn compile_stmt(emitter: &mut Emitter3, stmt: &Stmt) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { expr, .. }
            => compile_expr(emitter, expr),
        // Stmt::Block { end, body }
        //     => compile_block(end, body),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(emitter, name, value),
        // Stmt::Assignment { name, op, value }
        //     => compile_assignment(name, op, value),
        // Stmt::If { if_, cond, then_block, else_, else_block, end }
        //     => compile_if(if_, cond, then_block, else_.as_ref(), else_block.as_ref(), end),
        // Stmt::While { while_, cond, body, end }
        //     => compile_while(while_, cond, body, end),
        // Stmt::For { for_, init, cond, inc, body, end }
        //     => compile_for(for_, init.as_ref(), cond, inc.as_ref(), body, end),
        // Stmt::Func { name, args, arity, body, method }
        //     => compile_func(name, args, *arity, body, *method),
        // Stmt::Break { break_ }
        //     => compile_break(break_),
        // Stmt::Continue { continue_ }
        //     => compile_continue(continue_),
        Stmt::Retn { retn, value }
            => compile_retn(emitter, retn, value.as_ref()),
        // Stmt::Assert { assert, value }
        //     => compile_assert(assert, value),
        // Stmt::Data { name, methods, fields }
        //     => compile_data(name, methods, fields),
        _ => todo!("{:?}", stmt),
    }
}

#[rustfmt::skip]
fn compile_expr(emitter: &mut Emitter3, expr: &Expr) -> Result<(), PiccoloError> {
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
        // Expr::Logical { lhs, op, rhs }
        //     => compile_logical(lhs, op, rhs),
        // Expr::Call { callee, paren, arity, args }
        //     => compile_call(callee, paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(name, args),
        // Expr::Get { object, name }
        //     => compile_get(object, name),
        // Expr::Set { object, name, value }
        //     => compile_set(object, name, value),
        // Expr::Index { right_bracket, object, idx }
        //     => compile_index(right_bracket, object, idx),
        // Expr::Func { name, args, arity, body, method }
        //     => compile_func(name, args, *arity, body, *method),
        _ => todo!("{:?}", expr),
    }
}

fn compile_declaration(
    emitter: &mut Emitter3,
    name: &Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, value)?;
    emitter.make_variable(name)
}

fn compile_retn(
    emitter: &mut Emitter3,
    retn: &Token,
    expr: Option<&Expr>,
) -> Result<(), PiccoloError> {
    if let Some(expr) = expr {
        compile_expr(emitter, expr)?;
    }

    emitter.add_instruction(Opcode::Return, retn.line);

    Ok(())
}

fn compile_literal(emitter: &mut Emitter3, literal: &Token) -> Result<(), PiccoloError> {
    emitter.add_constant(Constant::try_from(*literal)?, literal.line);
    Ok(())
}

fn compile_paren(
    emitter: &mut Emitter3,
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

fn compile_variable(emitter: &mut Emitter3, variable: &Token) -> Result<(), PiccoloError> {
    if let Some(local) = emitter.get_local_slot(variable) {
        emitter.add_instruction_arg(Opcode::GetLocal, local, variable.line);
    } else {
        let global = emitter.get_global_ident(&variable)?;
        emitter.add_instruction_arg(Opcode::GetGlobal, global, variable.line);
    }
    Ok(())
}

fn compile_unary(emitter: &mut Emitter3, op: &Token, rhs: &Expr) -> Result<(), PiccoloError> {
    compile_expr(emitter, rhs)?;

    match op.kind {
        TokenKind::Minus => emitter.add_instruction(Opcode::Negate, op.line),
        TokenKind::Not => emitter.add_instruction(Opcode::Not, op.line),
        _ => unreachable!("unary {:?}", op),
    }

    Ok(())
}

fn compile_binary(
    emitter: &mut Emitter3,
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

pub struct Emitter3 {
    chunk: Chunk,
    locals: Vec<Local>,
    global_identifiers: FnvHashMap<String, u16>,
    scope_depth: u16,
}

impl Emitter3 {
    fn new() -> Self {
        Self {
            chunk: Chunk::default(),
            locals: Vec::new(),
            global_identifiers: FnvHashMap::default(),
            scope_depth: 0,
        }
    }

    fn current_chunk(&self) -> &Chunk {
        &self.chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }

    fn add_instruction(&mut self, op: Opcode, line: usize) {
        self.current_chunk_mut().write_u8(op, line)
    }

    fn add_instruction_arg(&mut self, op: Opcode, arg: u16, line: usize) {
        self.current_chunk_mut().write_arg_u16(op, arg, line);
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
            .map(|u| *u)
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::UndefinedVariable {
                    name: name.lexeme.to_owned(),
                })
                .line(name.line)
            })
    }

    fn get_local_slot(&self, name: &Token) -> Option<u16> {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if &local.name == name.lexeme {
                return Some(i as u16);
            }
        }

        None
    }

    fn make_variable(&mut self, name: &Token) -> Result<(), PiccoloError> {
        // are we in global scope?
        if self.scope_depth > 0 {
            // check if we have a local with this name
            if let Some(idx) = self.get_local_slot(name) {
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
            self.chunk
                .write_arg_u16(Opcode::DeclareGlobal, idx, name.line);
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn emitter3() {
        let ast = crate::parse(&mut crate::Scanner::new(
            "x =: 32\n\
             retn x",
        ))
        .unwrap();

        let mut e = super::Emitter3::new();
        super::compile_ast(&mut e, &ast).unwrap();
        println!("{}", e.current_chunk().disassemble("jioew"));

        crate::runtime::vm::Machine::new()
            .interpret(e.current_chunk())
            .unwrap();
    }
}
