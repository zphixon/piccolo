//! Bytecode compiler.

use crate::{
    compiler::ast::{Ast, Expr, Stmt},
    Chunk, ChunkIndex, ChunkOffset, Constant, ConstantIndex, ErrorKind, Line, LocalSlotIndex,
    Opcode, PiccoloError, Token, TokenKind, UpvalueIndex,
};

use super::Local;
use crate::runtime::LocalScopeDepth;

use fnv::FnvHashMap;

// top level compile into chunk index 0
// encounter a function, start compiling into a new chunk index
// after we end the function collect upvalues

pub fn compile(ast: &Ast) -> Result<Module, Vec<PiccoloError>> {
    let mut emitter = Emitter::new();
    let errors: Vec<_> = ast
        .iter()
        .map(|stmt| compile_stmt(&mut emitter, stmt))
        .filter_map(Result::err)
        .collect();

    if errors.is_empty() {
        Ok(emitter.into_module())
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
        Stmt::Fn { name, args, arity, body, method }
            => compile_fn(emitter, name, args, *arity, body, *method),
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
        Expr::Call { callee, paren, arity, args }
            => compile_call(emitter, callee, paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(emitter, name, args),
        // Expr::Get { object, name }
        //     => compile_get(emitter, object, name),
        // Expr::Set { object, name, value }
        //     => compile_set(emitter, object, name, value),
        // Expr::Index { right_bracket, object, index }
        //     => compile_index(emitter, right_bracket, object, index),
        // Expr::Fn { name, args, arity, body, method }
        //     => compile_fn(emitter, name, args, *arity, body, *method),
        _ => todo!("{:?}", expr),
    }
}

fn compile_expr_stmt(
    emitter: &mut Emitter,
    token: &Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} expr stmt", token.line);
    compile_expr(emitter, expr)?;
    emitter.add_instruction(Opcode::Pop, token.line);
    Ok(())
}

fn compile_block(emitter: &mut Emitter, end: &Token, body: &[Stmt]) -> Result<(), PiccoloError> {
    trace!("{} block", end.line);
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
    trace!("{} decl {}", name.line, name.lexeme);

    compile_expr(emitter, value)?;
    emitter.make_variable(name)
}

fn compile_assignment(
    emitter: &mut Emitter,
    name: &Token,
    op: &Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} assign {}", name.line, name.lexeme);

    if let Some(opcode) = op.assign_by_mutate_op() {
        // if this is an assignment-by-mutation operator, first get the value of the variable
        if let Some(index) = emitter.current_context().get_local_slot(name) {
            emitter.add_instruction_arg(Opcode::GetLocal, index, op.line);
        } else {
            let index = emitter.get_global_ident(name)?;
            emitter.add_instruction_arg(Opcode::GetGlobal, index, op.line);
        }

        // calculate the value to mutate with
        compile_expr(emitter, value)?;

        // then mutate
        emitter.add_instruction(opcode, op.line);
    } else {
        // otherwise just calculate the value
        compile_expr(emitter, value)?;
    }

    // then assign
    if emitter.current_context().is_local() {
        if let Some(index) = emitter.current_context().get_local_slot(name) {
            emitter.add_instruction_arg(Opcode::SetLocal, index, op.line);
        } else {
            let index = emitter.get_global_ident(name)?;
            emitter.add_instruction_arg(Opcode::SetGlobal, index, op.line);
        }
    } else {
        let index = emitter.get_global_ident(name)?;
        emitter.add_instruction_arg(Opcode::SetGlobal, index, op.line);
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
    trace!("{} if", if_.line);

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
    trace!("{} while", while_.line);

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
    trace!("{} for", for_.line);

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

fn compile_fn(
    emitter: &mut Emitter,
    name: &Token,
    args: &[Token],
    arity: usize,
    body: &[Stmt],
    _method: bool,
) -> Result<(), PiccoloError> {
    // declare variable w emitter & identifier
    //      if the compiler is scoped (current context locals scope depth > 0), and has a local in scope with that name, error
    //      otherwise add local
    //          get current context locals, insert identifier
    // if compiler is scoped
    //      mark local as initialized
    //          current context locals top local set initialized true
    // compile closure w information from above
    // inside new compiler context scoped
    //      declare & define each argument, local, so just initialize
    //      compile block with current new context
    //      add a return instruction I suppose
    // make a new Function using the info and chunk index from the scoped context
    // make a new Closure with upvalues from the scoped context
    // add a constant function/closure
    // add a constant instruction
    if let Some(_) = emitter.current_context().get_local_slot(name) {
        return Err(PiccoloError::new(ErrorKind::SyntaxError)
            .line(name.line)
            .msg_string(format!(
                "Function with name '{}' already exists",
                name.lexeme
            )));
    } else if emitter.current_context().is_local() {
        emitter.make_variable(name)?;
    }

    emitter.begin_context();
    emitter.begin_scope();

    for arg in args {
        emitter.make_variable(arg)?;
    }

    compile_block(emitter, name, body)?;
    emitter.add_instruction(Opcode::Nil, name.line);
    emitter.add_instruction(Opcode::Return, name.line);

    let chunk_index = emitter.end_context();

    let function = Function::new(arity, name.lexeme.to_owned(), chunk_index);

    let constant = emitter.make_constant(Constant::Function(function));
    emitter.add_instruction_arg(Opcode::Constant, constant, name.line);

    if !emitter.current_context().is_local() {
        emitter.make_variable(name)?;
    }

    Ok(())
}

fn compile_break(emitter: &mut Emitter, break_: &Token) -> Result<(), PiccoloError> {
    trace!("{} break", break_.line);

    let offset = emitter.start_jump(Opcode::JumpForward, break_.line);
    emitter.add_break(offset, break_)?;

    Ok(())
}

fn compile_continue(emitter: &mut Emitter, continue_: &Token) -> Result<(), PiccoloError> {
    trace!("{} continue", continue_.line);

    let offset = emitter.start_jump(Opcode::JumpForward, continue_.line);
    emitter.add_continue(offset, continue_)?;

    Ok(())
}

fn compile_retn(
    emitter: &mut Emitter,
    retn: &Token,
    expr: Option<&Expr>,
) -> Result<(), PiccoloError> {
    trace!("{} retn", retn.line);

    if let Some(expr) = expr {
        compile_expr(emitter, expr)?;
    }

    emitter.add_instruction(Opcode::Return, retn.line);

    Ok(())
}

fn compile_assert(emitter: &mut Emitter, assert: &Token, value: &Expr) -> Result<(), PiccoloError> {
    trace!("{} assert", assert.line);

    compile_expr(emitter, value)?;
    //emitter.add_instruction(Opcode::Assert, assert.line);

    let c = emitter.make_constant(Constant::String(super::ast::print_expression(value)));
    emitter.add_instruction_arg(Opcode::Assert, c, assert.line);

    Ok(())
}

fn compile_literal(emitter: &mut Emitter, literal: &Token) -> Result<(), PiccoloError> {
    trace!("{} literal", literal.line);

    // TODO: use constant ops instead of Constant ops
    emitter.add_constant(Constant::try_from(*literal)?, literal.line);
    Ok(())
}

fn compile_paren(
    emitter: &mut Emitter,
    right_paren: &Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    trace!("{} paren", right_paren.line);

    compile_expr(emitter, expr).map_err(|e| {
        e.msg_string(format!(
            "in expression starting on line {}",
            right_paren.line
        ))
    })
}

fn compile_variable(emitter: &mut Emitter, variable: &Token) -> Result<(), PiccoloError> {
    trace!("{} variable {}", variable.line, variable.lexeme);

    if let Some(local) = emitter.current_context().get_local_slot(variable) {
        emitter.add_instruction_arg(Opcode::GetLocal, local, variable.line);
    } else {
        let global = emitter.get_global_ident(&variable)?;
        emitter.add_instruction_arg(Opcode::GetGlobal, global, variable.line);
    }
    Ok(())
}

fn compile_unary(emitter: &mut Emitter, op: &Token, rhs: &Expr) -> Result<(), PiccoloError> {
    trace!("{} unary {}", op.line, op.lexeme);

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
    trace!("{} binary {}", op.line, op.lexeme);

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

        TokenKind::ShiftLeft => emitter.add_instruction(Opcode::ShiftLeft, op.line),
        TokenKind::ShiftRight => emitter.add_instruction(Opcode::ShiftRight, op.line),
        TokenKind::BitwiseAnd => emitter.add_instruction(Opcode::BitAnd, op.line),
        TokenKind::BitwiseOr => emitter.add_instruction(Opcode::BitOr, op.line),
        TokenKind::BitwiseXor => emitter.add_instruction(Opcode::BitXor, op.line),

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
    trace!("{} logical {}", op.line, op.lexeme);

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

fn compile_call(
    emitter: &mut Emitter,
    callee: &Expr,
    paren: &Token,
    arity: usize,
    args: &[Expr],
) -> Result<(), PiccoloError> {
    compile_expr(emitter, callee)?;
    for arg in args {
        compile_expr(emitter, arg)?;
    }
    emitter.add_instruction_arg(Opcode::Call, arity as u16, paren.line);
    Ok(())
}

// represents more of a scope
// TODO refactor to an enum?
#[derive(Default)]
struct EmitterContext {
    //context_type: ContextType,
    chunk_index: ChunkIndex,
    locals: Vec<Local>,
    depth: LocalScopeDepth,
    //upvalues: Vec<Upvalue>,
}

impl EmitterContext {
    fn new(chunk_index: ChunkIndex) -> Self {
        Self {
            chunk_index,
            locals: Vec::new(),
            depth: 0,
        }
    }

    fn get_local(&self, index: LocalSlotIndex) -> &Local {
        &self.locals[index as usize]
    }

    fn add_local(&mut self, name: &Token) {
        self.locals
            .push(Local::new(name.lexeme.to_owned(), self.scope_depth()));
    }

    fn is_local(&self) -> bool {
        self.depth > 0
    }

    fn scope_depth(&self) -> LocalScopeDepth {
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

    fn get_local_slot(&self, name: &Token) -> Option<LocalSlotIndex> {
        // IF SCOPE IS FUNCTION AND SCOPE DEPTH DOESN'T MATCH THIS IS A CAPTURE
        // mark as captured
        trace!("{} get local slot {}", name.line, name.lexeme);
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == name.lexeme {
                return Some(i as u16);
            }
        }

        None
    }

    fn get_local_depth(&self, name: &Token) -> Option<LocalScopeDepth> {
        trace!("{} get local depth {}", name.line, name.lexeme);
        for local in self.locals.iter().rev() {
            if local.name == name.lexeme {
                return Some(local.depth);
            }
        }

        None
    }
}

use crate::prelude::Function;
use crate::runtime::chunk::Module;

/// Bytecode compiler object
///
/// Construct an Emitter, and pass a `&mut` reference to `compile_ast` along with
/// the abstract syntax tree. Extract the `Chunk` with `current_chunk{_mut}()` or
/// `into_chunk()`.
pub struct Emitter {
    module: Module,
    children: Vec<EmitterContext>,
    //errors: Vec<PiccoloError>,
    global_identifiers: FnvHashMap<String, ConstantIndex>,
    continue_offsets: Vec<Vec<ChunkOffset>>,
    break_offsets: Vec<Vec<ChunkOffset>>,
}

impl Emitter {
    pub fn new() -> Self {
        Self {
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

    pub fn current_chunk(&self) -> &Chunk {
        self.module.chunk(self.current_context().chunk_index)
    }

    pub fn current_chunk_mut(&mut self) -> &mut Chunk {
        self.module.chunk_mut(self.current_context().chunk_index)
    }

    pub fn into_module(self) -> Module {
        self.module
    }

    fn add_instruction(&mut self, op: Opcode, line: Line) {
        self.current_chunk_mut().write_u8(op, line);
    }

    fn add_instruction_arg(&mut self, op: Opcode, arg: u16, line: Line) {
        self.current_chunk_mut().write_arg_u16(op, arg, line);
    }

    fn add_jump_back(&mut self, offset: ChunkOffset, line: Line) {
        self.current_chunk_mut().write_jump_back(offset, line);
    }

    fn add_constant(&mut self, value: Constant, line: Line) {
        let index = self.make_constant(value);
        self.current_chunk_mut()
            .write_arg_u16(Opcode::Constant, index, line);
    }

    fn make_constant(&mut self, c: Constant) -> ConstantIndex {
        self.module.make_constant(c)
    }

    fn make_global_ident(&mut self, name: &Token) -> ConstantIndex {
        trace!("{} make global {}", name.line, name.lexeme);

        if self.global_identifiers.contains_key(name.lexeme) {
            self.global_identifiers[name.lexeme]
        } else {
            let index = self.make_constant(Constant::String(name.lexeme.to_owned()));
            self.global_identifiers
                .insert(name.lexeme.to_owned(), index);
            index
        }
    }

    fn get_global_ident(&self, name: &Token) -> Result<ConstantIndex, PiccoloError> {
        trace!("{} get global {}", name.line, name.lexeme);

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

    fn make_variable(&mut self, name: &Token) -> Result<(), PiccoloError> {
        trace!("{} make variable {}", name.line, name.lexeme);

        // are we in global scope?
        if self.current_context().is_local() {
            // check if we have a local with this name
            if let Some(index) = self.current_context().get_local_depth(name) {
                // if we do,
                if index != self.current_context().scope_depth() {
                    // create a new local if we're in a different scope
                    self.current_context_mut().add_local(name);
                } else {
                    // error if we're in the same scope
                    return Err(PiccoloError::new(ErrorKind::SyntaxError)
                        .line(name.line)
                        .msg_string(format!(
                            "variable with name '{}' already exists",
                            name.lexeme,
                        )));
                }
            } else {
                // if we don't, create a new local with this name
                self.current_context_mut().add_local(name);
            }
        } else {
            // yes, make a global
            let index = self.make_global_ident(name);
            self.add_instruction_arg(Opcode::DeclareGlobal, index, name.line);
        }

        Ok(())
    }

    fn begin_context(&mut self) {
        let index = self.module.add_chunk();
        self.children.push(EmitterContext::new(index));
    }

    fn end_context(&mut self) -> ChunkIndex {
        let context = self.children.pop().unwrap();
        context.chunk_index
    }

    fn begin_scope(&mut self) {
        self.current_context_mut().begin_scope();
    }

    fn end_scope(&mut self, line: Line) {
        for _ in self.current_context_mut().end_scope() {
            self.add_instruction(Opcode::Pop, line);
        }
    }

    fn start_jump(&mut self, op: Opcode, line: Line) -> ChunkOffset {
        self.current_chunk_mut().start_jump(op, line)
    }

    fn patch_jump(&mut self, offset: ChunkOffset) {
        self.current_chunk_mut().patch_jump(offset);
    }

    fn start_loop_jumps(&mut self) -> ChunkOffset {
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

    fn add_break(&mut self, offset: ChunkOffset, break_: &Token) -> Result<(), PiccoloError> {
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

    fn add_continue(&mut self, offset: ChunkOffset, continue_: &Token) -> Result<(), PiccoloError> {
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
        let module = compile(&ast).unwrap();
        println!("{}", crate::runtime::chunk::disassemble(&module, "jioew"));
        let mut heap = crate::runtime::memory::Heap::default();
        crate::runtime::vm::Machine::new(&mut heap, &module)
            .interpret(&mut heap)
            .unwrap();
    }
}
