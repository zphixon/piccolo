//! Contains `Emitter`, which converts an AST into a [`Chunk`].
//!
//! [`Chunk`]: ../runtime/chunk/struct.Chunk.html

use crate::runtime::op::Opcode;
use crate::{Chunk, Constant, ErrorKind, PiccoloError, Token, TokenKind};

use super::ast::{Arity, Expr, ExprAccept, ExprVisitor, Stmt, StmtAccept, StmtVisitor};

use std::collections::HashMap;

#[derive(PartialEq)]
struct Local {
    pub(crate) name: String,
    pub(crate) depth: u16,
}

impl Local {
    fn new(name: String, depth: u16) -> Self {
        Self {
            name,
            depth,
        }
    }
}

/// Struct for emitting Piccolo virtual machine bytecode.
///
/// Implements [`StmtVisitor`] and [`ExprVisitor`] to walk the AST, compiling
/// into a [`Chunk`].
///
/// [`ExprVisitor`]: ../ast/trait.ExprVisitor.html
/// [`StmtVisitor`]: ../ast/trait.StmtVisitor.html
/// [`Chunk`]: ../runtime/chunk/struct.Chunk.html
pub struct Emitter {
    chunk: Chunk,
    strings: HashMap<String, u16>,
    identifiers: HashMap<String, u16>,
    scope_depth: u16,
    locals: Vec<Local>,
    continue_offsets: Vec<Vec<usize>>,
    break_offsets: Vec<Vec<usize>>,
}

impl Emitter {
    /// Create a new bytecode emitter.
    pub fn new(chunk: Chunk) -> Self {
        Emitter {
            chunk,
            strings: HashMap::new(),
            identifiers: HashMap::new(),
            scope_depth: 0,
            locals: Vec::new(),
            continue_offsets: Vec::new(),
            break_offsets: Vec::new(),
        }
    }

    /// Get the chunk of the emitter.
    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    /// Compile an AST into a chunk. Moves the emitter's chunk out of itself,
    /// replacing it with [`Chunk::default`].
    ///
    /// [`Chunk::default`]: ../../runtime/chunk/struct.Chunk.html
    pub fn compile(&mut self, stmts: &[Stmt]) -> Result<Chunk, Vec<PiccoloError>> {
        let mut errs = Vec::new();
        for stmt in stmts {
            trace!("stmt {}", super::ast::AstPrinter::print_stmt(stmt));

            match stmt.accept(self) {
                Ok(_) => {}
                Err(e) => errs.push(e),
            }
        }

        if errs.is_empty() {
            Ok(std::mem::take(&mut self.chunk))
        } else {
            Err(errs)
        }
    }

    fn make_variable(&mut self, name: &Token) -> Result<(), PiccoloError>{
        if self.scope_depth > 0 {
            // if there exists some local with this name
            if let Some(idx) = self.get_local_depth(name.lexeme) {
                if idx != self.scope_depth {
                    // create a new local if we're in a different scope
                    self.locals.push(Local::new(name.lexeme.to_owned(), self.scope_depth));
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
                // create a new local with this name
                self.locals.push(Local::new(name.lexeme.to_owned(), self.scope_depth));
            }
        } else {
            let idx = self.make_ident(name.lexeme);
            self.chunk
                .write_arg_u16(Opcode::DeclareGlobal, idx, name.line);
        }

        Ok(())
    }

    fn get_local_slot(&self, name: &str) -> Option<u16> {
        trace!("get local slot for '{}'", name);

        for (i, local) in self.locals.iter().enumerate().rev() {
            if &local.name == name {
                return Some(i as u16);
            }
        }

        None
    }

    fn get_local_depth(&self, name: &str) -> Option<u16> {
        trace!("get local depth for '{}'", name);

        for local in self.locals.iter().rev() {
            if &local.name == name {
                return Some(local.depth);
            }
        }

        None
    }

    fn make_ident(&mut self, name: &str) -> u16 {
        trace!("make ident '{}'", name);

        if self.identifiers.contains_key(name) {
            self.identifiers[name]
        } else {
            let idx = self.chunk.make_constant(Constant::String(name.to_owned()));
            self.identifiers.insert(name.to_owned(), idx);
            idx
        }
    }

    fn get_ident(&self, name: &Token) -> Result<u16, PiccoloError> {
        trace!("get ident '{}'", name.lexeme);

        if self.identifiers.contains_key(name.lexeme) {
            Ok(self.identifiers[name.lexeme])
        } else {
            Err(PiccoloError::new(ErrorKind::UndefinedVariable {
                name: name.lexeme.to_owned(),
            })
            .line(name.line))
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self, line: usize) {
        self.scope_depth -= 1;
        while !self.locals.is_empty() && self.locals[self.locals.len() - 1].depth > self.scope_depth {
            self.chunk.write_u8(Opcode::Pop, line);
            self.locals.pop().unwrap();
        }
    }
}

impl ExprVisitor for Emitter {
    type Output = Result<(), PiccoloError>;

    fn visit_literal(&mut self, literal: &Token) -> Self::Output {
        trace!("{}: literal '{}'", literal.line, literal.lexeme);
        let i = if literal.kind == TokenKind::String && self.strings.contains_key(literal.lexeme) {
            trace!("has string {}", literal.lexeme);

            *self.strings.get(literal.lexeme).unwrap()
        } else if literal.kind == TokenKind::String {
            let i = self.chunk.make_constant(Constant::try_from(*literal)?);
            self.strings.insert(literal.lexeme.to_string(), i);
            i
        } else if literal.kind == TokenKind::Nil {
            self.chunk.write_u8(Opcode::Nil, literal.line);
            return Ok(());
        } else if literal.kind == TokenKind::True {
            self.chunk.write_u8(Opcode::True, literal.line);
            return Ok(());
        } else if literal.kind == TokenKind::False {
            self.chunk.write_u8(Opcode::False, literal.line);
            return Ok(());
        } else {
            self.chunk
                .make_constant(Constant::try_from(*literal).unwrap())
        };

        self.chunk.write_arg_u16(Opcode::Constant, i, literal.line);

        Ok(())
    }

    fn visit_paren(&mut self, _right_paren: &Token, value: &Expr) -> Self::Output {
        trace!("{}: paren", _right_paren.line);
        value.accept(self)
    }

    fn visit_variable(&mut self, variable: &Token) -> Self::Output {
        trace!("{}: variable '{}'", variable.line, variable.lexeme);
        match self.get_local_slot(variable.lexeme) {
            Some(idx) => self
                .chunk
                .write_arg_u16(Opcode::GetLocal, idx, variable.line),
            None => {
                let i = self.get_ident(variable)?;
                self.chunk
                    .write_arg_u16(Opcode::GetGlobal, i, variable.line);
            }
        }
        Ok(())
    }

    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> Self::Output {
        trace!("{}: unary {}", op.line, op.lexeme);
        rhs.accept(self)?;
        match op.kind {
            TokenKind::Not => self.chunk.write_u8(Opcode::Not, op.line),
            TokenKind::Minus => self.chunk.write_u8(Opcode::Negate, op.line),
            _ => unreachable!("unrecognized unary op {:?}", op),
        }
        Ok(())
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        trace!("{}: binary {}", op.line, op.lexeme);

        if let Expr::Binary { op, .. } = lhs {
            if super::parser::infix_binding_power(op.kind)
                == super::parser::BindingPower::Comparison
            {
                return Err(PiccoloError::new(ErrorKind::SyntaxError)
                    .line(op.line)
                    .msg("cannot chain comparison operators"));
            }
        }

        lhs.accept(self)?;
        rhs.accept(self)?;

        match op.kind {
            TokenKind::Plus => self.chunk.write_u8(Opcode::Add, op.line),
            TokenKind::Minus => self.chunk.write_u8(Opcode::Subtract, op.line),
            TokenKind::Divide => self.chunk.write_u8(Opcode::Divide, op.line),
            TokenKind::Multiply => self.chunk.write_u8(Opcode::Multiply, op.line),
            TokenKind::Equal => self.chunk.write_u8(Opcode::Equal, op.line),
            TokenKind::NotEqual => {
                self.chunk.write_u8(Opcode::Equal, op.line);
                self.chunk.write_u8(Opcode::Not, op.line);
            }
            TokenKind::Greater => self.chunk.write_u8(Opcode::Greater, op.line),
            TokenKind::GreaterEqual => self.chunk.write_u8(Opcode::GreaterEqual, op.line),
            TokenKind::Less => self.chunk.write_u8(Opcode::Less, op.line),
            TokenKind::LessEqual => self.chunk.write_u8(Opcode::LessEqual, op.line),
            TokenKind::Modulo => self.chunk.write_u8(Opcode::Modulo, op.line),
            _ => unreachable!("unrecognized binary op {:?}", op),
        }

        Ok(())
    }

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        trace!("{}: logical {}", op.line, op.lexeme);

        let jump_op = match op.kind {
            TokenKind::LogicalAnd => Opcode::JumpFalse,
            TokenKind::LogicalOr => Opcode::JumpTrue,
            _ => unreachable!("op {:?} for logical", op),
        };

        lhs.accept(self)?;

        let short = self.chunk.start_jump(jump_op, op.line);
        self.chunk.write_u8(Opcode::Pop, op.line);

        rhs.accept(self)?;

        self.chunk.patch_jump(short);

        Ok(())
    }

    fn visit_call(
        &mut self,
        _callee: &Expr,
        _paren: &Token,
        _arity: Arity,
        _args: &[Expr],
    ) -> Self::Output {
        trace!("{}: call", _paren.line);
        todo!("visit_call")
    }

    fn visit_new(&mut self, _name: &Token, _args: &[(Token, Box<Expr>)]) -> Self::Output {
        trace!("{}: new", _name.line);
        todo!("visit_new")
    }

    fn visit_get(&mut self, _object: &Expr, _name: &Token) -> Self::Output {
        trace!("{}: get '{}'", _name.line, _name.lexeme);
        todo!("visit_get")
    }

    fn visit_set(&mut self, _object: &Expr, _name: &Token, _value: &Expr) -> Self::Output {
        trace!("{}: set '{}'", _name.line, _name.lexeme);
        todo!("visit_set")
    }

    fn visit_index(&mut self, _right_bracket: &Token, _object: &Expr, _idx: &Expr) -> Self::Output {
        trace!("{}: index", _right_bracket.line);
        todo!("visit_index")
    }

    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        trace!("{}: fn '{}'", _name.line, _name.lexeme);
        todo!("visit_func")
    }
}

impl StmtVisitor for Emitter {
    type Output = Result<(), PiccoloError>;

    fn visit_expr(&mut self, token: &Token, expr: &Expr) -> Self::Output {
        trace!("stmt expr");
        expr.accept(self)?;
        self.chunk.write_u8(Opcode::Pop, token.line);
        Ok(())
    }

    fn visit_block(&mut self, end: &Token, body: &[Stmt]) -> Self::Output {
        trace!("{}: block", end.line);
        self.begin_scope();
        for stmt in body {
            stmt.accept(self)?;
        }
        self.end_scope(end.line);
        Ok(())
    }

    fn visit_declaration(&mut self, name: &Token, _op: &Token, value: &Expr) -> Self::Output {
        trace!("{}: declare {}", name.line, name.lexeme);

        value.accept(self)?;
        self.make_variable(name)?;

        Ok(())
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output {
        trace!("{}: assign {}", name.line, name.lexeme);
        value.accept(self)?;
        if self.scope_depth > 0 {
            if let Some(idx) = self.get_local_slot(name.lexeme) {
                // local variable exists, reassign it
                self.chunk.write_arg_u16(Opcode::SetLocal, idx, op.line);
            } else {
                // reassign global variable, checking for existence at runtime
                let i = self.get_ident(name)?;
                self.chunk.write_arg_u16(Opcode::SetGlobal, i, name.line);
            }
        } else {
            let idx = self.get_ident(name)?;
            self.chunk.write_arg_u16(Opcode::SetGlobal, idx, name.line);
        }

        Ok(())
    }

    fn visit_if(
        &mut self,
        if_: &Token,
        cond: &Expr,
        then_block: &[Stmt],
        else_: Option<&Token>,
        else_block: Option<&Vec<Stmt>>,
        end: &Token,
    ) -> Self::Output {
        trace!("{}: if", if_.line);
        // compile the condition
        cond.accept(self)?;

        if let Some(else_block) = else_block {
            // jump if the condition is false
            let jump_else = self.chunk.start_jump(Opcode::JumpFalse, if_.line);
            // pop the condition
            self.chunk.write_u8(Opcode::Pop, if_.line);
            // evaluate the then-block
            self.visit_block(end, then_block)?;
            // skip past the else-block
            let end_jump = self
                .chunk
                .start_jump(Opcode::JumpForward, else_.unwrap().line);

            // pop the condition
            self.chunk.patch_jump(jump_else);
            self.chunk.write_u8(Opcode::Pop, else_.unwrap().line);
            // evaluate the else-block
            self.visit_block(else_.unwrap(), else_block)?;
            self.chunk.patch_jump(end_jump);
        } else {
            // jump if the condition is false
            let jump_end = self.chunk.start_jump(Opcode::JumpFalse, if_.line);
            // pop the condition if true
            self.chunk.write_u8(Opcode::Pop, if_.line);
            // evaluate the then-block
            self.visit_block(end, then_block)?;
            // skip over the pop instruction
            let jump_pop = self.chunk.start_jump(Opcode::JumpForward, end.line);
            // pop the condition if false
            self.chunk.patch_jump(jump_end);
            self.chunk.write_u8(Opcode::Pop, end.line);
            self.chunk.patch_jump(jump_pop);
        }

        Ok(())
    }

    fn visit_while(
        &mut self,
        while_: &Token,
        cond: &Expr,
        body: &[Stmt],
        end: &Token,
    ) -> Self::Output {
        trace!("{}: while", while_.line);
        // calculate condition
        let loop_start = self.chunk.data.len();
        self.continue_offsets.push(Vec::new());
        self.break_offsets.push(Vec::new());
        cond.accept(self)?;

        // jump over the loop if it's false, pop if not
        let exit_jump = self.chunk.start_jump(Opcode::JumpFalse, while_.line);
        self.chunk.write_u8(Opcode::Pop, while_.line);

        // execute the body
        self.visit_block(end, body)?;

        // if there were any continue statements, jump
        // to the jump which goes back to the condition
        for offset in self.continue_offsets.pop().unwrap() {
            trace!("patch continue while at {:x}", offset);
            self.chunk.patch_jump(offset);
        }

        // go back to the condition
        self.chunk.write_jump_back(loop_start, end.line);

        // pop the condition
        self.chunk.patch_jump(exit_jump);
        self.chunk.write_u8(Opcode::Pop, end.line);

        // if there were any break statements, jump out of the loop
        for offset in self.break_offsets.pop().unwrap() {
            trace!("patch break while at {:x}", offset);
            self.chunk.patch_jump(offset);
        }

        Ok(())
    }

    fn visit_for(
        &mut self,
        for_: &Token,
        init: &Stmt,
        cond: &Expr,
        inc: &Stmt,
        body: &[Stmt],
        end: &Token,
    ) -> Self::Output {
        trace!("{}: for {}", for_.line, for_.lexeme);

        // execute the initializer
        self.begin_scope();
        init.accept(self)?;

        // jump to the condition at the end of a cycle
        let start_offset = self.chunk.data.len();
        cond.accept(self)?;
        // jump over the body if the condition fails
        let end_jump = self.chunk.start_jump(Opcode::JumpFalse, for_.line);
        self.chunk.write_u8(Opcode::Pop, for_.line);

        // execute the body
        self.break_offsets.push(Vec::new());
        self.continue_offsets.push(Vec::new());
        self.visit_block(end, body)?;

        // if there were any continue statements, jump to the increment
        for offset in self.continue_offsets.pop().unwrap() {
            trace!("patch continue in for at {:x}", offset);
            self.chunk.patch_jump(offset);
        }

        // execute the increment
        inc.accept(self)?;

        // unconditional jump back to the condition
        self.chunk.write_jump_back(start_offset, end.line);

        // jump here if the condition fails
        self.chunk.patch_jump(end_jump);
        self.chunk.write_u8(Opcode::Pop, end.line);

        // if there were any break statements, jump out of the loop
        for offset in self.break_offsets.pop().unwrap() {
            trace!("patch break in for at {:x}", offset);
            self.chunk.patch_jump(offset);
        }

        // drop all the local variables
        self.end_scope(end.line);
        Ok(())
    }

    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        trace!("{}: fn {}", _name.line, _name.lexeme);
        todo!("visit_func")
    }

    fn visit_break(&mut self, break_: &Token) -> Self::Output {
        let offset = self.chunk.start_jump(Opcode::JumpForward, break_.line);
        self.break_offsets
            .last_mut()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::SyntaxError)
                    .msg("cannot break outside of a loop")
                    .line(break_.line)
            })?
            .push(offset);
        trace!("start break at {:x}", offset);
        Ok(())
    }

    fn visit_continue(&mut self, continue_: &Token) -> Self::Output {
        let offset = self.chunk.start_jump(Opcode::JumpForward, continue_.line);
        self.continue_offsets
            .last_mut()
            .ok_or_else(|| {
                PiccoloError::new(ErrorKind::SyntaxError)
                    .msg("cannot continue outside of a loop")
                    .line(continue_.line)
            })?
            .push(offset);
        trace!("start continue at {:x}", offset);
        Ok(())
    }

    fn visit_retn(&mut self, retn: &Token, value: Option<&Expr>) -> Self::Output {
        trace!("{}: retn", retn.line);
        if let Some(expr) = value {
            expr.accept(self)?;
        }
        self.chunk.write_u8(Opcode::Return, retn.line);
        Ok(())
    }

    fn visit_assert(&mut self, assert: &Token, value: &Expr) -> Self::Output {
        trace!("{}: assert", assert.line);
        value.accept(self)?;
        self.chunk.write_u8(Opcode::Assert, assert.line);
        Ok(())
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> Self::Output {
        trace!("{}: data {}", _name.line, _name.lexeme);
        todo!("visit_data")
    }
}
