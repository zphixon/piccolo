use fnv::FnvHashMap;
use slotmap::DefaultKey;

use crate::{
    compiler::{
        ast::{Expr, Stmt},
        ns::{NamespaceRepository, VariableLocation},
        Token, TokenKind,
    },
    error::PiccoloError,
    runtime::{
        interner::{Interner, StringPtr},
        op::Opcode,
        value::{Constant, Value},
    },
    trace,
    v2::Program,
};

use super::State;

#[derive(Default)]
pub struct Emitter<'src> {
    pub program: Program,
    pub repo: NamespaceRepository<'src>,
    pub globals: FnvHashMap<StringPtr, Constant>,
}

impl<'src> Emitter<'src> {
    fn push_op(&mut self, op: Opcode) {
        self.program.push_op(op);
    }

    pub fn to_state(&self) -> State {
        let mut globals: FnvHashMap<StringPtr, Value> = Default::default();

        for (k, v) in self.globals.iter() {
            globals.insert(*k, Value::from_constant(v));
        }

        State {
            globals,
            ..Default::default()
        }
    }
}

pub fn compile<'src>(ast: &[Stmt<'src>]) -> (Emitter<'src>, Interner) {
    let mut interner = Interner::default();
    let mut emitter = Emitter::default();

    let global = emitter.repo.global_key();
    for stmt in ast {
        compile_stmt(&mut emitter, &mut interner, global, stmt).unwrap();
    }

    (emitter, interner)
}

fn compile_stmt<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    stmt: &Stmt<'src>,
) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { token, expr } => compile_expr_stmt(emitter, interner, ns, *token, expr),
        //Stmt::Block { do_, body, end }
        //    => compile_block(emitter, interner, ns, *do_, body, *end),
        Stmt::Declaration { name, value, .. } => {
            compile_declaration(emitter, interner, ns, *name, value)
        }
        //Stmt::Assignment { lval, op, rval }
        //    => compile_assignment(emitter, interner, ns, lval, *op, rval),
        //Stmt::If { if_, cond, do_, then_block, else_, else_block, end }
        //    => compile_if(emitter, interner, ns, *if_, cond, *do_, then_block, else_.as_ref(), else_block.as_ref(), *end),
        //Stmt::While { while_, cond, do_, body, end }
        //    => compile_while(emitter, interner, ns, *while_, cond, *do_, body, *end),
        //Stmt::For { for_, init, cond, name, inc_op, inc_expr, do_, body, end }
        //    => compile_for(emitter, interner, ns, *for_, init.as_ref(), cond, *name, *inc_op, inc_expr, *do_, body, *end),
        //Stmt::ForEach { for_, item, iter, do_, body, end }
        //    => compile_for_each(emitter, interner, ns, *for_, *item, *iter, *do_, body, *end),
        //Stmt::Fn { name, args, arity, body, method, end }
        //    => compile_fn(emitter, interner, ns, *name, args, *arity, body, *method, *end),
        //Stmt::Break { break_ }
        //    => compile_break(program, *break_),
        //Stmt::Continue { continue_ }
        //    => compile_continue(program, *continue_),
        //Stmt::Return { return_, value }
        //    => compile_return(emitter, interner, ns, *return_, value.as_ref()),
        //Stmt::Assert { assert, value }
        //    => compile_assert(emitter, interner, ns, *assert, value),
        //Stmt::Data { name, methods, fields }
        //    => compile_data(emitter, interner, ns, *name, methods, fields),
        _ => todo!("{:?}", stmt),
    }
}

fn compile_expr_stmt<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    _token: Token<'src>,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    compile_expr(emitter, interner, ns, expr)?;
    emitter.push_op(Opcode::Pop);
    Ok(())
}

fn compile_declaration<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    name: Token<'src>,
    value: &Expr<'src>,
) -> Result<(), PiccoloError> {
    trace!("declare {}", name.format());

    compile_expr(emitter, interner, ns, value)?;

    let ptr = interner.allocate_str(name.lexeme);
    let index = emitter.program.num_constants();
    emitter.program.add_constant(Constant::StringPtr(ptr));

    match emitter.repo.new_variable(ns, name, 0)? {
        VariableLocation::Global(_) => {
            emitter.push_op(Opcode::DeclareGlobal(index as u16));
        }
        VariableLocation::Local(_, _) => todo!(),
        VariableLocation::Capture(_, _) => todo!(),
    }

    Ok(())
}

fn compile_expr<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    match expr {
        Expr::Literal { literal } => compile_literal(emitter, interner, ns, *literal),
        //Expr::ArrayLiteral { right_bracket, values }
        //    => compile_array_literal(emitter, interner, ns, *right_bracket, values),
        //Expr::Paren { right_paren, expr }
        //    => compile_paren(emitter, interner, ns, *right_paren, expr),
        Expr::Variable { variable } => compile_variable(emitter, interner, ns, *variable),
        //Expr::Unary { op, rhs }
        //    => compile_unary(emitter, interner, ns, *op, rhs),
        Expr::Binary { lhs, op, rhs } => compile_binary(emitter, interner, ns, lhs, *op, rhs),
        //Expr::Logical { lhs, op, rhs }
        //    => compile_logical(emitter, interner, ns, lhs, *op, rhs),
        //Expr::Call { callee, paren, arity, args }
        //    => compile_call(emitter, interner, ns, callee, *paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(emitter, interner, ns, name, args),
        //Expr::Get { object, name }
        //    => compile_get(emitter, interner, ns, object, *name),
        //Expr::Index { right_bracket, object, index }
        //    => compile_index(emitter, interner, ns, *right_bracket, object, index),
        //Expr::Fn { fn_, args, arity, body, end }
        //    => compile_lambda(emitter, interner, ns, *fn_, args, *arity, body, *end),
        _ => todo!("{:?}", expr),
    }
}

fn compile_literal<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    literal: Token<'src>,
) -> Result<(), PiccoloError> {
    trace!("literal {}", literal.format());

    match literal.kind {
        TokenKind::True => emitter.push_op(Opcode::Bool(true)),
        TokenKind::False => emitter.push_op(Opcode::Bool(false)),
        // TODO make Integer use i64 instead
        TokenKind::Integer(v) if u16::try_from(v).is_ok() => {
            emitter.push_op(Opcode::Integer(v.try_into().unwrap()))
        }
        TokenKind::Nil => emitter.push_op(Opcode::Nil),
        TokenKind::String => {
            let ptr = crate::compiler::maybe_escape_string(interner, literal)?;
            emitter.program.push_constant_op(Constant::StringPtr(ptr));
        }
        _ => emitter
            .program
            .push_constant_op(Constant::try_from(interner, literal)?),
    }

    Ok(())
}

fn compile_variable<'src>(
    emitter: &mut Emitter<'src>,
    _interner: &mut Interner,
    ns: DefaultKey,
    variable: Token<'src>,
) -> Result<(), PiccoloError> {
    trace!("variable {}", variable.format());

    match emitter.repo.get(ns, variable) {
        Some((_, VariableLocation::Global(index))) => {
            emitter.push_op(Opcode::GetGlobal(index as u16))
        }
        Some((_, VariableLocation::Local(_, _))) => todo!(),
        Some((_, VariableLocation::Capture(_, _))) => todo!(),
        None => todo!(),
    }

    Ok(())
}

fn compile_binary<'src>(
    emitter: &mut Emitter<'src>,
    interner: &mut Interner,
    ns: DefaultKey,
    lhs: &Expr<'src>,
    op: Token<'src>,
    rhs: &Expr<'src>,
) -> Result<(), PiccoloError> {
    trace!("binary {}", op.format());

    compile_expr(emitter, interner, ns, lhs)?;
    compile_expr(emitter, interner, ns, rhs)?;

    match op.kind {
        TokenKind::Plus => emitter.push_op(Opcode::Add),
        TokenKind::Minus => emitter.push_op(Opcode::Subtract),
        TokenKind::Divide => emitter.push_op(Opcode::Divide),
        TokenKind::Multiply => emitter.push_op(Opcode::Multiply),
        TokenKind::Equal => emitter.push_op(Opcode::Equal),
        TokenKind::NotEqual => {
            emitter.push_op(Opcode::Equal);
            emitter.push_op(Opcode::Not);
        }
        TokenKind::Greater => emitter.push_op(Opcode::Greater),
        TokenKind::GreaterEqual => emitter.push_op(Opcode::GreaterEqual),
        TokenKind::Less => emitter.push_op(Opcode::Less),
        TokenKind::LessEqual => emitter.push_op(Opcode::LessEqual),
        TokenKind::Modulo => emitter.push_op(Opcode::Modulo),

        TokenKind::ShiftLeft => emitter.push_op(Opcode::ShiftLeft),
        TokenKind::ShiftRight => emitter.push_op(Opcode::ShiftRight),
        TokenKind::BitwiseAnd => emitter.push_op(Opcode::BitAnd),
        TokenKind::BitwiseOr => emitter.push_op(Opcode::BitOr),
        TokenKind::BitwiseXor => emitter.push_op(Opcode::BitXor),

        _ => unreachable!("binary {:?}", op),
    }

    Ok(())
}
