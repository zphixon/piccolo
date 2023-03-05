use crate::{
    compiler::{
        ast::{Expr, Stmt},
        ns::{NamespaceRepository, VariableLocation},
        Token, TokenKind,
    },
    error::PiccoloError,
    runtime::{op::Opcode, value::Constant},
    trace,
    v2::{Func, Program, State},
};
use fnv::FnvHashMap;
use slotmap::DefaultKey;

#[derive(Default)]
pub struct Emitter<'src> {
    pub programs: Vec<Program>,
    current_program: usize,
    pub repo: NamespaceRepository<'src>,
    pub builtins: FnvHashMap<&'src str, Box<dyn Func>>,
}

impl Emitter<'_> {
    fn program(&mut self) -> &mut Program {
        &mut self.programs[self.current_program]
    }

    fn begin_program(&mut self) -> usize {
        let old = self.current_program;
        self.programs.push(Program::default());
        // bleeehhhhhh
        old
    }
}

pub fn compile(ast: &[Stmt]) -> Result<(State, Program), PiccoloError> {
    let mut state = State::default();
    let mut emitter = Emitter::default();

    let global = emitter.repo.global_key();
    for stmt in ast {
        compile_stmt(&mut state, &mut emitter, global, stmt)?;
    }

    emitter.program().push_op(Opcode::Return);
    Ok((state, emitter.programs.pop().unwrap()))
}

#[rustfmt::skip]
fn compile_stmt<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    stmt: &Stmt<'src>,
) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { token, expr }
            => compile_expr_stmt(state, emitter, ns, *token, expr),
        Stmt::Block { do_, body, end }
            => compile_block(state, emitter, ns, *do_, body, *end),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(state, emitter, ns, *name, value),
        //Stmt::Assignment { lval, op, rval }
        //    => compile_assignment(state, emitter, ns, lval, *op, rval),
        //Stmt::If { if_, cond, do_, then_block, else_, else_block, end }
        //    => compile_if(state, emitter, ns, *if_, cond, *do_, then_block, else_.as_ref(), else_block.as_ref(), *end),
        //Stmt::While { while_, cond, do_, body, end }
        //    => compile_while(state, emitter, ns, *while_, cond, *do_, body, *end),
        //Stmt::For { for_, init, cond, name, inc_op, inc_expr, do_, body, end }
        //    => compile_for(state, emitter, ns, *for_, init.as_ref(), cond, *name, *inc_op, inc_expr, *do_, body, *end),
        //Stmt::ForEach { for_, item, iter, do_, body, end }
        //    => compile_for_each(state, emitter, ns, *for_, *item, *iter, *do_, body, *end),
        Stmt::Fn { name, args, arity, body, method, end }
            => compile_fn(state, emitter, ns, *name, args, *arity, body, *method, *end),
        //Stmt::Break { break_ }
        //    => compile_break(program, *break_),
        //Stmt::Continue { continue_ }
        //    => compile_continue(program, *continue_),
        //Stmt::Return { return_, value }
        //    => compile_return(state, emitter, ns, *return_, value.as_ref()),
        Stmt::Assert { assert, value }
            => compile_assert(state, emitter, ns, *assert, value),
        //Stmt::Data { name, methods, fields }
        //    => compile_data(state, emitter, ns, *name, methods, fields),
        _ => todo!("{:?}", stmt),
    }
}

fn compile_expr_stmt<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    _token: Token<'src>,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    compile_expr(state, emitter, ns, expr)?;
    emitter.program().push_op(Opcode::Pop);
    Ok(())
}

fn compile_block<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    do_: Token<'src>,
    body: &[Stmt<'src>],
    _end: Token<'src>,
) -> Result<(), PiccoloError> {
    let inner = emitter.repo.with_parent(ns, do_);
    for stmt in body {
        compile_stmt(state, emitter, inner, stmt)?;
    }
    Ok(())
}

fn compile_declaration<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    name: Token<'src>,
    value: &Expr<'src>,
) -> Result<(), PiccoloError> {
    trace!("declare {}", name.format());

    compile_expr(state, emitter, ns, value)?;

    let ptr = state.interner.allocate_str(name.lexeme);
    let index = emitter.program().num_constants();
    emitter.program().add_constant(Constant::StringPtr(ptr));

    match emitter.repo.new_variable(ns, name)? {
        VariableLocation::Global => {
            emitter.program().push_op(Opcode::DeclareGlobal(index as u16));
        }

        VariableLocation::Local { .. } => {
            // :)
        }

        VariableLocation::Capture { .. } => todo!(),
        VariableLocation::Builtin => todo!("redefining a builtin always an error"),
    }

    Ok(())
}

fn compile_fn<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    name: Token<'src>,
    args: &[Token<'src>],
    _arity: usize,
    body: &[Stmt<'src>],
    _method: bool,
    end: Token<'src>,
) -> Result<(), PiccoloError> {
    let ns_body = emitter.repo.with_parent_captures(ns, name);
    emitter.repo.new_variable(ns_body, name)?;
    for arg in args {
        emitter.repo.new_variable(ns_body, *arg)?;
    }

    compile_block(state, emitter, ns_body, name, body, end)?;

    emitter.program().push_op(Opcode::Return);
    Ok(())
}

fn compile_assert<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    _assert: Token<'src>,
    value: &Expr<'src>,
) -> Result<(), PiccoloError> {
    compile_expr(state, emitter, ns, value)?;

    // TODO actual span in file
    let ast = format!("{:?}", value);
    let ptr = state.interner.allocate_string(ast);
    let index = emitter.program().add_constant(Constant::StringPtr(ptr));
    emitter.program().push_op(Opcode::Assert(index));

    Ok(())
}

#[rustfmt::skip]
fn compile_expr<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    match expr {
        Expr::Literal { literal }
            => compile_literal(state, emitter, ns, *literal),
        //Expr::ArrayLiteral { right_bracket, values }
        //    => compile_array_literal(state, emitter, ns, *right_bracket, values),
        Expr::Paren { right_paren, expr }
            => compile_paren(state, emitter, ns, *right_paren, expr),
        Expr::Variable { variable }
            => compile_variable(state, emitter, ns, *variable),
        //Expr::Unary { op, rhs }
        //    => compile_unary(state, emitter, ns, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => compile_binary(state, emitter, ns, lhs, *op, rhs),
        //Expr::Logical { lhs, op, rhs }
        //    => compile_logical(state, emitter, ns, lhs, *op, rhs),
        //Expr::Call { callee, paren, arity, args }
        //    => compile_call(state, emitter, ns, callee, *paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(state, emitter, ns, name, args),
        //Expr::Get { object, name }
        //    => compile_get(state, emitter, ns, object, *name),
        //Expr::Index { right_bracket, object, index }
        //    => compile_index(state, emitter, ns, *right_bracket, object, index),
        //Expr::Fn { fn_, args, arity, body, end }
        //    => compile_lambda(state, emitter, ns, *fn_, args, *arity, body, *end),
        _ => todo!("{:?}", expr),
    }
}

fn compile_literal<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    literal: Token<'src>,
) -> Result<(), PiccoloError> {
    trace!("literal {}", literal.format());

    match literal.kind {
        TokenKind::True => emitter.program().push_op(Opcode::Bool(true)),
        TokenKind::False => emitter.program().push_op(Opcode::Bool(false)),
        // TODO make Integer use i64 instead
        TokenKind::Integer(v) if u16::try_from(v).is_ok() => emitter
            .program()
            .push_op(Opcode::Integer(v.try_into().unwrap())),
        TokenKind::Nil => emitter.program().push_op(Opcode::Nil),
        TokenKind::String => {
            let ptr = crate::compiler::maybe_escape_string(&mut state.interner, literal)?;
            emitter.program().push_constant_op(Constant::StringPtr(ptr));
        }
        _ => emitter
            .program()
            .push_constant_op(Constant::try_from(&mut state.interner, literal)?),
    }

    Ok(())
}

fn compile_paren<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    _right_paren: Token<'src>,
    expr: &Expr<'src>,
) -> Result<(), PiccoloError> {
    compile_expr(state, emitter, ns, expr)?;
    Ok(())
}

fn compile_variable<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    variable: Token<'src>,
) -> Result<(), PiccoloError> {
    trace!("variable {}", variable.format());

    match emitter.repo.find(ns, variable) {
        Some(VariableLocation::Builtin) => {
            let func = emitter.builtins[variable.lexeme].clone_func();
            emitter.program().push_func(func);
        }

        Some(VariableLocation::Global) => {
            let ptr = state.interner.get_string_ptr(variable.lexeme).unwrap();
            let index = emitter.program().add_constant(Constant::StringPtr(ptr));
            emitter.program().push_op(Opcode::GetGlobal(index as u16));
        }

        Some(VariableLocation::Local { slot, .. }) => {
            emitter.program().push_op(Opcode::GetLocal(slot as u16));
        }

        Some(VariableLocation::Capture { .. }) => todo!(),
        None => todo!(),
    }

    Ok(())
}

fn compile_binary<'src>(
    state: &mut State,
    emitter: &mut Emitter<'src>,
    ns: DefaultKey,
    lhs: &Expr<'src>,
    op: Token<'src>,
    rhs: &Expr<'src>,
) -> Result<(), PiccoloError> {
    trace!("binary {}", op.format());

    compile_expr(state, emitter, ns, lhs)?;
    compile_expr(state, emitter, ns, rhs)?;

    match op.kind {
        TokenKind::Plus => emitter.program().push_op(Opcode::Add),
        TokenKind::Minus => emitter.program().push_op(Opcode::Subtract),
        TokenKind::Divide => emitter.program().push_op(Opcode::Divide),
        TokenKind::Multiply => emitter.program().push_op(Opcode::Multiply),
        TokenKind::Equal => emitter.program().push_op(Opcode::Equal),
        TokenKind::NotEqual => {
            emitter.program().push_op(Opcode::Equal);
            emitter.program().push_op(Opcode::Not);
        }
        TokenKind::Greater => emitter.program().push_op(Opcode::Greater),
        TokenKind::GreaterEqual => emitter.program().push_op(Opcode::GreaterEqual),
        TokenKind::Less => emitter.program().push_op(Opcode::Less),
        TokenKind::LessEqual => emitter.program().push_op(Opcode::LessEqual),
        TokenKind::Modulo => emitter.program().push_op(Opcode::Modulo),

        TokenKind::ShiftLeft => emitter.program().push_op(Opcode::ShiftLeft),
        TokenKind::ShiftRight => emitter.program().push_op(Opcode::ShiftRight),
        TokenKind::BitwiseAnd => emitter.program().push_op(Opcode::BitAnd),
        TokenKind::BitwiseOr => emitter.program().push_op(Opcode::BitOr),
        TokenKind::BitwiseXor => emitter.program().push_op(Opcode::BitXor),

        _ => unreachable!("binary {:?}", op),
    }

    Ok(())
}
