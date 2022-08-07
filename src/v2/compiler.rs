use crate::{
    compiler::{
        ast::{Expr, Stmt},
        Token, TokenKind,
    },
    error::PiccoloError,
    runtime::{interner::Interner, op::Opcode, value::Constant},
    v2::{Func, Program, State},
};

pub fn compile(ast: &[Stmt]) -> (Program, Interner) {
    let mut program = Program::default();
    let mut interner = Interner::default();
    for stmt in ast {
        compile_stmt(&mut program, &mut interner, stmt).unwrap();
    }
    (program, interner)
}

#[rustfmt::skip]
fn compile_stmt(program: &mut Program, interner: &mut Interner, stmt: &Stmt) -> Result<(), PiccoloError> {
    match stmt {
        Stmt::Expr { token, expr }
            => compile_expr_stmt(program, interner, *token, expr),
        //Stmt::Block { do_, body, end }
        //    => compile_block(program, interner, *do_, body, *end),
        Stmt::Declaration { name, value, .. }
            => compile_declaration(program, interner, *name, value),
        //Stmt::Assignment { lval, op, rval }
        //    => compile_assignment(program, interner, lval, *op, rval),
        //Stmt::If { if_, cond, do_, then_block, else_, else_block, end }
        //    => compile_if(program, interner, *if_, cond, *do_, then_block, else_.as_ref(), else_block.as_ref(), *end),
        //Stmt::While { while_, cond, do_, body, end }
        //    => compile_while(program, interner, *while_, cond, *do_, body, *end),
        //Stmt::For { for_, init, cond, name, inc_op, inc_expr, do_, body, end }
        //    => compile_for(program, interner, *for_, init.as_ref(), cond, *name, *inc_op, inc_expr, *do_, body, *end),
        //Stmt::ForEach { for_, item, iter, do_, body, end }
        //    => compile_for_each(program, interner, *for_, *item, *iter, *do_, body, *end),
        //Stmt::Fn { name, args, arity, body, method, end }
        //    => compile_fn(program, interner, *name, args, *arity, body, *method, *end),
        //Stmt::Break { break_ }
        //    => compile_break(program, *break_),
        //Stmt::Continue { continue_ }
        //    => compile_continue(program, *continue_),
        //Stmt::Return { return_, value }
        //    => compile_return(program, interner, *return_, value.as_ref()),
        //Stmt::Assert { assert, value }
        //    => compile_assert(program, interner, *assert, value),
        //Stmt::Data { name, methods, fields }
        //    => compile_data(program, interner, *name, methods, fields),
        _ => todo!("{:?}", stmt),
    }
}

fn compile_expr_stmt(
    program: &mut Program,
    interner: &mut Interner,
    _token: Token,
    expr: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(program, interner, expr)?;
    program.push_op(Opcode::Pop);
    Ok(())
}

fn compile_declaration(
    program: &mut Program,
    interner: &mut Interner,
    name: Token,
    value: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(program, interner, value)?;

    #[derive(Debug)]
    struct DeclareGlobal {
        // TODO intern this
        name: String,
    }

    impl Func for DeclareGlobal {
        fn call(&mut self, state: &mut State) -> Result<(), PiccoloError> {
            let value = state.pop();
            todo!();
            Ok(())
        }
    }

    program.push_func(DeclareGlobal {
        name: name.lexeme.into(),
    });

    Ok(())
}

#[rustfmt::skip]
fn compile_expr(program: &mut Program, interner: &mut Interner, expr: &Expr) -> Result<(), PiccoloError> {
    match expr {
        Expr::Literal { literal }
            => compile_literal(program, interner, *literal),
        //Expr::ArrayLiteral { right_bracket, values }
        //    => compile_array_literal(program, interner, *right_bracket, values),
        //Expr::Paren { right_paren, expr }
        //    => compile_paren(program, interner, *right_paren, expr),
        Expr::Variable { variable }
            => compile_variable(program, interner, *variable),
        //Expr::Unary { op, rhs }
        //    => compile_unary(program, interner, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => compile_binary(program, interner, lhs, *op, rhs),
        //Expr::Logical { lhs, op, rhs }
        //    => compile_logical(program, interner, lhs, *op, rhs),
        //Expr::Call { callee, paren, arity, args }
        //    => compile_call(program, interner, callee, *paren, *arity, args),
        // Expr::New { name, args }
        //     => compile_new(program, interner, name, args),
        //Expr::Get { object, name }
        //    => compile_get(program, interner, object, *name),
        //Expr::Index { right_bracket, object, index }
        //    => compile_index(program, interner, *right_bracket, object, index),
        //Expr::Fn { fn_, args, arity, body, end }
        //    => compile_lambda(program, interner, *fn_, args, *arity, body, *end),
        _ => todo!("{:?}", expr),
    }
}

fn compile_literal(
    program: &mut Program,
    interner: &mut Interner,
    literal: Token,
) -> Result<(), PiccoloError> {
    match literal.kind {
        TokenKind::True => program.push_op(Opcode::Bool(true)),
        TokenKind::False => program.push_op(Opcode::Bool(false)),
        // TODO make Integer use i64 instead
        TokenKind::Integer(v) if u16::try_from(v).is_ok() => {
            program.push_op(Opcode::Integer(v.try_into().unwrap()))
        }
        TokenKind::Nil => program.push_op(Opcode::Nil),
        TokenKind::String => {
            let ptr = crate::compiler::maybe_escape_string(interner, literal)?;
            program.push_constant(Constant::StringPtr(ptr));
        }
        _ => program.push_constant(Constant::try_from(interner, literal)?),
    }

    Ok(())
}

fn compile_variable(
    program: &mut Program,
    _interner: &mut Interner,
    variable: Token,
) -> Result<(), PiccoloError> {
    // TODO check exists, intern
    #[derive(Debug)]
    struct GetGlobal {
        name: String,
    }

    impl Func for GetGlobal {
        fn call(&mut self, state: &mut State) -> Result<(), PiccoloError> {
            todo!();
            Ok(())
        }
    }

    program.push_func(GetGlobal {
        name: variable.lexeme.into(),
    });

    Ok(())
}

fn compile_binary(
    program: &mut Program,
    interner: &mut Interner,
    lhs: &Expr,
    op: Token,
    rhs: &Expr,
) -> Result<(), PiccoloError> {
    compile_expr(program, interner, lhs)?;
    compile_expr(program, interner, rhs)?;

    match op.kind {
        TokenKind::Plus => program.push_op(Opcode::Add),
        TokenKind::Minus => program.push_op(Opcode::Subtract),
        TokenKind::Divide => program.push_op(Opcode::Divide),
        TokenKind::Multiply => program.push_op(Opcode::Multiply),
        TokenKind::Equal => program.push_op(Opcode::Equal),
        TokenKind::NotEqual => {
            program.push_op(Opcode::Equal);
            program.push_op(Opcode::Not);
        }
        TokenKind::Greater => program.push_op(Opcode::Greater),
        TokenKind::GreaterEqual => program.push_op(Opcode::GreaterEqual),
        TokenKind::Less => program.push_op(Opcode::Less),
        TokenKind::LessEqual => program.push_op(Opcode::LessEqual),
        TokenKind::Modulo => program.push_op(Opcode::Modulo),

        TokenKind::ShiftLeft => program.push_op(Opcode::ShiftLeft),
        TokenKind::ShiftRight => program.push_op(Opcode::ShiftRight),
        TokenKind::BitwiseAnd => program.push_op(Opcode::BitAnd),
        TokenKind::BitwiseOr => program.push_op(Opcode::BitOr),
        TokenKind::BitwiseXor => program.push_op(Opcode::BitXor),

        _ => unreachable!("binary {:?}", op),
    }

    Ok(())
}
