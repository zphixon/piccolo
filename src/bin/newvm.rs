#![allow(dead_code)]

use fnv::FnvHashMap;
use piccolo::{
    compiler::{
        ast::{Expr, Stmt},
        Token, TokenKind,
    },
    error::PiccoloError,
    make_error,
    runtime::{interner::Interner, memory::Heap, value::Value},
};

#[derive(Default, Debug)]
pub struct State {
    heap: Heap,
    interner: Interner,
    stack: Vec<Value>,
    globals: FnvHashMap<String, Value>,
}

impl State {
    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}

pub trait Func: std::fmt::Debug {
    fn call(&mut self, state: State) -> Result<State, PiccoloError>;
}

mod ops {
    use super::*;

    #[derive(Debug)]
    pub struct Add {}
    impl Func for Add {
        fn call(&mut self, mut state: State) -> Result<State, PiccoloError> {
            let rhs = state.pop();
            let lhs = state.pop();
            if lhs.is_double() {
                let lhs = lhs.into::<f64>();
                if rhs.is_double() {
                    let rhs = rhs.into::<f64>();
                    state.stack.push(Value::Double(lhs + rhs));
                } else if rhs.is_integer() {
                    let rhs = rhs.into::<i64>();
                    state.stack.push(Value::Double(lhs + rhs as f64));
                } else {
                    return Err(make_error!(Todo { why: "lol".into() }));
                }
            } else if lhs.is_integer() {
                let lhs = lhs.into::<i64>();
                if rhs.is_integer() {
                    let rhs = rhs.into::<i64>();
                    state.stack.push(Value::Integer(lhs.wrapping_add(rhs)));
                } else if rhs.is_double() {
                    let rhs = rhs.into::<f64>();
                    state.stack.push(Value::Double(lhs as f64 + rhs));
                } else {
                    return Err(make_error!(Todo { why: "lol".into() }));
                }
            } else if lhs.is_string() {
                let value = format!("todo :)");
                state
                    .stack
                    .push(Value::String(state.interner.allocate_string(value)));
            } else {
                return Err(make_error!(Todo { why: "lol".into() }));
            }
            Ok(state)
        }
    }

    #[derive(Debug)]
    pub struct Pop {}
    impl Func for Pop {
        fn call(&mut self, mut state: State) -> Result<State, PiccoloError> {
            state.pop();
            Ok(state)
        }
    }

    #[derive(Debug)]
    pub struct Lit {
        pub literal: Value,
    }

    impl Func for Lit {
        fn call(&mut self, mut state: State) -> Result<State, PiccoloError> {
            state.stack.push(self.literal);
            Ok(state)
        }
    }
}

#[derive(Default, Debug)]
pub struct Program {
    pub funcs: Vec<Box<dyn Func>>,
}

impl Program {
    pub fn run(&mut self) -> Result<State, PiccoloError> {
        let mut state = State::default();

        for next in self.funcs.iter_mut() {
            println!("{state:#?}");
            state = next.call(state)?;
        }

        Ok(state)
    }

    pub fn push<Op: Func + 'static>(&mut self, op: Op) {
        self.funcs.push(Box::new(op));
    }
}

fn compile(ast: &[Stmt]) -> (Program, Interner) {
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
    program.push(ops::Pop {});
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
        fn call(&mut self, mut state: State) -> Result<State, PiccoloError> {
            let value = state.pop();
            state.globals.insert(self.name.clone(), value);
            Ok(state)
        }
    }

    program.push(DeclareGlobal {
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
    _interner: &mut Interner,
    literal: Token,
) -> Result<(), PiccoloError> {
    match literal.kind {
        TokenKind::Integer(int) => program.push(ops::Lit {
            literal: Value::Integer(int),
        }),
        _ => todo!(),
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
        fn call(&mut self, mut state: State) -> Result<State, PiccoloError> {
            let value = state.globals[&self.name];
            state.stack.push(value);
            Ok(state)
        }
    }

    program.push(GetGlobal {
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
    match op.kind {
        TokenKind::Plus => {
            compile_expr(program, interner, lhs)?;
            compile_expr(program, interner, rhs)?;
            program.push(ops::Add {});
        }
        _ => todo!(),
    }

    Ok(())
}

fn main() {
    let ast = piccolo::compiler::parser::parse("a =: 1 + 2 b =: a + 3").unwrap();
    println!("{ast:#?}");

    let (mut program, _) = compile(&ast);
    println!("{program:#?}");

    let state = program.run().unwrap();
    println!("{state:#?}");
}
