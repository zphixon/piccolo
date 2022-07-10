//! The Piccolo abstract syntax tree.
//!
//! The main Piccolo AST is fit into enums [`Expr`] and [`Stmt`]. `Expr` contains
//! variants for working with Piccolo expressions, and `Stmt` for statements. Generally,
//! `Stmt` AST nodes have some block component to them, like a function, object, or
//! control flow.
//!
//! [`Expr`]: ./enum.Expr.html
//! [`Stmt`]: ./enum.Stmt.html

// https://github.com/Darksecond/lox/blob/master/lox-compiler/src/bettercompiler/statements.rs

use crate::compiler::{Token, TokenKind};
use std::fmt::Write;

/// Simple type alias for the abstract syntax tree.
pub type Ast<'a> = [Stmt<'a>];

/// Piccolo expression AST node.
///
/// This enum contains every expression variant available in Piccolo.
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal {
        literal: Token<'a>,
    },
    ArrayLiteral {
        right_bracket: Token<'a>,
        values: Vec<Expr<'a>>,
    },
    Paren {
        right_paren: Token<'a>,
        expr: Box<Expr<'a>>,
    },
    Path {
        names: Vec<Token<'a>>,
    },
    Variable {
        variable: Token<'a>,
    },
    Unary {
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Binary {
        lhs: Box<Expr<'a>>,
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Logical {
        lhs: Box<Expr<'a>>,
        op: Token<'a>,
        rhs: Box<Expr<'a>>,
    },
    Call {
        callee: Box<Expr<'a>>,
        paren: Token<'a>,
        arity: usize,
        args: Vec<Expr<'a>>,
    },
    New {
        name: Token<'a>,
        args: Vec<(Token<'a>, Box<Expr<'a>>)>,
    },
    Get {
        object: Box<Expr<'a>>,
        name: Token<'a>,
    },
    Index {
        right_bracket: Token<'a>,
        object: Box<Expr<'a>>,
        index: Box<Expr<'a>>,
    },
    Fn {
        fn_: Token<'a>,
        args: Vec<Token<'a>>,
        arity: usize,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
}

impl Expr<'_> {
    pub(crate) fn token(&self) -> Token {
        match self {
            Expr::Literal { literal, .. } => *literal,
            Expr::ArrayLiteral { right_bracket, .. } => *right_bracket,
            Expr::Paren { right_paren, .. } => *right_paren,
            Expr::Path { names, .. } => names[0],
            Expr::Variable { variable, .. } => *variable,
            Expr::Unary { op, .. } => *op,
            Expr::Binary { op, .. } => *op,
            Expr::Logical { op, .. } => *op,
            Expr::Call { paren, .. } => *paren,
            Expr::New { name, .. } => *name,
            Expr::Get { name, .. } => *name,
            Expr::Index { right_bracket, .. } => *right_bracket,
            Expr::Fn { fn_, .. } => *fn_,
        }
    }
}

/// Piccolo statement AST node.
///
/// This enum contains every statement variant available in Piccolo.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr {
        token: Token<'a>,
        expr: Expr<'a>,
    },
    Block {
        end: Token<'a>,
        body: Vec<Stmt<'a>>,
    },
    Assignment {
        lval: Expr<'a>,
        op: Token<'a>,
        rval: Expr<'a>,
    },
    Declaration {
        name: Token<'a>,
        op: Token<'a>,
        value: Expr<'a>,
    },
    If {
        if_: Token<'a>,
        cond: Expr<'a>,
        then_block: Vec<Stmt<'a>>,
        else_: Option<Token<'a>>,
        else_block: Option<Vec<Stmt<'a>>>,
        end: Token<'a>,
    },
    While {
        while_: Token<'a>,
        cond: Expr<'a>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
    For {
        for_: Token<'a>,
        init: Box<Stmt<'a>>,
        cond: Expr<'a>,
        name: Token<'a>,
        inc_op: Token<'a>,
        inc_expr: Expr<'a>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
    ForEach {
        for_: Token<'a>,
        item: Token<'a>,
        iter: Token<'a>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
    Fn {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: usize,
        body: Vec<Stmt<'a>>,
        method: bool,
        end: Token<'a>,
    },
    Break {
        break_: Token<'a>,
        // label: Token<'a>,
    },
    Continue {
        continue_: Token<'a>,
        // label: Token<'a>,
    },
    Retn {
        retn: Token<'a>,
        value: Option<Expr<'a>>,
    },
    Assert {
        assert: Token<'a>,
        value: Expr<'a>,
    },
    Data {
        name: Token<'a>,
        methods: Vec<Stmt<'a>>,
        fields: Vec<Stmt<'a>>,
    },
}

impl Stmt<'_> {
    pub(crate) fn token(&self) -> Token {
        match self {
            Stmt::Expr { token, .. } => *token,
            Stmt::Block { end, .. } => *end,
            Stmt::Assignment { op, .. } => *op,
            Stmt::Declaration { op, .. } => *op,
            Stmt::If { if_, .. } => *if_,
            Stmt::While { while_, .. } => *while_,
            Stmt::For { for_, .. } => *for_,
            Stmt::ForEach { for_, .. } => *for_,
            Stmt::Fn { name, .. } => *name,
            Stmt::Break { break_, .. } => *break_,
            Stmt::Continue { continue_, .. } => *continue_,
            Stmt::Retn { retn, .. } => *retn,
            Stmt::Assert { assert, .. } => *assert,
            Stmt::Data { name, .. } => *name,
        }
    }
}

/// Print an abstract syntax tree.
pub fn print_ast(ast: &Ast) -> String {
    let mut s = String::new();
    for stmt in ast.iter() {
        s.push_str(&print_stmt(0, stmt));
        s.push('\n');
    }
    s
}

/// Print a single expression.
pub fn print_expression(expr: &Expr) -> String {
    print_expr(0, expr)
}

fn parenthesize(indent: usize, name: &str, expressions: &[&Expr]) -> String {
    let mut s = format!("({name}");
    for expr in expressions {
        s.push(' ');
        s.push_str(&print_expr(indent, expr));
    }
    s.push(')');
    s
}

fn parenthesize_list(indent: usize, name: &str, expr: Option<&Expr>, stmts: &[Stmt]) -> String {
    parenthesize_lists(indent, name, expr, &[stmts])
}

fn parenthesize_lists(indent: usize, name: &str, expr: Option<&Expr>, stmts: &[&[Stmt]]) -> String {
    let mut s = format!("({name}");
    if expr.is_some() {
        s.push(' ');
        s.push_str(&print_expr(indent + 1, expr.as_ref().unwrap()));
    }
    for stmt_list in stmts.iter() {
        for stmt in stmt_list.iter() {
            s.push('\n');
            for _ in 0..indent + 1 {
                s.push_str("  ");
            }
            s.push_str(&print_stmt(indent + 1, stmt));
        }
    }
    s.push(')');
    s
}

#[rustfmt::skip]
fn print_stmt(indent: usize, stmt: &Stmt) -> String {
    match stmt {
        Stmt::Expr { expr, .. }
            => print_expr_stmt(indent, expr),
        Stmt::Block { body, .. }
            => print_block(indent, body),
        Stmt::Declaration { name, value, .. }
            => print_declaration(indent, *name, value),
        Stmt::Assignment { lval, rval, .. }
            => print_assignment(indent, lval, rval),
        Stmt::If { cond, then_block, else_block, .. }
            => print_if(indent, cond, then_block, else_block.as_ref()),
        Stmt::While { cond, body, .. }
            => print_while(indent, cond, body),
        Stmt::For { init, cond, name, inc_op, inc_expr, body, .. }
            => print_for(indent, init.as_ref(), cond, *name, *inc_op, inc_expr, body),
        Stmt::ForEach { item, iter, body, .. }
            => print_for_each(indent, *item, *iter, body),
        Stmt::Fn { name, args, body, .. }
            => print_fn(indent, *name, args, body),
        Stmt::Break { .. }
            => print_break(),
        Stmt::Continue { .. }
            => print_continue(),
        Stmt::Retn { retn, value }
            => print_retn(indent, *retn, value.as_ref()),
        Stmt::Assert { value, .. }
            => print_assert(indent, value),
        Stmt::Data { name, methods, fields }
            => print_data(indent, *name, methods, fields),
    }
}

#[rustfmt::skip]
fn print_expr(indent: usize, expr: &Expr) -> String {
    match expr {
        Expr::Literal { literal }
            => print_literal(*literal),
        Expr::ArrayLiteral { values, .. }
            => print_array_literal(indent, values),
        Expr::Paren { expr, .. }
            => print_paren(indent, expr),
        Expr::Path { names }
            => print_path(names),
        Expr::Variable { variable }
            => print_variable(*variable),
        Expr::Unary { op, rhs }
            => print_unary(indent, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => print_binary(indent, lhs, *op, rhs),
        Expr::Logical { lhs, op, rhs }
            => print_logical(indent, lhs, *op, rhs),
        Expr::Call { callee, args, .. }
            => print_call(indent, callee, args),
        Expr::New { name, args }
            => print_new(indent, *name, args),
        Expr::Get { object, name }
            => print_get(indent, object, *name),
        Expr::Index { object, index, .. }
            => print_index(indent, object, index),
        Expr::Fn { args, body, .. }
            => print_fn(indent, Token::identifier("<anon>"), args, body),
    }
}

fn print_expr_stmt(indent: usize, expr: &Expr) -> String {
    parenthesize(indent, "expr", &[expr])
}

fn print_block(indent: usize, body: &[Stmt]) -> String {
    parenthesize_list(indent, "do", None, body)
}

fn print_declaration(indent: usize, name: Token, value: &Expr) -> String {
    parenthesize(indent, &format!("=: {}", name.lexeme), &[value])
}

fn print_assignment(indent: usize, lval: &Expr, rval: &Expr) -> String {
    parenthesize(indent, "=", &[lval, rval])
}

fn print_if(
    indent: usize,
    cond: &Expr,
    then_block: &[Stmt],
    else_block: Option<&Vec<Stmt>>,
) -> String {
    if let Some(else_block) = else_block {
        parenthesize_lists(indent, "if-else", Some(cond), &[then_block, else_block])
    } else {
        parenthesize_list(indent, "if", Some(cond), then_block)
    }
}

fn print_while(indent: usize, cond: &Expr, body: &[Stmt]) -> String {
    parenthesize_list(indent, "while", Some(cond), body)
}

fn print_for(
    indent: usize,
    init: &Stmt,
    cond: &Expr,
    name: Token,
    inc_op: Token,
    inc_expr: &Expr,
    body: &[Stmt],
) -> String {
    let s = format!(
        "for {}, {}, {} {} {}",
        print_stmt(indent, init),
        print_expr(indent, cond),
        name.lexeme,
        inc_op.lexeme,
        print_expr(indent, inc_expr),
    );
    parenthesize_list(indent, &s, None, body)
}

fn print_for_each(indent: usize, item: Token, iter: Token, body: &[Stmt]) -> String {
    let s = format!("for {} in {}", item.lexeme, iter.lexeme,);
    parenthesize_list(indent, &s, None, body)
}

fn print_fn(indent: usize, name: Token, args: &[Token], body: &[Stmt]) -> String {
    let mut s = format!("fn {} (", name.lexeme);
    for (n, arg) in args.iter().enumerate() {
        if n + 1 != args.len() {
            write!(s, "{} ", arg.lexeme).unwrap();
        } else {
            s.push_str(arg.lexeme);
        }
    }
    s.push(')');
    parenthesize_list(indent, &s, None, body)
}

fn print_break() -> String {
    String::from("(break)")
}

fn print_continue() -> String {
    String::from("(continue)")
}

fn print_retn(indent: usize, retn: Token, expr: Option<&Expr>) -> String {
    parenthesize(
        indent,
        "retn",
        &[expr.unwrap_or(&Expr::Literal {
            literal: Token::new(TokenKind::Nil, "nil", retn.pos),
        })],
    )
}

fn print_assert(indent: usize, value: &Expr) -> String {
    parenthesize(indent, "assert", &[value])
}

fn print_data(indent: usize, name: Token, methods: &[Stmt], fields: &[Stmt]) -> String {
    let name = format!("data {}", name.lexeme);
    parenthesize_lists(indent, &name, None, &[fields, methods])
}

fn print_literal(literal: Token) -> String {
    format!("{literal}")
}

fn print_array_literal(indent: usize, values: &[Expr]) -> String {
    let mut s = String::from("[");
    for (i, value) in values.iter().enumerate() {
        s.push_str(&print_expr(indent, value));
        if i + 1 != values.len() {
            s.push_str(", ");
        }
    }
    s.push(']');
    s
}

fn print_paren(indent: usize, expr: &Expr) -> String {
    parenthesize(indent, "paren", &[expr])
}

fn print_path(names: &[Token]) -> String {
    let mut s = String::new();
    for (i, name) in names.iter().enumerate() {
        s.push_str(name.lexeme);
        if i + 1 != names.len() {
            s.push(':');
        }
    }
    s
}

fn print_variable(variable: Token) -> String {
    String::from(variable.lexeme)
}

fn print_unary(indent: usize, op: Token, rhs: &Expr) -> String {
    parenthesize(indent, op.lexeme, &[rhs])
}

fn print_binary(indent: usize, lhs: &Expr, op: Token, rhs: &Expr) -> String {
    parenthesize(indent, op.lexeme, &[lhs, rhs])
}

fn print_logical(indent: usize, lhs: &Expr, op: Token, rhs: &Expr) -> String {
    parenthesize(indent, op.lexeme, &[lhs, rhs])
}

fn print_call(indent: usize, callee: &Expr, args: &[Expr]) -> String {
    let s = format!("call {}", print_expr(indent, callee));
    let args: Vec<&Expr> = args.iter().collect();
    parenthesize(indent, &s, &args)
}

fn print_new(indent: usize, name: Token, args: &[(Token, Box<Expr>)]) -> String {
    let args: Vec<&Expr> = args.iter().map(|tb| tb.1.as_ref()).collect();
    parenthesize(indent, &format!("new {}", name.lexeme), &args)
}

fn print_get(indent: usize, object: &Expr, name: Token) -> String {
    parenthesize(indent, &format!("get {}", name.lexeme), &[object])
}

fn print_index(indent: usize, object: &Expr, index: &Expr) -> String {
    let s = format!("index {}", print_expr(indent, index));
    parenthesize(indent, &s, &[object])
}
