use crate::{
    compiler::{
        ast::{Expr, Stmt},
        Token, TokenKind,
    },
    runtime::{
        chunk::{Chunk, Module},
        interner::Interner,
        op::Opcode,
    },
    trace,
};
use std::fmt::Write;
use tcolor::{Color, ColorString};

pub fn disassemble(interner: &Interner, module: &Module, name: &str) -> String {
    trace!("disassemble");

    let mut s = format!(" -- {name} --\n");
    s.push_str(" ++ constants\n");
    for (index, constant) in module.constants().iter().enumerate() {
        writeln!(s, "{index:04x} {}", constant.debug(interner)).unwrap();
    }

    for (i, chunk) in module.chunks().iter().enumerate() {
        writeln!(s, " ++ chunk {i}").unwrap();
        let mut offset = 0;
        while offset < chunk.ops.len() {
            s.push_str(&disassemble_instruction(interner, module, chunk, offset));
            s.push('\n');
            offset += 1;
        }
    }

    s
}

pub fn color_disassemble(interner: &Interner, module: &Module, name: &str) -> ColorString {
    trace!("disassemble");

    let mut s = ColorString::new_fg(format!(" -- {name} --\n"), Color::BrightWhite);
    s.push_string(" ++ constants\n", Color::Green);
    for (index, constant) in module.constants().iter().enumerate() {
        s.push_string(format!("{index:04x} "), Color::BrightMagenta);
        s.push(constant.color_format(interner));
        s.push_string("\n", Color::BrightWhite);
    }

    for (i, chunk) in module.chunks().iter().enumerate() {
        s.push_string(format!(" ++ chunk {i}\n"), Color::Green);
        let mut offset = 0;
        while offset < chunk.ops.len() {
            s.push(color_disassemble_instruction(
                interner, module, chunk, offset,
            ));
            s.push_string("\n", Color::None);
            offset += 1;
        }
    }

    s
}

pub fn disassemble_instruction(
    interner: &Interner,
    module: &Module,
    chunk: &Chunk,
    offset: usize,
) -> String {
    let op = chunk.ops[offset];

    let arg = match op {
        Opcode::Constant(index) => {
            format!("@{index:04x} ({})", {
                module.get_constant(index).debug(interner)
            })
        }
        Opcode::GetLocal(index) | Opcode::SetLocal(index) => {
            format!("${index}")
        }
        Opcode::GetGlobal(index) | Opcode::SetGlobal(index) | Opcode::DeclareGlobal(index) => {
            format!("g{index:04x} ({})", {
                module.get_constant(index).debug(interner)
            })
        }
        Opcode::JumpForward(jump) | Opcode::JumpFalse(jump) | Opcode::JumpTrue(jump) => {
            format!("+{jump:04x} -> {:04x}", offset + jump as usize)
        }
        Opcode::JumpBack(jump) => {
            format!("-{jump:04x} -> {:04x}", offset - jump as usize)
        }
        _ => String::new(),
    };

    format!(
        "{:<6} {offset:04x} {:20} {arg}",
        format!("{}", chunk.get_pos_from_index(offset)),
        format!("{op:?}")
    )
}

pub fn color_disassemble_instruction(
    interner: &Interner,
    module: &Module,
    chunk: &Chunk,
    offset: usize,
) -> ColorString {
    let op = chunk.ops[offset];
    let mut arg = ColorString::new();

    match op {
        Opcode::Constant(index) => {
            arg.push_string(format!("@{index:04x} "), Color::BrightMagenta);
            arg.push(module.get_constant(index).color_format(interner));
        }
        Opcode::GetLocal(index) | Opcode::SetLocal(index) => {
            arg.push_string(format!("${index}"), Color::BrightBlue);
        }
        Opcode::GetGlobal(index) | Opcode::SetGlobal(index) | Opcode::DeclareGlobal(index) => {
            arg.push_string(format!("g{index:04x} ("), Color::BrightBlue);
            arg.push(module.get_constant(index).color_format(interner));
            arg.push_string(")", Color::BrightBlue);
        }
        Opcode::JumpForward(jump) | Opcode::JumpFalse(jump) | Opcode::JumpTrue(jump) => {
            arg.push_string(format!("+{jump:04x} -> "), Color::Red);
            arg.push_string(
                format!("{:04x}", offset + jump as usize),
                Color::BrightWhite,
            );
        }
        Opcode::JumpBack(jump) => {
            arg.push_string(format!("-{jump:04x} -> "), Color::Red);
            arg.push_string(
                format!("{:04x}", offset - jump as usize),
                Color::BrightWhite,
            );
        }
        _ => {}
    };

    let mut s = ColorString::new_fg(
        format!("{:<6} ", format!("{}", chunk.get_pos_from_index(offset))),
        Color::BrightGreen,
    );
    s.push_string(format!("{offset:04x} "), Color::BrightWhite);
    s.push_string(format!("{:20} ", format!("{op:?}")), Color::Red);
    s.push(arg);
    s
}

/// Print an abstract syntax tree.
pub fn print_ast(ast: &[Stmt]) -> String {
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
        Stmt::Return { return_, value }
            => print_return(indent, *return_, value.as_ref()),
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

fn print_return(indent: usize, return_: Token, expr: Option<&Expr>) -> String {
    parenthesize(
        indent,
        "return",
        &[expr.unwrap_or(&Expr::Literal {
            literal: Token::new(TokenKind::Nil, "nil", return_.pos),
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
