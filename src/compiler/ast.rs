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

use crate::compiler::Token;

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
        do_: Token<'a>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
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
        do_: Token<'a>,
        then_block: Vec<Stmt<'a>>,
        else_: Option<Token<'a>>,
        else_block: Option<Vec<Stmt<'a>>>,
        end: Token<'a>,
    },
    While {
        while_: Token<'a>,
        cond: Expr<'a>,
        do_: Token<'a>,
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
        do_: Token<'a>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
    ForEach {
        for_: Token<'a>,
        item: Token<'a>,
        iter: Token<'a>,
        do_: Token<'a>,
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
    Return {
        return_: Token<'a>,
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
            Stmt::Block { do_, .. } => *do_,
            Stmt::Assignment { op, .. } => *op,
            Stmt::Declaration { op, .. } => *op,
            Stmt::If { if_, .. } => *if_,
            Stmt::While { while_, .. } => *while_,
            Stmt::For { for_, .. } => *for_,
            Stmt::ForEach { for_, .. } => *for_,
            Stmt::Fn { name, .. } => *name,
            Stmt::Break { break_, .. } => *break_,
            Stmt::Continue { continue_, .. } => *continue_,
            Stmt::Return { return_, .. } => *return_,
            Stmt::Assert { assert, .. } => *assert,
            Stmt::Data { name, .. } => *name,
        }
    }
}
