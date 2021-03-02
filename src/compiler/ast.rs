//! Contains types and traits useful for working with a Piccolo AST.
//!
//! The main Piccolo AST is fit into enums [`Expr`] and [`Stmt`]. `Expr` contains
//! variants for working with Piccolo expressions, and `Stmt` for statements. Generally,
//! `Stmt` AST nodes have some block component to them, like a function, object, or
//! control flow.
//!
//! [`ExprVisitor`] and [`StmtVisitor`] are traits for implementing the [visitor]
//! pattern. They allow a struct implementing them to walk the AST to perform some
//! operation without modifying it. Piccolo's bytecode [`Emitter`] implements these
//! traits to compile an AST into a [`Chunk`].
//!
//! [`Expr`]: ./enum.Expr.html
//! [`Stmt`]: ./enum.Stmt.html
//! [`ExprVisitor`]: ./trait.ExprVisitor.html
//! [`StmtVisitor`]: ./trait.StmtVisitor.html
//! [visitor]: https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md
//! [`Emitter`]: ../emitter/struct.Emitter.html
//! [`Chunk`]: ../runtime/struct.Chunk.html

// https://github.com/Darksecond/lox/blob/master/lox-compiler/src/bettercompiler/statements.rs

use crate::{Token, TokenKind};

/// Pretty-prints a Piccolo AST using the ExprVisitor and StmtVisitor traits.
#[derive(Copy, Clone)]
pub struct AstPrinter {
    indent: usize,
}

impl AstPrinter {
    /// Pretty-print the full AST.
    pub fn print(ast: &[Stmt]) -> String {
        let mut s = String::new();
        for stmt in ast.iter() {
            s.push_str(&stmt.accept(&mut AstPrinter { indent: 0 }));
            s.push_str("\n");
        }
        s
    }

    /// Pretty-print a single expression.
    pub fn print_expr(expr: &Expr) -> String {
        expr.accept(&mut AstPrinter { indent: 0 })
    }

    /// Pretty-print a single statement.
    pub fn print_stmt(stmt: &Stmt) -> String {
        stmt.accept(&mut AstPrinter { indent: 0 })
    }

    fn parenthesize(&mut self, name: &str, expressions: &[&Expr]) -> String {
        let mut s = format!("({}", name);
        for expr in expressions {
            s.push_str(" ");
            s.push_str(&expr.accept(self));
        }
        s.push_str(")");
        s
    }

    fn parenthesize_list(&mut self, name: &str, expr: Option<&Expr>, stmts: &[Stmt]) -> String {
        self.parenthesize_lists(name, expr, &[stmts])
    }

    fn parenthesize_lists(&mut self, name: &str, expr: Option<&Expr>, stmts: &[&[Stmt]]) -> String {
        self.indent += 1;
        let mut s = format!("({}", name);
        if expr.is_some() {
            s.push_str(" ");
            s.push_str(&expr.as_ref().unwrap().accept(self));
        }
        for stmt_list in stmts.iter() {
            for stmt in stmt_list.iter() {
                s.push_str("\n");
                for _ in 0..self.indent {
                    s.push_str("  ");
                }
                s.push_str(&stmt.accept(self));
            }
        }
        s.push_str(")");
        self.indent -= 1;
        s
    }
}

impl StmtVisitor for AstPrinter {
    type Output = String;

    fn visit_expr(&mut self, _token: &Token, expr: &Expr) -> Self::Output {
        self.parenthesize("expr", &[&expr])
    }

    fn visit_block(&mut self, _do_: &Token, body: &[Stmt]) -> Self::Output {
        self.parenthesize_list("do", None, body)
    }

    fn visit_declaration(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output {
        self.parenthesize(&format!("{} {}", op.lexeme, name.lexeme), &[value])
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output {
        self.parenthesize(&format!("{} {}", op.lexeme, name.lexeme), &[value])
    }

    fn visit_if(
        &mut self,
        _if_: &Token,
        cond: &Expr,
        then_block: &[Stmt],
        _else_: Option<&Token>,
        else_block: Option<&Vec<Stmt>>,
        _end: &Token,
    ) -> Self::Output {
        if let Some(else_block) = else_block {
            self.parenthesize_lists("if-else", Some(cond), &[then_block, else_block])
        } else {
            self.parenthesize_list("if", Some(cond), then_block)
        }
    }

    fn visit_while(
        &mut self,
        _while_: &Token,
        cond: &Expr,
        body: &[Stmt],
        _end: &Token,
    ) -> Self::Output {
        self.parenthesize_list("while", Some(cond), body)
    }

    fn visit_for(
        &mut self,
        _for: &Token,
        init: &Stmt,
        cond: &Expr,
        inc: &Stmt,
        body: &[Stmt],
        _end: &Token,
    ) -> Self::Output {
        let s = format!(
            "for {}, {}, {}",
            init.accept(self),
            cond.accept(self),
            inc.accept(self)
        );
        self.parenthesize_list(&s, None, body)
    }

    fn visit_fn(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: usize,
        body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        let mut s = format!("fn {} (", name.lexeme);
        for (n, arg) in args.iter().enumerate() {
            if n + 1 != args.len() {
                s.push_str(&format!("{} ", arg.lexeme));
            } else {
                s.push_str(arg.lexeme);
            }
        }
        s.push_str(")");
        self.parenthesize_list(&s, None, body)
    }

    fn visit_break(&mut self, _break: &Token) -> Self::Output {
        String::from("(break)")
    }

    fn visit_continue(&mut self, _continue: &Token) -> Self::Output {
        String::from("(continue)")
    }

    fn visit_retn(&mut self, retn: &Token, value: Option<&Expr>) -> Self::Output {
        self.parenthesize(
            "retn",
            &[value.unwrap_or(&Expr::Literal {
                literal: Token::new(TokenKind::Nil, "nil", retn.line),
            })],
        )
    }

    fn visit_assert(&mut self, _keyword: &Token, value: &Expr) -> Self::Output {
        self.parenthesize("assert", &[value])
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> Self::Output {
        // TODO
        self.parenthesize("data", &[])
    }
}

impl ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_literal(&mut self, literal: &Token) -> Self::Output {
        format!("{}", literal)
    }

    fn visit_paren(&mut self, _right_paren: &Token, expr: &Expr) -> Self::Output {
        self.parenthesize("paren", &[expr])
    }

    fn visit_variable(&mut self, variable: &Token) -> Self::Output {
        String::from(variable.lexeme)
    }

    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> Self::Output {
        self.parenthesize(op.lexeme, &[rhs])
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        _paren: &Token,
        _arity: usize,
        args: &[Expr],
    ) -> Self::Output {
        let s = format!("call {}", callee.accept(self));
        let args: Vec<&Expr> = args.iter().map(|arg| arg).collect();
        self.parenthesize(&s, &args)
    }

    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> Self::Output {
        let args: Vec<&Expr> = args.iter().map(|tb| tb.1.as_ref()).collect();
        self.parenthesize(&format!("new {}", name.lexeme), &args)
    }

    fn visit_get(&mut self, object: &Expr, name: &Token) -> Self::Output {
        self.parenthesize(&format!("get {}", name.lexeme), &[object])
    }

    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Self::Output {
        let s = format!("set {} = {}", name.lexeme, value.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_index(&mut self, _right_bracket: &Token, object: &Expr, idx: &Expr) -> Self::Output {
        let s = format!("index {}", idx.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_fn(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: usize,
        body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        let mut s = format!("fn {} (", name.lexeme);
        for (n, arg) in args.iter().enumerate() {
            if n + 1 != args.len() {
                s.push_str(&format!("{} ", arg.lexeme));
            } else {
                s.push_str(arg.lexeme);
            }
        }
        s.push_str(")");
        self.parenthesize_list(&s, None, body)
    }
}

/// Trait for an expression to accept an [`ExprVisitor`].
///
/// Only [`Expr`] will implement this trait, you probably want [`ExprVisitor`].
///
/// [`ExprVisitor`]: ./trait.ExprVisitor.html
/// [`Expr`]: ./enum.Expr.html
pub trait ExprAccept {
    /// De-structures the [`Expr`] variant into its parts, and calls the corresponding
    /// method on [`ExprVisitor`].
    ///
    /// [`Expr`]: ./enum.Expr.html
    /// [`ExprVisitor`]: ./trait.ExprVisitor.html
    fn accept<T: ExprVisitor>(&self, visitor: &mut T) -> T::Output;
}

/// One of two traits used to walk a Piccolo AST.
///
/// An implementor of this trait will specify some output which is folded over
/// recursively. To continue walking the AST, the implementor will need to call
/// [`Expr::accept`] with itself as the argument, which will return to the
/// `ExprVisitor` implementation.
///
/// [`Expr::accept`]: ./enum.Expr.html
pub trait ExprVisitor {
    /// Type which the visitor folds over.
    type Output;

    /// Visit Piccolo value literals, like `nil`, bools, strings, integers, and
    /// floating point numbers.
    fn visit_literal(&mut self, literal: &Token) -> Self::Output;

    /// Visit parenthesized expressions.
    fn visit_paren(&mut self, right_paren: &Token, expr: &Expr) -> Self::Output;

    /// Visit variable references.
    fn visit_variable(&mut self, variable: &Token) -> Self::Output;

    /// Visit unary operator expressions, like `!` or `-`.
    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> Self::Output;

    /// Visit binary operator expressions.
    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output;

    /// Visit logical operator expressions, like `&&` and `||`.
    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output;

    /// Visit function call expressions.
    fn visit_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        arity: usize,
        args: &[Expr],
    ) -> Self::Output;

    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> Self::Output;
    fn visit_get(&mut self, object: &Expr, name: &Token) -> Self::Output;
    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Self::Output;
    fn visit_index(&mut self, right_bracket: &Token, object: &Expr, idx: &Expr) -> Self::Output;
    fn visit_fn(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: usize,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;
}

/// Piccolo expression AST node.
///
/// This enum contains every expression variant available in Piccolo.
/// Generally, if one expression could be built in terms of another,
/// for example `Binary` and `Logical`, the behavior for an implementor
/// of [`ExprVisitor`] should reflect a difference between the two.
///
/// [`ExprVisitor`]: ./trait.ExprVisitor.html
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Literal {
        literal: Token<'a>,
    },
    Paren {
        right_paren: Token<'a>,
        expr: Box<Expr<'a>>,
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
    Set {
        object: Box<Expr<'a>>,
        name: Token<'a>,
        value: Box<Expr<'a>>,
    },
    Index {
        right_bracket: Token<'a>,
        object: Box<Expr<'a>>,
        idx: Box<Expr<'a>>,
    },
    Func {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: usize,
        body: Vec<Stmt<'a>>,
        method: bool,
    },
}

#[rustfmt::skip]
impl ExprAccept for Expr<'_> {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Expr::Literal { literal }
                => v.visit_literal(literal),
            Expr::Paren { right_paren, expr }
                => v.visit_paren(right_paren, expr),
            Expr::Variable { variable }
                => v.visit_variable(variable),
            Expr::Unary { op, rhs }
                => v.visit_unary(op, rhs),
            Expr::Binary { lhs, op, rhs }
                => v.visit_binary(lhs, op, rhs),
            Expr::Logical { lhs, op, rhs }
                => v.visit_logical(lhs, op, rhs),
            Expr::Call { callee, paren, arity, args }
                => v.visit_call(callee, paren, *arity, args),
            Expr::New { name, args }
                => v.visit_new(name, args),
            Expr::Get { object, name }
                => v.visit_get(object, name),
            Expr::Set { object, name, value }
                => v.visit_set(object, name, value),
            Expr::Index { right_bracket, object, idx }
                => v.visit_index(right_bracket, object, idx),
            Expr::Func { name, args, arity, body, method }
                => v.visit_fn(name, args, *arity, body, *method),
        }
    }
}

/// Trait for a statement to accept an [`StmtVisitor`].
///
/// Only [`Stmt`] will implement this trait, you probably want [`StmtVisitor`].
///
/// [`StmtVisitor`]: ./trait.StmtVisitor.html
/// [`Stmt`]: ./enum.Stmt.html
pub trait StmtAccept {
    /// De-structures the [`Stmt`] variant into its parts, and calls the corresponding
    /// method on [`StmtVisitor`].
    ///
    /// [`Stmt`]: ./enum.Stmt.html
    /// [`StmtVisitor`]: ./trait.StmtVisitor.html
    fn accept<T: StmtVisitor>(&self, visitor: &mut T) -> T::Output;
}

/// One of two traits used to walk a Piccolo AST.
///
/// An implementor of this trait will specify some output which is folded over
/// recursively. To continue walking the AST, the implementor will need to call
/// [`Stmt::accept`] with itself as the argument, which will return to the
/// `StmtVisitor` implementation.
///
/// [`Stmt::accept`]: ./enum.Stmt.html
pub trait StmtVisitor {
    /// Type which the visitor folds over.
    type Output;

    /// Visit an expression statement, like function calls, method calls, or property sets.
    fn visit_expr(&mut self, token: &Token, expr: &Expr) -> Self::Output;

    /// Visit a bare block.
    fn visit_block(&mut self, end: &Token, body: &[Stmt]) -> Self::Output;

    /// Visit a variable declaration.
    fn visit_declaration(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output;

    /// Visit a variable assignment.
    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output;

    /// Visit an `if-then` statement.
    #[allow(clippy::too_many_arguments)]
    fn visit_if(
        &mut self,
        if_: &Token,
        cond: &Expr,
        then_block: &[Stmt],
        else_: Option<&Token>,
        else_block: Option<&Vec<Stmt>>,
        end: &Token,
    ) -> Self::Output;

    /// Visit a `while` loop.
    fn visit_while(
        &mut self,
        while_: &Token,
        cond: &Expr,
        body: &[Stmt],
        end: &Token,
    ) -> Self::Output;

    /// Visit a `for` loop.
    fn visit_for(
        &mut self,
        for_: &Token,
        init: &Stmt,
        cond: &Expr,
        inc: &Stmt,
        body: &[Stmt],
        end: &Token,
    ) -> Self::Output;

    fn visit_fn(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: usize,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;

    fn visit_break(&mut self, break_: &Token) -> Self::Output;

    fn visit_continue(&mut self, continue_: &Token) -> Self::Output;

    fn visit_retn(&mut self, retn: &Token, value: Option<&Expr>) -> Self::Output;
    fn visit_assert(&mut self, assert: &Token, value: &Expr) -> Self::Output;
    fn visit_data(
        &mut self,
        name: &Token,
        methods: &[Stmt],
        fields: &[(Token, Expr)],
    ) -> Self::Output;
}

/// Piccolo statement AST node.
///
/// This enum contains every statement variant available in Piccolo. Generally,
/// if one statement could be built in terms of another, for example `Assignment`
/// and `Declaration`, the behavior for an implementor of [`StmtVisitor`] should
/// reflect a difference between the two.
///
/// [`StmtVisitor`]: ./trait.StmtVisitor.html
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
        name: Token<'a>,
        op: Token<'a>,
        value: Expr<'a>,
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
        inc: Box<Stmt<'a>>,
        body: Vec<Stmt<'a>>,
        end: Token<'a>,
    },
    Fn {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: usize,
        body: Vec<Stmt<'a>>,
        method: bool,
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
        fields: Vec<(Token<'a>, Expr<'a>)>,
    },
}

#[rustfmt::skip]
impl StmtAccept for Stmt<'_> {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Stmt::Expr { token, expr }
                => v.visit_expr(token, expr),
            Stmt::Block { end, body }
                => v.visit_block(end, body),
            Stmt::Declaration { name, op, value }
                => v.visit_declaration(name, op, value),
            Stmt::Assignment { name, op, value }
                => v.visit_assignment(name, op, value),
            Stmt::If { if_, cond, then_block, else_, else_block, end }
                => v.visit_if(if_, cond, then_block, else_.as_ref(), else_block.as_ref(), end),
            Stmt::While { while_, cond, body, end }
                => v.visit_while(while_, cond, body, end),
            Stmt::For { for_, init, cond, inc, body, end }
                => v.visit_for(for_, init.as_ref(), cond, inc.as_ref(), body, end),
            Stmt::Fn { name, args, arity, body, method }
                => v.visit_fn(name, args, *arity, body, *method),
            Stmt::Break { break_ }
                => v.visit_break(break_),
            Stmt::Continue { continue_ }
                => v.visit_continue(continue_),
            Stmt::Retn { retn, value }
                => v.visit_retn(retn, value.as_ref()),
            Stmt::Assert { assert, value }
                => v.visit_assert(assert, value),
            Stmt::Data { name, methods, fields }
                => v.visit_data(name, methods, fields),
        }
    }
}
