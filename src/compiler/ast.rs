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

use crate::{Token, TokenKind};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Arity {
    None,
    Multi,
    Some(usize),
}

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

    fn visit_expr(&mut self, expr: &Expr) -> String {
        self.parenthesize("expr", &[&expr])
    }

    fn visit_block(&mut self, _do_: &Token, body: &[Stmt]) -> String {
        self.parenthesize_list("do", None, body)
    }

    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> String {
        self.parenthesize(&format!("{} {}", op.lexeme, name.lexeme), &[value])
    }

    fn visit_if(&mut self, cond: &Expr, then: &[Stmt], else_: Option<&Vec<Stmt>>) -> String {
        if let Some(else_) = else_ {
            self.parenthesize_lists("if-else", Some(cond), &[then, else_])
        } else {
            self.parenthesize_list("if", Some(cond), then)
        }
    }

    fn visit_while(&mut self, cond: &Expr, body: &[Stmt]) -> String {
        self.parenthesize_list("while", Some(cond), body)
    }

    fn visit_for(&mut self, name: &Token, iter: &Expr, body: &[Stmt]) -> String {
        self.parenthesize_list(&format!("for {} in ", name.lexeme), Some(iter), body)
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: Arity,
        body: &[Stmt],
        _method: bool,
    ) -> String {
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

    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> String {
        self.parenthesize(
            "retn",
            &[value.unwrap_or(&Expr::Atom(Token::new(TokenKind::Nil, "nil", keyword.line)))],
        )
    }

    fn visit_assert(&mut self, _keyword: &Token, value: &Expr) -> String {
        self.parenthesize("assert", &[value])
    }

    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> String {
        // TODO
        self.parenthesize("data", &[])
    }
}

impl ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_atom(&mut self, token: &Token) -> String {
        format!("{}", token)
    }

    fn visit_paren(&mut self, value: &Expr) -> String {
        self.parenthesize("paren", &[value])
    }

    fn visit_variable(&mut self, name: &Token) -> String {
        String::from(name.lexeme)
    }

    fn visit_unary(&mut self, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[rhs])
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_logical(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> String {
        self.parenthesize(op.lexeme, &[lhs, rhs])
    }

    fn visit_call(
        &mut self,
        callee: &Expr,
        _paren: &Token,
        _arity: Arity,
        args: &[Expr],
    ) -> String {
        let s = format!("call {}", callee.accept(self));
        let args: Vec<&Expr> = args.iter().map(|arg| arg).collect();
        self.parenthesize(&s, &args)
    }

    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> String {
        let args: Vec<&Expr> = args.iter().map(|tb| tb.1.as_ref()).collect();
        self.parenthesize(&format!("new {}", name.lexeme), &args)
    }

    fn visit_get(&mut self, object: &Expr, name: &Token) -> String {
        self.parenthesize(&format!("get {}", name.lexeme), &[object])
    }

    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> String {
        let s = format!("set {} = {}", name.lexeme, value.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_index(&mut self, _rb: &Token, object: &Expr, idx: &Expr) -> String {
        let s = format!("index {}", idx.accept(self));
        self.parenthesize(&s, &[object])
    }

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        _arity: Arity,
        body: &[Stmt],
        _method: bool,
    ) -> String {
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
    fn visit_atom(&mut self, token: &Token) -> Self::Output;

    /// Visit parenthesized expressions.
    fn visit_paren(&mut self, value: &Expr) -> Self::Output;

    /// Visit variable references.
    fn visit_variable(&mut self, name: &Token) -> Self::Output;

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
        arity: Arity,
        args: &[Expr],
    ) -> Self::Output;

    fn visit_new(&mut self, name: &Token, args: &[(Token, Box<Expr>)]) -> Self::Output;
    fn visit_get(&mut self, object: &Expr, name: &Token) -> Self::Output;
    fn visit_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Self::Output;
    fn visit_index(&mut self, rb: &Token, object: &Expr, idx: &Expr) -> Self::Output;
    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: Arity,
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
    Atom(Token<'a>),
    Paren(Box<Expr<'a>>),
    Variable(Token<'a>),
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
        arity: Arity,
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
        rb: Token<'a>,
        object: Box<Expr<'a>>,
        idx: Box<Expr<'a>>,
    },
    Func {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: Arity,
        body: Vec<Stmt<'a>>,
        method: bool,
    },
}

#[rustfmt::skip]
impl ExprAccept for Expr<'_> {
    fn accept<T: ExprVisitor>(&self, v: &mut T) -> T::Output {
        match self {
            Expr::Atom(token)
                => v.visit_atom(token),
            Expr::Paren(value)
                => v.visit_paren(value),
            Expr::Variable(name)
                => v.visit_variable(name),
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
            Expr::Index { rb, object, idx }
                => v.visit_index(rb, object, idx),
            Expr::Func { name, args, arity, body, method }
                => v.visit_func(name, args, *arity, body, *method),
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
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output;

    /// Visit a bare block.
    fn visit_block(&mut self, end: &Token, body: &[Stmt]) -> Self::Output;

    /// Visit a variable assignment or declaration.
    fn visit_assignment(&mut self, name: &Token, op: &Token, value: &Expr) -> Self::Output;

    /// Visit an `if-then` statement.
    fn visit_if(&mut self, cond: &Expr, then: &[Stmt], else_: Option<&Vec<Stmt>>) -> Self::Output;

    /// Visit a `while` loop.
    fn visit_while(&mut self, cond: &Expr, body: &[Stmt]) -> Self::Output;

    /// Visit a `for` loop.
    fn visit_for(&mut self, name: &Token, iter: &Expr, body: &[Stmt]) -> Self::Output;

    fn visit_func(
        &mut self,
        name: &Token,
        args: &[Token],
        arity: Arity,
        body: &[Stmt],
        method: bool,
    ) -> Self::Output;
    fn visit_retn(&mut self, keyword: &Token, value: Option<&Expr>) -> Self::Output;
    fn visit_assert(&mut self, keyword: &Token, value: &Expr) -> Self::Output;
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
#[derive(Debug, PartialEq)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Block {
        end: Token<'a>,
        body: Vec<Stmt<'a>>,
    },
    Assignment {
        name: Token<'a>,
        op: Token<'a>,
        value: Expr<'a>,
    },
    If {
        cond: Expr<'a>,
        then: Vec<Stmt<'a>>,
        else_: Option<Vec<Stmt<'a>>>,
    },
    While {
        cond: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    For {
        name: Token<'a>,
        iter: Expr<'a>,
        body: Vec<Stmt<'a>>,
    },
    Func {
        name: Token<'a>,
        args: Vec<Token<'a>>,
        arity: Arity,
        body: Vec<Stmt<'a>>,
        method: bool,
    },
    Retn {
        keyword: Token<'a>,
        value: Option<Expr<'a>>,
    },
    Assert {
        keyword: Token<'a>,
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
            Stmt::Expr(expr)
                => v.visit_expr(expr),
            Stmt::Block { end, body }
                => v.visit_block(end, body),
            Stmt::Assignment { name, op, value }
                => v.visit_assignment(name, op, value),
            Stmt::If { cond, then, else_ }
                => v.visit_if(cond, then, else_.as_ref()),
            Stmt::While { cond, body }
                => v.visit_while(cond, body),
            Stmt::For { name, iter, body }
                => v.visit_for(name, iter, body),
            Stmt::Func { name, args, arity, body, method }
                => v.visit_func(name, args, *arity, body, *method),
            Stmt::Retn { keyword, value }
                => v.visit_retn(keyword, value.as_ref()),
            Stmt::Assert { keyword, value }
                => v.visit_assert(keyword, value),
            Stmt::Data { name, methods, fields }
                => v.visit_data(name, methods, fields),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{Token, TokenKind};

    use super::{Arity, AstPrinter, Expr, Stmt};

    #[test]
    fn ast_print() {
        let source = "x =: 3\ny =: x - 4\nif x > y then\n  io.prln(3)\nend\n";
        let mut tokens = vec![
            Some(Token::new(TokenKind::Identifier, &source[0..1], 1)),
            Some(Token::new(TokenKind::Declare, &source[2..4], 1)),
            Some(Token::new(TokenKind::Integer(3), &source[5..6], 1)),
            Some(Token::new(TokenKind::Identifier, &source[7..8], 2)),
            Some(Token::new(TokenKind::Declare, &source[9..11], 2)),
            Some(Token::new(TokenKind::Identifier, &source[12..13], 2)),
            Some(Token::new(TokenKind::Minus, &source[14..15], 2)),
            Some(Token::new(TokenKind::Integer(3), &source[16..17], 2)),
            Some(Token::new(TokenKind::If, &source[18..19], 3)),
            Some(Token::new(TokenKind::Identifier, &source[21..22], 3)),
            Some(Token::new(TokenKind::Greater, &source[23..24], 3)),
            Some(Token::new(TokenKind::Identifier, &source[25..26], 3)),
            Some(Token::new(TokenKind::Nil, &source[27..31], 3)),
            Some(Token::new(TokenKind::Identifier, &source[34..36], 4)),
            Some(Token::new(TokenKind::Period, &source[36..37], 4)),
            Some(Token::new(TokenKind::Identifier, &source[37..41], 4)),
            Some(Token::new(TokenKind::LeftParen, &source[41..42], 4)),
            Some(Token::new(TokenKind::Integer(3), &source[42..43], 4)),
            Some(Token::new(TokenKind::RightParen, &source[43..44], 4)),
            Some(Token::new(TokenKind::End, &source[45..48], 5)),
        ];

        let ast = vec![
            Stmt::Assignment {
                name: tokens[0].take().unwrap(),
                op: tokens[1].take().unwrap(),
                value: Expr::Atom(Token::new(TokenKind::Integer(3), "3", 1)),
            },
            Stmt::Assignment {
                name: tokens[3].take().unwrap(),
                op: tokens[4].take().unwrap(),
                value: Expr::Binary {
                    lhs: Box::new(Expr::Variable(tokens[5].take().unwrap())),
                    op: tokens[6].take().unwrap(),
                    rhs: Box::new(Expr::Atom(Token::new(TokenKind::Integer(4), "4", 2))),
                },
            },
            Stmt::If {
                cond: Expr::Binary {
                    lhs: Box::new(Expr::Variable(tokens[9].take().unwrap())),
                    op: tokens[10].take().unwrap(),
                    rhs: Box::new(Expr::Variable(tokens[11].take().unwrap())),
                },
                then: vec![Stmt::Expr(Expr::Call {
                    callee: Box::new(Expr::Get {
                        object: Box::new(Expr::Variable(tokens[13].take().unwrap())),
                        name: tokens[15].take().unwrap(),
                    }),
                    paren: tokens[16].take().unwrap(),
                    arity: Arity::Some(1),
                    args: vec![Expr::Atom(Token::new(TokenKind::Integer(3), "3", 4))],
                })],
                else_: None,
            },
        ];

        assert_eq!(
            "(=: x 3)\n(=: y (- x 4))\n(if (> x y)\n  (expr (call (get prln io) 3)))\n",
            AstPrinter::print(&ast)
        );
    }
}
