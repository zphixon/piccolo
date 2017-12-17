
use ::*;

pub trait StmtAccept {
    fn accept<T: StmtVisitor>(&self, visitor: &mut T) -> T::Output;
}

pub trait StmtVisitor {
    type Output;
    fn visit_expr(&mut self, s: &StmtExpr) -> Self::Output;
    fn visit_assignment(&mut self, s: &Assignment) -> Self::Output;
    fn visit_block(&mut self, s: &Block) -> Self::Output;
    fn visit_if(&mut self, s: &If) -> Self::Output;
    fn visit_while(&mut self, s: &While) -> Self::Output;
    fn visit_for(&mut self, s: &For) -> Self::Output;
    fn visit_func(&mut self, s: &Func) -> Self::Output;
}

#[derive(Debug, PartialEq, Clone)]
pub struct StmtExpr(pub expr::Expr);

impl StmtAccept for StmtExpr {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_expr(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Assignment {
    pub name: token::Token,
    pub value: expr::Expr,
}

impl StmtAccept for Assignment {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_assignment(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block(pub Vec<Stmt>);

impl StmtAccept for Block {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_block(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub cond: expr::Expr,
    pub then: Vec<Stmt>,
    pub else_: Option<Vec<Stmt>>,
}

impl StmtAccept for If {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_if(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub cond: expr::Expr,
    pub body: Vec<Stmt>,
}

impl StmtAccept for While {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_while(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub name: token::Token,
    pub iter: expr::Expr,
    pub body: Vec<Stmt>,
}

impl StmtAccept for For {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_for(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Func {
    pub name: token::Token,
    pub args: Vec<token::Token>,
    pub body: Vec<Stmt>,
}

impl StmtAccept for Func {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        v.visit_func(&self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    StmtExpr(StmtExpr),
    Assignment(Assignment),
    Block(Block),
    If(If),
    While(While),
    For(For),
    Func(Func),
}

impl StmtAccept for Stmt {
    fn accept<T: StmtVisitor>(&self, v: &mut T) -> T::Output {
        match *self {
            Stmt::StmtExpr(ref s) => s.accept(v),
            Stmt::Assignment(ref s) => s.accept(v),
            Stmt::Block(ref s) => s.accept(v),
            Stmt::If(ref s) => s.accept(v),
            Stmt::While(ref s) => s.accept(v),
            Stmt::For(ref s) => s.accept(v),
            Stmt::Func(ref s) => s.accept(v),
        }
    }
}

