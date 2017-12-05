
use token::Token;

pub trait ExprVisitor<T> {
    fn visit_binary(&mut self, b: Binary) -> T;
    fn visit_unary(&mut self, b: Unary) -> T;
    fn visit_paren(&mut self, b: Paren) -> T;
    //fn visit_fncall(&mut self, b: FnCall) -> T; // TODO
    //fn visit_me(&mut self, b: Me) -> T;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Range, // TODO
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub op: Token,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
    pub op: Token,
    pub rhs: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Paren(Box<Expr>);

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary(Binary),
    Unary(Unary),
    Literal(Literal),
    Paren(Box<Expr>),
}

pub fn pprint_ast(e: Expr) {
    pp_rec(e, 0);
}

fn pp_rec(e: Expr, indent: u64) {
    match e {
        Expr::Binary(b) => {
            for _ in 0..indent { print!("    "); }
            println!("binary");
            for _ in 0..indent { print!("    "); }
            println!("    lhs");
            pp_rec(*b.lhs, indent + 2);
            for _ in 0..indent { print!("    "); }
            println!("    op: {:?}", b.op.kind);
            for _ in 0..indent { print!("    "); }
            println!("    rhs");
            pp_rec(*b.rhs, indent + 2);
        },
        Expr::Unary(u) =>  {
            for _ in 0..indent { print!("    "); }
            println!("unary");
            for _ in 0..indent { print!("    "); }
            println!("    op: {:?}", u.op.kind);
            for _ in 0..indent { print!("    "); }
            println!("    rhs");
            pp_rec(*u.rhs, indent + 1);
        },
        Expr::Literal(l) => {
            for _ in 0..indent { print!("    "); }
            println!("{:?}", l);
        },
        Expr::Paren(p) => {
            pp_rec(*p, indent + 1);
        }
    }
}

