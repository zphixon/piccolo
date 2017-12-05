
use token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Lit {
    Bool(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Range, // TODO
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Binary {
        lhs: Box<Expr>,
        op: Token,
        rhs: Box<Expr>,
    },
    Unary {
        op: Token,
        rhs: Box<Expr>,
    },
    Literal(Lit),
    Paren(Box<Expr>),
}

pub fn pprint_ast(e: Expr) {
    pp_rec(e, 0);
}

fn pp_rec(e: Expr, indent: u64) {
    match e {
        Expr::Binary { lhs, op, rhs } => {
            for _ in 0..indent { print!("    "); }
            println!("binary");
            for _ in 0..indent { print!("    "); }
            println!("    lhs");
            pp_rec(*lhs, indent + 2);
            for _ in 0..indent { print!("    "); }
            println!("    op: {:?}", op.kind);
            for _ in 0..indent { print!("    "); }
            println!("    rhs");
            pp_rec(*rhs, indent + 2);
        },
        Expr::Unary { op, rhs } =>  {
            for _ in 0..indent { print!("    "); }
            println!("unary");
            for _ in 0..indent { print!("    "); }
            println!("    op: {:?}", op.kind);
            for _ in 0..indent { print!("    "); }
            println!("    rhs");
            pp_rec(*rhs, indent + 1);
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

