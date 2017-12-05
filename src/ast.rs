/*
#[derive(Debug, PartialEq)]
pub struct Paren {
    pub inner: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Access {
    pub from: Expression,
    pub of: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Name,
    pub value: Expression, // TODO
}

#[derive(Debug, PartialEq)]
pub struct ExclusiveRange {
    pub lower: Expression,
    pub upper: Expression,
}

#[derive(Debug, PartialEq)]
pub struct InclusiveRange {
    pub lower: Expression,
    pub upper: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(String),
    Integer(i64),
    Float(f64),
    True,
    False,
    ExclusiveRange(ExclusiveRange),
    InclusiveRange(InclusiveRange),
    Paren(Paren),
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub callee: Expression,
    pub args: Vec<Expression>,
}*/

/*#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct UnaryRest {
    pub op: UnaryOp,
    pub rhs: Box<Unary>,
}

#[derive(Debug, PartialEq)]
pub enum Unary {
    Primary(Literal),
    UnaryOp(UnaryRest),
}

#[derive(Debug, PartialEq)]
pub enum MultiplicationOp {
    Multiply,
    Divide,
    Mod,
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq)]
pub struct MultiplicationRest {
    pub op: MultiplicationOp,
    pub rhs: Vec<Unary>,
}

#[derive(Debug, PartialEq)]
pub struct Multiplication {
    pub inner: Unary,
    pub rest: Vec<MultiplicationRest>,
}

#[derive(Debug, PartialEq)]
pub enum AdditionOp {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub struct AdditionRest {
    pub op: AdditionOp,
    pub rhs: Multiplication,
}

#[derive(Debug, PartialEq)]
pub struct Addition {
    pub inner: Multiplication,
    pub rest: Vec<AdditionRest>,
}

#[derive(Debug, PartialEq)]
pub enum ComparisonOp {
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
}

#[derive(Debug, PartialEq)]
pub struct ComparisonRest {
    pub op: ComparisonOp,
    pub rhs: Addition
}

#[derive(Debug, PartialEq)]
pub struct Comparison {
    pub inner: Addition,
    pub rest: Vec<ComparisonRest>
}

#[derive(Debug, PartialEq)]
pub enum EqualityOp {
    NotEqual,
    Equal,
}

#[derive(Debug, PartialEq)]
pub struct EqualityRest {
    pub op: EqualityOp,
    pub rhs: Comparison,
}

#[derive(Debug, PartialEq)]
pub struct Equality {
    pub inner: Comparison,
    pub rest: Vec<ComparisonRest>,
}

#[derive(Debug, PartialEq)]
pub struct OrRest {
    pub rhs: Equality,
}

#[derive(Debug, PartialEq)]
pub struct Or {
    pub inner: Equality,
    pub rest: Vec<EqualityRest>,
}

#[derive(Debug, PartialEq)]
pub struct AndRest {
    pub rhs: Or,
}

#[derive(Debug, PartialEq)]
pub struct And {
    pub inner: Or,
    pub rest: Vec<OrRest>,
}

// math: and -> or -> equality -> comparison -> addition -> multiplication -> unary -> primary

#[derive(Debug, PartialEq)]
pub struct Math {
    pub inner: And,
    pub rest: Vec<AndRest>,
} */
/*
#[derive(Debug, PartialEq)]
pub enum Expression {
    //Paren(Box<Paren>),
    Access(Box<Access>),
    Variable(Box<Variable>),
    Literal(Box<Literal>),
    FunctionCall(Box<FunctionCall>),
    Math(Box<Math>),
    Nil,
}*/
/*
#[derive(Debug, PartialEq)]
pub struct Assignment {
    pub name: Name,
    pub expr: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Error {
    pub msg: Option<String>,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub enum FlowKind {
    If,
    While,
    For,
}

#[derive(Debug, PartialEq)]
pub struct Flow {
    pub flow_kind: FlowKind,
    pub inner: Vec<Statement>,
    pub else_: Option<Expression>
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Assignment(Assignment),
    Return(Return),
    Error(Error),
    Flow(Flow),
    Block(Vec<Statement>),
    Function(Function),
    Data(Data),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub args: Vec<Name>,
    pub inner: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Name {
    pub name: String,
}

impl<'a> From<&'a str> for Name {
    fn from(name: &str) -> Self {
        Name {
            name: name.to_owned()
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Field {
    pub public: bool,
    pub name: Name,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Data {
    pub name: Name,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
} */

use token::Token;
/////////////////////////////////////////////
// ad-hoc implementation of all this stuff //
/////////////////////////////////////////////

//pub enum BinaryOp {
//    Plus,
//    Minus,
//    Multiply,
//    Divide,
//    Mod,
//    And,
//    Or,
//    BinaryAnd,
//    BinaryOr,
//    BinaryXor,
//    Equals,
//    NotEquals,
//    Less,
//    Greater,
//    LessEquals,
//    GreaterEquals,
//}

// uncomment when you eventually delete the rest of this file
//pub enum UnaryOp {
//    Not,
//    Negate,
//}

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

