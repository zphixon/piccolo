use crate::{
    compiler::{
        ast::{Ast, Expr, Stmt},
        Token, TokenKind,
    },
    error::PiccoloError,
};
use fnv::FnvHashMap;

macro_rules! type_names {
    ($name:ident : $str:expr) => {
        paste::paste! {
            pub const [<TYPE_NAME_ $name>]: &'static str = $str;
        }
    };

    ($($name:ident : $str:expr,)*) => {
        $(type_names!($name: $str);)*
    };
}

macro_rules! integer_constants {
    ($($bits:expr),*) => {
        paste::paste! {
            $(
                type_names!(
                    [<INT $bits>]: concat!("Int", stringify!($bits)),
                    [<UINT $bits>]: concat!("UInt", stringify!($bits)),
                );
            )*
        }
    };
}

macro_rules! integer_register {
    ($whomst:ident, $($bits:expr),*) => {
        paste::paste! {
            $(
                $whomst.register([<TYPE_NAME_INT $bits>]);
                $whomst.register([<TYPE_NAME_UINT $bits>]);
            )*
        }
    };
}

integer_constants!(8, 16, 32, 64);
type_names!(
    BOOL: "Bool",
    ARRAY: "Array",
    STRING: "String",
    FLOAT: "Float",
);

pub struct TypeKb<'src> {
    types: FnvHashMap<&'src str, TypeId<'src>>,
    next_id: usize,
}

impl<'src> TypeKb<'src> {
    fn new() -> Self {
        let mut type_kb = TypeKb {
            types: FnvHashMap::default(),
            next_id: 0,
        };

        integer_register!(type_kb, 8, 16, 32, 64);

        type_kb.register(TYPE_NAME_BOOL);
        type_kb.register_args(TYPE_NAME_ARRAY, vec!["Item"]);
        type_kb.register(TYPE_NAME_STRING);
        type_kb.register(TYPE_NAME_FLOAT);

        type_kb
    }

    fn register(&mut self, name: &'src str) -> (TypeId<'src>, bool) {
        if self.types.contains_key(name) {
            return (self.types[name].clone(), true);
        }

        let id = self.next_id;
        self.next_id += 1;

        let type_id = TypeId {
            name,
            id,
            args: Vec::with_capacity(0),
        };

        self.types.insert(name, type_id.clone());

        (type_id, false)
    }

    fn register_args(&mut self, name: &'src str, args: Vec<&'src str>) -> (TypeId<'src>, bool) {
        let (mut type_id, exists) = self.register(name);
        if exists {
            return (type_id, exists);
        }

        type_id.args = args;

        (type_id, false)
    }

    fn get(&self, name: &'src str) -> Option<TypeId<'src>> {
        self.types.get(name).cloned()
    }

    fn inner_id_of(&self, name: &'src str) -> Option<usize> {
        Some(self.get(name)?.id)
    }
}

#[derive(Clone, Debug)]
pub struct TypeId<'src> {
    name: &'src str,
    id: usize,
    args: Vec<&'src str>,
}

impl PartialEq for TypeId<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.args == other.args
    }
}

pub type CheckedAst<'src> = [CheckedStmt<'src>];

#[derive(Debug, PartialEq)]
pub enum CheckedExpr<'src> {
    Literal {
        literal: Token<'src>,
        type_: TypeId<'src>,
    },
    Binary {
        lhs: Box<CheckedExpr<'src>>,
        op: Token<'src>,
        rhs: Box<CheckedExpr<'src>>,
        type_: TypeId<'src>,
    },
}

impl CheckedExpr<'_> {
    fn inner_id(&self) -> usize {
        match self {
            CheckedExpr::Literal { type_, .. } => type_.id,
            CheckedExpr::Binary { type_, .. } => type_.id,
            //_ => todo!("id for {self:#?}"),
        }
    }

    #[allow(dead_code)]
    fn id(&self) -> TypeId {
        match self {
            CheckedExpr::Literal { type_, .. } => type_.clone(),
            CheckedExpr::Binary { type_, .. } => type_.clone(),
            //_ => todo!("id for {self:#?}"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CheckedStmt<'src> {
    Expr {
        token: Token<'src>,
        expr: CheckedExpr<'src>,
    },
}

pub fn check<'src>(ast: &Ast<'src>) -> Result<Vec<CheckedStmt<'src>>, PiccoloError> {
    let mut type_kb = TypeKb::new();

    let mut checked_ast = Vec::new();
    for stmt in ast {
        checked_ast.push(check_stmt(&mut type_kb, stmt)?);
    }

    Ok(checked_ast)
}

#[rustfmt::skip]
fn check_stmt<'src>(type_kb: &mut TypeKb<'src>, stmt: &Stmt<'src>) -> Result<CheckedStmt<'src>, PiccoloError> {
    match stmt {
        Stmt::Expr { token, expr }
            => check_expr_stmt(type_kb, *token, expr),
        //Stmt::Block { end, body }
        //    => check_block(type_kb, *end, body),
        //Stmt::Declaration { name, value, .. }
        //    => check_declaration(type_kb, *name, value),
        //Stmt::Assignment { name, op, value }
        //    => check_assignment(type_kb, *name, *op, value),
        //Stmt::If { if_, cond, then_block, else_, else_block, end }
        //    => check_if(type_kb, *if_, cond, then_block, else_.as_ref(), else_block.as_ref(), *end),
        //Stmt::While { while_, cond, body, end }
        //    => check_while(type_kb, *while_, cond, body, *end),
        //Stmt::For { for_, init, cond, inc, body, end }
        //    => check_for(type_kb, *for_, init.as_ref(), cond, inc.as_ref(), body, *end),
        //Stmt::Fn { name, args, arity, body, method, end }
        //    => check_fn(type_kb, *name, args, *arity, body, *method, *end),
        //Stmt::Break { break_ }
        //    => check_break(type_kb, *break_),
        //Stmt::Continue { continue_ }
        //    => check_continue(type_kb, *continue_),
        //Stmt::Retn { retn, value }
        //    => check_retn(type_kb, *retn, value.as_ref()),
        //Stmt::Assert { assert, value }
        //    => check_assert(type_kb, *assert, value),
        //Stmt::Data { name, methods, fields }
        //    => check_data(type_kb, *name, methods, fields),
        _ => todo!("type check Stmt::{stmt:#?}")
    }
}

fn check_expr_stmt<'src>(
    type_kb: &mut TypeKb<'src>,
    token: Token<'src>,
    expr: &Expr<'src>,
) -> Result<CheckedStmt<'src>, PiccoloError> {
    let expr = check_expr(type_kb, expr)?;
    Ok(CheckedStmt::Expr { token, expr })
}

#[rustfmt::skip]
fn check_expr<'src>(
    type_kb: &mut TypeKb<'src>,
    expr: &Expr<'src>,
) -> Result<CheckedExpr<'src>, PiccoloError> {
    match expr {
        Expr::Literal { literal }
            => check_literal(type_kb, *literal),
        //Expr::Paren { right_paren, expr }
        //    => check_paren(type_kb, *right_paren, expr),
        //Expr::Variable { variable }
        //    => check_variable(type_kb, *variable),
        //Expr::Unary { op, rhs }
        //    => check_unary(type_kb, *op, rhs),
        Expr::Binary { lhs, op, rhs }
            => check_binary(type_kb, lhs, *op, rhs),
        //Expr::Logical { lhs, op, rhs }
        //    => check_logical(type_kb, lhs, *op, rhs),
        //Expr::Call { callee, paren, arity, args }
        //    => check_call(type_kb, callee, *paren, *arity, args),
        //Expr::New { name, args }
        //     => check_new(type_kb, name, args),
        //Expr::Get { object, name }
        //     => check_get(type_kb, object, name),
        //Expr::Set { object, name, value }
        //     => check_set(type_kb, object, name, value),
        //Expr::Index { right_bracket, object, index }
        //     => check_index(type_kb, right_bracket, object, index),
        //Expr::Fn { fn_, args, arity, body, end }
        //    => check_lambda(type_kb, *fn_, args, *arity, body, *end),
        _ => todo!("type check Expr::{:#?}", expr),
    }
}

fn check_literal<'src>(
    type_kb: &mut TypeKb<'src>,
    literal: Token<'src>,
) -> Result<CheckedExpr<'src>, PiccoloError> {
    Ok(CheckedExpr::Literal {
        literal,
        type_: match literal.kind {
            TokenKind::String => type_kb.get(TYPE_NAME_STRING).unwrap(),
            TokenKind::True | TokenKind::False => type_kb.get(TYPE_NAME_BOOL).unwrap(),
            TokenKind::Double(_) => type_kb.get(TYPE_NAME_FLOAT).unwrap(),
            TokenKind::Integer(_) => type_kb.get(TYPE_NAME_INT64).unwrap(),
            _ => unreachable!("checking literal type of non-literal {literal:?}"),
        },
    })
}

fn check_binary<'src>(
    type_kb: &mut TypeKb<'src>,
    lhs: &Expr<'src>,
    op: Token<'src>,
    rhs: &Expr<'src>,
) -> Result<CheckedExpr<'src>, PiccoloError> {
    let lhs = Box::new(check_expr(type_kb, lhs)?);
    let lhs_id = lhs.inner_id();

    let rhs = Box::new(check_expr(type_kb, rhs)?);
    let rhs_id = lhs.inner_id();

    let string_id = type_kb.inner_id_of(TYPE_NAME_STRING).unwrap();
    let int64_id = type_kb.inner_id_of(TYPE_NAME_INT64).unwrap();

    Ok(CheckedExpr::Binary {
        lhs,
        op,
        rhs,
        type_: match (op.kind, lhs_id, rhs_id) {
            (TokenKind::Plus, lhs, _) if lhs == string_id => type_kb.get(TYPE_NAME_STRING).unwrap(),
            (TokenKind::Plus, lhs, rhs) if lhs == int64_id && rhs == int64_id => {
                type_kb.get(TYPE_NAME_INT64).unwrap()
            }
            _ => todo!(),
        },
    })
}

#[cfg(test)]
mod test {
    use crate::compiler::{parser::parse, Pos};

    use super::*;

    #[test]
    fn literal() {
        let prog = "\"hello!\" + 32";
        let ast = parse(prog).unwrap();
        let checked = check(&ast).unwrap();

        assert_eq!(
            checked,
            vec![CheckedStmt::Expr {
                token: Token::new(TokenKind::String, "\"hello!\"", Pos::Builtin),
                expr: CheckedExpr::Binary {
                    lhs: Box::new(CheckedExpr::Literal {
                        literal: Token::new(TokenKind::String, "\"hello!\"", Pos::Builtin),
                        type_: TypeId {
                            name: TYPE_NAME_STRING,
                            id: 0,
                            args: vec![],
                        }
                    }),
                    op: Token::new(TokenKind::Plus, "+", Pos::Builtin),
                    rhs: Box::new(CheckedExpr::Literal {
                        literal: Token::new(TokenKind::Integer(32), "32", Pos::Builtin),
                        type_: TypeId {
                            name: TYPE_NAME_INT64,
                            id: 0,
                            args: vec![]
                        }
                    }),
                    type_: TypeId {
                        name: TYPE_NAME_STRING,
                        id: 0,
                        args: vec![]
                    },
                }
            }]
        )
    }
}
