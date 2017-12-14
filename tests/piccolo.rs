
extern crate piccolo;

#[test]
fn scan_correctly() {
    use piccolo::scanner::Scanner;
    use piccolo::token::TokenKind;
    use piccolo::token::TokenKind::*;
    let tk: Vec<TokenKind> = Scanner::new("do end fn if else while for in data is pub me new err retn \
                           [ ] ( ) , . .. ... = \n ! + - * / % && || & | ^ == != < > \
                           <= >= ident \"string\" 3.14 23".into()).scan_tokens().unwrap()
               .iter().map(|t| t.kind).collect();
    assert_eq!(tk, vec![
        Do, End, Fn, If, Else, While, For, In, Data, Is, Pub, Me, New, Err, Retn,
        LBracket, RBracket, LParen, RParen, Comma, Dot, ERange, IRange, Assign,
        Newline, Not, Plus, Minus, Star, Divide, Mod, And, Or, BAnd, BOr, BXor,
        Equals, NotEquals, LessThan, GreaterThan, LessThanEquals, GreaterThanEquals,
        Ident, String, Double(3.14), Integer(23), Eof
    ]);
}

#[test]
fn test_file() {
    println!("{:?}", piccolo::parse_file("test.pc").unwrap());
}

#[test]
fn equal_truthy() {
    use piccolo::interp::{is_equal, is_truthy};
    use piccolo::value::Value;
    assert!(is_equal(&Value::Nil, &Value::Nil));
    assert!(!is_truthy(&Value::Nil));
    assert!(is_equal(&Value::String("a".into()), &Value::String("a".into())));
    assert!(!is_equal(&Value::String("a".into()), &Value::String("b".into())));
    assert!(!is_equal(&Value::Float(3.0), &Value::Integer(3)));
    assert!(is_equal(&Value::Integer(3), &Value::Integer(3)));
    assert!(is_truthy(&Value::String("".into())));
}

/*
#[test]
fn parse_math() {
    use piccolo::scanner::Scanner;
    use piccolo::parser::Parser;
    //use piccolo::ast::*;
    use piccolo::expr::*;
    use piccolo::*;
    use piccolo::expr::Expr::Binary;
    use piccolo::expr::Expr::*;
    use piccolo::token::{Token, TokenKind};

    let code = "32 + -4.5 == 72 * 3 && 4 != 5".into();
    let parse = Parser::new(Scanner::new(code).scan_tokens().unwrap()).parse();

    let ast = Ok(Expr::Binary(expr::Binary {
        lhs: Box::new(Expr::Binary(expr::Binary {
            lhs: Box::new(Expr::Binary(expr::Binary {
                lhs: Box::new(Expr::Literal(expr::Literal::Integer(32))),
                op: Token { kind: TokenKind::Plus, lexeme: "+".into(), line: 1 },
                rhs: Box::new(Expr::Unary(expr::Unary {
                    op: Token { kind: TokenKind::Minus, lexeme: "-".into(), line: 1 },
                    rhs: Box::new(Expr::Literal(expr::Literal::Float(4.5))),
                }))
            })),
            op: Token { kind: TokenKind::Equals, lexeme: "==".into(), line: 1 },
            rhs: Box::new(Expr::Binary(expr::Binary {
                lhs: Box::new(Expr::Literal(expr::Literal::Integer(72))),
                op: Token { kind: TokenKind::Star, lexeme: "*".into(), line: 1 },
                rhs: Box::new(Expr::Literal(expr::Literal::Integer(3))),
            }))
        })),
        op: Token { kind: TokenKind::And, lexeme: "&&".into(), line: 1 },
        rhs: Box::new(Expr::Binary(expr::Binary {
            lhs: Box::new(Expr::Literal(expr::Literal::Integer(4))),
            op: Token { kind: TokenKind::NotEquals, lexeme: "!=".into(), line: 1 },
            rhs: Box::new(Expr::Literal(expr::Literal::Integer(5))),
        }))
    }));

    assert_eq!(ast, parse);
}

#[test]
#[ignore]
fn parse() {
    use piccolo::scanner::Scanner;
    use piccolo::parser::Parser;
    use piccolo::ast::*;

    let code = "fn main() x = 3 + 2.3 end".into();

    let parse = Parser::new(Scanner::new(code).scan_tokens().unwrap()).parse();

    /*
    let ast: Result<Vec<Statement>, String> = Ok(vec![
        Statement::Function(
            Function {
                name: "main".into(),
                args: vec![],
                inner: vec![
                    Statement::Assignment(
                        Assignment {
                            name: "x".into(),
                            expr: Expression::Math(Box::new(Math {
                                    inner: And {
                                        inner: Or {
                                            inner: Equality {
                                                inner: Comparison {
                                                    inner: Addition {
                                                        inner: Multiplication {
                                                            inner: Unary::Primary(Literal::Integer(3)),
                                                            rest: vec![]
                                                        },
                                                        rest: vec![
                                                            AdditionRest {
                                                                op: AdditionOp::Plus,
                                                                rhs: Multiplication {
                                                                    inner: Unary::Primary(Literal::Float(2.3)),
                                                                    rest: vec![]
                                                                }
                                                            }
                                                        ]
                                                    },
                                                    rest: vec![]
                                                },
                                                rest: vec![]
                                            },
                                            rest: vec![]
                                        },
                                        rest: vec![]
                                    },
                                    rest: vec![]
                                })
                            )
                        }
                    ),
                ]
            }
        )
    ]);*/

    //assert_eq!(ast, parse);
}*/

