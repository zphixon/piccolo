use crate::{ErrorKind, PiccoloError, Scanner, Token, TokenKind, Value};

use super::expr::Expr;
use super::stmt::Stmt;
use crate::ast::expr::ExprAccept;
use crate::compiler::emitter2::NewEmitter;

#[derive(Default)]
pub struct Parser<'a> {
    ast: Vec<Stmt<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser { ast: Vec::new() }
    }

    pub fn parse<'b>(&mut self, scanner: &'b mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError>
    where
        'a: 'b,
    {
        while scanner.peek_token(0)?.kind != TokenKind::Eof {
            self.declaration(scanner)?;
        }

        Ok(std::mem::replace(&mut self.ast, Vec::new()))
    }

    fn declaration<'b>(&mut self, scanner: &'b mut Scanner<'a>) -> Result<(), PiccoloError> {
        if scanner.peek_token(1)?.kind == TokenKind::Assign {
            let name = scanner.next_token()?;
            let op = scanner.next_token()?;
            let value = self.expr_bp(scanner, 0)?;
            self.ast.push(Stmt::Assignment { name, op, value });
        } else if scanner.peek_token(1)?.kind == TokenKind::Declare {
            let name = scanner.next_token()?;
            let op = scanner.next_token()?;
            let value = self.expr_bp(scanner, 0)?;
            self.ast.push(Stmt::Assignment { name, op, value });
        } else {
            let expr = self.expr_bp(scanner, 0)?;
            self.ast.push(Stmt::Expr { expr });
        }

        Ok(())
    }

    fn expr_bp<'b>(
        &mut self,
        scanner: &'b mut Scanner<'a>,
        min_bp: u8,
    ) -> Result<Expr<'a>, PiccoloError>
    where
        'a: 'b,
    {
        let lhs_token = scanner.next_token()?;
        let mut lhs = match lhs_token.kind {
            TokenKind::Identifier => unimplemented!("var get"), //self.named_variable(),
            _ => Expr::Atom(Value::try_from(lhs_token).unwrap()),
        };

        loop {
            let op_token = scanner.peek_token(0)?;
            if op_token.kind == TokenKind::Eof {
                break;
            }

            let (lbp, rbp) = infix_binding_power(op_token.kind);
            if lbp < min_bp {
                break;
            }

            let op = scanner.next_token()?;
            let rhs = self.expr_bp(scanner, rbp)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: TokenKind) -> (u8, u8) {
    match op {
        TokenKind::Plus | TokenKind::Minus => (1, 2),
        TokenKind::Multiply | TokenKind::Divide => (3, 4),
        _ => panic!("ibp {:?}", op),
    }
}

#[test]
fn idk() {
    let src = "a=:1+2";
    let mut scanner = Scanner::new(src);
    let ast = Parser::new().parse(&mut scanner).unwrap();
    println!("{}", super::AstPrinter.print(&ast));
    let mut ne = NewEmitter(crate::Chunk::default());
    ne.emit(&ast);
    #[cfg(feature = "pc-debug")]
    {
        ne.0.disassemble("idklol");
    }
    let mut vm = crate::runtime::vm::Machine::new(ne.0);
    println!("{}", vm.interpret().unwrap());
}

#[test]
fn visitor_emitter() {
    let src = "1+2*3+4";
    let mut scanner = Scanner::new(src);
    let ast = Parser::new().parse(&mut scanner).unwrap();
    if let Stmt::Expr { expr } = &ast[0] {
        assert_eq!(
            expr,
            &Expr::Binary {
                lhs: Box::new(Expr::Binary {
                    lhs: Box::new(Expr::Atom(Value::Integer(1))),
                    op: Token::new(TokenKind::Plus, "+", 1),
                    rhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Atom(Value::Integer(2))),
                        op: Token::new(TokenKind::Multiply, "*", 1),
                        rhs: Box::new(Expr::Atom(Value::Integer(3)))
                    })
                }),
                op: Token::new(TokenKind::Plus, "+", 1),
                rhs: Box::new(Expr::Atom(Value::Integer(4))),
            }
        );

        let mut ne = NewEmitter(crate::Chunk::default());
        println!("{}", super::AstPrinter.print_expr(expr));
        expr.accept(&mut ne);
        #[cfg(feature = "pc-debug")]
        {
            ne.0.disassemble("idklol");
        }
        let mut vm = crate::runtime::vm::Machine::new(ne.0);
        assert_eq!(vm.interpret().unwrap(), Value::Integer(11));
    } else {
        panic!("ast not initialized")
    }
}
