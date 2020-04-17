use crate::{ErrorKind, PiccoloError, Scanner, Token, TokenKind, Value};

use super::expr::Expr;
use super::stmt::Stmt;

pub struct Parser<'a> {
    ast: Vec<Stmt<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser { ast: Vec::new() }
    }

    fn parse<'b>(&mut self, scanner: &'b mut Scanner<'a>) -> Result<Vec<Stmt<'a>>, PiccoloError>
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
            let _assign = scanner.next_token()?;
            let value = self.expr_bp(scanner, 0)?;
            self.ast.push(Stmt::Assignment { name, value });
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
#[should_panic] // TODO
fn idk() {
    let src = "a=1+2";
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

struct NewEmitter(crate::Chunk);
impl NewEmitter {
    fn emit(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            stmt.accept(self);
        }
    }
}
use super::ExprAccept;
use crate::ast::stmt::{StmtAccept, StmtVisitor};
use crate::runtime::op::Opcode;
impl super::ExprVisitor for NewEmitter {
    type Output = ();

    fn visit_value(&mut self, value: &Value) -> Self::Output {
        let i = self.0.make_constant(value.try_clone().unwrap());
        let (low, high) = crate::decode_bytes(i);
        self.0.write(Opcode::Constant, 1);
        self.0.write(low, 1);
        self.0.write(high, 1);
    }

    fn visit_unary(&mut self, _op: &Token, rhs: &Expr) -> Self::Output {
        rhs.accept(self);
    }

    fn visit_binary(&mut self, lhs: &Expr, op: &Token, rhs: &Expr) -> Self::Output {
        lhs.accept(self);
        rhs.accept(self);
        match op.kind {
            TokenKind::Plus => self.0.write(Opcode::Add, 1),
            TokenKind::Minus => self.0.write(Opcode::Subtract, 1),
            TokenKind::Divide => self.0.write(Opcode::Divide, 1),
            TokenKind::Multiply => self.0.write(Opcode::Multiply, 1),
            TokenKind::Equal => self.0.write(Opcode::Equal, 1),
            TokenKind::NotEqual => {
                self.0.write(Opcode::Equal, 1);
                self.0.write(Opcode::Not, 1);
            }
            TokenKind::Greater => self.0.write(Opcode::Greater, 1),
            TokenKind::GreaterEqual => self.0.write(Opcode::GreaterEqual, 1),
            TokenKind::Less => self.0.write(Opcode::Less, 1),
            TokenKind::LessEqual => self.0.write(Opcode::LessEqual, 1),
            _ => {}
        }
    }

    fn visit_paren(&mut self, value: &Expr) -> Self::Output {
        value.accept(self);
    }

    fn visit_variable(&mut self, _name: &Token) -> Self::Output {
        unimplemented!("visit_variable")
    }

    fn visit_assign(&mut self, _name: &Token, value: &Expr) -> Self::Output {
        value.accept(self);
    }

    fn visit_logical(&mut self, lhs: &Expr, _op: &Token, rhs: &Expr) -> Self::Output {
        lhs.accept(self);
        rhs.accept(self);
    }

    fn visit_call(
        &mut self,
        _callee: &Expr,
        _paren: &Token,
        _arity: super::Arity,
        _args: &[Expr],
    ) -> Self::Output {
        unimplemented!()
    }

    fn visit_new(&mut self, _name: &Token, _args: &[(Token, Box<Expr>)]) -> Self::Output {
        unimplemented!("visit_new")
    }

    fn visit_get(&mut self, _object: &Expr, _name: &Token) -> Self::Output {
        unimplemented!("visit_get")
    }

    fn visit_set(&mut self, _object: &Expr, _name: &Token, _value: &Expr) -> Self::Output {
        unimplemented!("visit_set")
    }

    fn visit_index(&mut self, _rb: &Token, _object: &Expr, _idx: &Expr) -> Self::Output {
        unimplemented!("visit_index")
    }

    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: super::Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        unimplemented!("visit_func")
    }
}
impl StmtVisitor for NewEmitter {
    type Output = ();
    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        expr.accept(self);
        self.0.write(Opcode::Pop, 1); // TODO: line
    }
    fn visit_assignment(&mut self, name: &Token, value: &Expr) -> Self::Output {
        value.accept(self);
        let idx = self.0.make_constant(Value::String(name.lexeme.to_owned()));
        let (low, high) = crate::decode_bytes(idx);
        self.0.write(Opcode::AssignGlobal, name.line);
        self.0.write(low, name.line);
        self.0.write(high, name.line);
    }
    fn visit_block(&mut self, _stmts: &[Stmt]) -> Self::Output {
        unimplemented!("visit_block")
    }
    fn visit_if(
        &mut self,
        _cond: &Expr,
        _then: &[Stmt],
        _else_: Option<&Vec<Stmt>>,
    ) -> Self::Output {
        unimplemented!("visit_if")
    }
    fn visit_while(&mut self, _cond: &Expr, _body: &[Stmt]) -> Self::Output {
        unimplemented!("visit_while")
    }
    fn visit_for(&mut self, _name: &Token, _iter: &Expr, _body: &[Stmt]) -> Self::Output {
        unimplemented!("visit_for")
    }
    fn visit_func(
        &mut self,
        _name: &Token,
        _args: &[Token],
        _arity: super::Arity,
        _body: &[Stmt],
        _method: bool,
    ) -> Self::Output {
        unimplemented!("visit_func")
    }
    fn visit_retn(&mut self, _keyword: &Token, _value: Option<&Expr>) -> Self::Output {
        unimplemented!("visit_retn")
    }
    fn visit_data(
        &mut self,
        _name: &Token,
        _methods: &[Stmt],
        _fields: &[(Token, Expr)],
    ) -> Self::Output {
        unimplemented!("visit_data")
    }
}
