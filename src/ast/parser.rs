use crate::{ErrorKind, PiccoloError, Scanner, Token, TokenKind, Value};

use super::Expr;
use super::Stmt;

#[derive(Default)]
pub struct Parser<'a> {
    ast: Vec<Stmt<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Parser { ast: Vec::new() }
    }

    pub fn parse<'b>(
        &mut self,
        scanner: &'b mut Scanner<'a>,
    ) -> Result<Vec<Stmt<'a>>, Vec<PiccoloError>>
    where
        'a: 'b,
    {
        let mut errors = vec![];
        while scanner.peek_token(0).map_err(|e| vec![e])?.kind != TokenKind::Eof {
            if let Err(e) = self.declaration(scanner) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(std::mem::take(&mut self.ast))
        } else {
            Err(errors)
        }
    }

    #[allow(clippy::if_same_then_else)]
    fn declaration<'b>(&mut self, scanner: &'b mut Scanner<'a>) -> Result<(), PiccoloError> {
        if scanner.peek_token(1)?.kind == TokenKind::Assign {
            let name = scanner.next_token()?;
            let op = scanner.next_token()?;
            let value = self.expr_bp(scanner, BindingPower::Assignment)?;
            self.ast.push(Stmt::Assignment { name, op, value });
        } else if scanner.peek_token(1)?.kind == TokenKind::Declare {
            let name = scanner.next_token()?;
            let op = scanner.next_token()?;
            let value = self.expr_bp(scanner, BindingPower::Assignment)?;
            self.ast.push(Stmt::Assignment { name, op, value });
        } else if scanner.peek_token(0)?.kind == TokenKind::Retn {
            let keyword = scanner.next_token()?;
            let value = Some(self.expr_bp(scanner, BindingPower::Assignment)?);
            self.ast.push(Stmt::Retn { keyword, value })
        } else {
            let expr = self.expr_bp(scanner, BindingPower::Assignment)?;
            self.ast.push(Stmt::Expr { expr });
        }

        Ok(())
    }

    fn expr_bp<'b>(
        &mut self,
        scanner: &'b mut Scanner<'a>,
        min_prec: BindingPower,
    ) -> Result<Expr<'a>, PiccoloError>
    where
        'a: 'b,
    {
        let lhs_token = scanner.next_token()?;
        let mut lhs = if lhs_token.is_value() {
            Expr::Atom(Value::try_from(lhs_token).unwrap())
        } else {
            let pbp = prefix_binding_power(lhs_token.kind);
            let rhs = self.expr_bp(scanner, pbp)?;
            Expr::Unary {
                op: lhs_token,
                rhs: Box::new(rhs)
            }
        };

        loop {
            let op_token = scanner.peek_token(0)?;
            if op_token.kind == TokenKind::Eof {
                break;
            }

            let op_prec = infix_binding_power(op_token.kind);
            if op_prec < min_prec {
                break;
            }

            let op = scanner.next_token()?;
            let rhs = self.expr_bp(scanner, op_prec + 1)?;
            lhs = Expr::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        }

        Ok(lhs)
    }
}

fn prefix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Minus => BindingPower::Unary,
        TokenKind::Not => BindingPower::Unary,
        op => panic!("no pbp for {:?}", op),
    }
}

fn infix_binding_power(kind: TokenKind) -> BindingPower {
    match kind {
        TokenKind::Plus => BindingPower::Term,
        TokenKind::Minus => BindingPower::Term,
        TokenKind::Multiply => BindingPower::Factor,
        TokenKind::Divide => BindingPower::Factor,
        TokenKind::Modulo => BindingPower::Factor,
        TokenKind::Equal => BindingPower::Equality,
        TokenKind::NotEqual => BindingPower::Equality,
        TokenKind::Less => BindingPower::Comparison,
        TokenKind::Greater => BindingPower::Comparison,
        TokenKind::LessEqual => BindingPower::Comparison,
        TokenKind::GreaterEqual => BindingPower::Comparison,
        op => panic!("no ibp for {:?}", op),
    }
}

macro_rules! prec {
    ($name:ident => $($item:ident = $num:expr,)*) => {
        #[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
        pub(crate) enum $name {
            $($item = $num,)*
        }

        impl Into<u8> for $name {
            fn into(self) -> u8 {
                match self {
                    $($name::$item => $num,)*
                }
            }
        }

        impl From<u8> for $name {
            fn from(u: u8) -> $name {
                match u {
                    $($num => $name::$item,)*
                    n => panic!("{} does not correspond to any item in {}", n, stringify!($name))
                }
            }
        }

        impl std::ops::Add<u8> for $name {
            type Output = $name;
            fn add(self, rhs: u8) -> $name {
                let s: u8 = self.into();
                (s + rhs).into()
            }
        }
    };
}

prec!(BindingPower =>
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
);
