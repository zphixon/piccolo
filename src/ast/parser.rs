use crate::ast::stmt::Stmt;
use crate::compiler::{scanner::Scanner, Token, TokenKind};
use crate::error::{ErrorKind, PiccoloError};

pub struct Parser<'a: 'b, 'b> {
    err: Vec<PiccoloError>,
    scanner: &'b mut Scanner<'a>,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(scanner: &'b mut Scanner<'a>) -> Self {
        Parser {
            err: vec![],
            scanner,
        }
    }

    pub fn parse(mut self) -> Result<Vec<Stmt<'a, 'b>>, Vec<PiccoloError>> {
        let stmts = vec![
            Stmt::Retn {
                keyword: self.scanner.current(),
                value: None,
            }
        ];

        Ok(stmts)
    }
}
