use crate::compiler::Compiler;
use crate::scanner::TokenKind;

pub(crate) fn get_rule(kind: &TokenKind) -> &'static ParseRule<'static> {
    if let TokenKind::Double(_) = kind {}
    for (k, rule) in RULES.iter() {
        if k == kind {
            return rule;
        } else {
            if let TokenKind::Double(_) = kind {
                if let TokenKind::Double(_) = k {
                    return rule;
                }
            }

            if let TokenKind::Integer(_) = kind {
                if let TokenKind::Integer(_) = k {
                    return rule;
                }
            }

            if let TokenKind::String(_) = kind {
                if let TokenKind::String(_) = k {
                    return rule;
                }
            }
        }
    }

    panic!("no rule for {:?}", kind);
}

#[derive(Debug, Clone, Copy, PartialOrd, PartialEq)]
pub(crate) enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

pub(crate) struct ParseRule<'a> {
    pub(crate) prefix: Option<fn(&'a mut Compiler<'a>) -> crate::Result<()>>,
    pub(crate) infix: Option<fn(&'a mut Compiler<'a>) -> crate::Result<()>>,
    pub(crate) precedence: Precedence,
}

pub(crate) const RULES: &[(TokenKind, ParseRule)] = &[
    (TokenKind::Do,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::End,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Fn,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::If,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Else,              ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::While,             ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::For,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::In,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Data,              ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Is,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Me,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::New,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Err,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Retn,              ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Nil,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::LeftBracket,       ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::RightBracket,      ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::LeftParen,         ParseRule { prefix: Some(Compiler::grouping), infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::RightParen,        ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Comma,             ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Period,            ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::ExclusiveRange,    ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::InclusiveRange,    ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Assign,            ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Newline,           ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Not,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Plus,              ParseRule { prefix: None,                     infix: Some(Compiler::binary), precedence: Precedence::Term,       }),
    (TokenKind::Minus,             ParseRule { prefix: Some(Compiler::unary),    infix: Some(Compiler::binary), precedence: Precedence::Term,       }),
    (TokenKind::Multiply,          ParseRule { prefix: None,                     infix: Some(Compiler::binary), precedence: Precedence::Factor,     }),
    (TokenKind::Divide,            ParseRule { prefix: None,                     infix: Some(Compiler::binary), precedence: Precedence::Factor,     }),
    (TokenKind::Modulo,            ParseRule { prefix: None,                     infix: Some(Compiler::binary), precedence: Precedence::Factor,     }),
    (TokenKind::And,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Or,                ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::BitwiseAnd,        ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::BitwiseOr,         ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::BitwiseXor,        ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Equals,            ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::NotEquals,         ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::LessThan,          ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::GreaterThan,       ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::LessThanEquals,    ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::GreaterThanEquals, ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::ShiftLeft,         ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::ShiftRight,        ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Identifier,        ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::True,              ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::False,             ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Eof,               ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::String(String::new()), ParseRule { prefix: None,                     infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Double(0.0),           ParseRule { prefix: Some(Compiler::number),   infix: None,                   precedence: Precedence::None,       }),
    (TokenKind::Integer(0),            ParseRule { prefix: Some(Compiler::number),   infix: None,                   precedence: Precedence::None,       }),
];
