
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
        Newline, Bang, Plus, Hyphen, Star, FSlash, Mod, And, Or, BAnd, BOr, BXor,
        Equals, BangEquals, LessThan, GreaterThan, LessThanEquals, GreaterThanEquals,
        Ident, String, Double(3.14), Integer(23), Eof
    ]);
}

#[test]
fn test_file() {
    println!("{:?}", piccolo::parse_file("test.pc").unwrap());
}

