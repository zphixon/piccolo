use crate::scanner::Token;

pub fn compile(s: Vec<Token>) -> crate::Result<()> {
    crate::scanner::print_tokens(s);
    Ok(())
}
