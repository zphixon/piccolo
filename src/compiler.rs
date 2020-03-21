use crate::scanner::Token;

pub fn compile(s: Vec<Token>) -> crate::Result<()> {
    println!("{:#?}", s);
    Ok(())
}
