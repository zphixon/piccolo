
pub mod parser;
pub mod token;
pub mod scanner;
pub mod expr;
pub mod ast;
pub mod err;

use scanner::Scanner;

use std::fs::File;
use std::io::Read;

pub fn parse_file(filename: &str) -> Result<Vec<token::Token>, String> {
    if let Ok(mut file) = File::open(filename) {
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        Scanner::new(data).scan_tokens()
        //println!("{:?}", Scanner::new(data).scan_tokens()?);
        //Ok(())
    } else {
        Err("could not read file".into())
    }
}

