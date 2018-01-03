
extern crate piccolo;

use std::fs::File;
use std::io::Read;

fn main() {
    let filename = std::env::args().last().unwrap();
    if let Ok(mut file) = File::open(filename) {
        let mut data = String::new();
        file.read_to_string(&mut data).unwrap();
        match piccolo::evaluate(&data) {
            Ok(_) => {},
            Err(e) => {
                for err in e {
                    eprintln!("{}", err);
                }
            }
        }
    } else {
        eprintln!("Could not read file");
    }
}

