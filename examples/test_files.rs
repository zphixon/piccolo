extern crate piccolo;

use piccolo::prelude::*;

use std::fs;
use std::{io, path};

fn main() -> Result<(), PiccoloError> {
    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push("test_files");
    let mut files = Vec::new();
    collect_files_recursively(&path, &mut files)?;

    println!("found {} tests", files.len());

    let mut ignored = 0;
    let mut test_errors = Vec::new();
    for item in files.iter() {
        let name = item.display().to_string();
        if name.ends_with("ignore") {
            ignored += 1;
        } else if !name.ends_with("_fail.pc") {
            println!(" -- '{}'", name);
            let _ = piccolo::do_file(&item).map_err(|errors| {
                test_errors.push(errors);
            });
        } else {
            println!(" xx '{}'", name);
            let _ = piccolo::do_file(&item).map(|v| {
                test_errors.push(vec![PiccoloError::new(ErrorKind::AssertFailed {
                    assertion: String::from("test file would fail"),
                })
                .file(name)
                .msg_string(format!("resulted in {}", v))])
            });
        }
    }

    println!();

    for errors in test_errors.iter() {
        print!(" XX ");
        if errors.len() == 1 {
            println!("Error {}", errors[0])
        } else {
            println!("{} Errors:", errors.len());
            for e in errors.iter() {
                println!("        {}", e);
            }
        }
    }

    println!(
        "\nreported {} successful, {} failures, {} ignored",
        files.len() - test_errors.len() - ignored,
        test_errors.len(),
        ignored,
    );

    Ok(())
}

fn collect_files_recursively<'a>(
    dir: &path::Path,
    vec: &'a mut Vec<path::PathBuf>,
) -> Result<&'a mut Vec<path::PathBuf>, io::Error> {
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        if !path.is_dir() {
            vec.push(path);
        } else {
            collect_files_recursively(&path, vec)?;
        }
    }
    Ok(vec)
}
