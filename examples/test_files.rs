extern crate piccolo;

use std::fs;
use std::{io, path};

fn main() -> io::Result<()> {
    let path: path::PathBuf = "examples\\test_files".into();
    let mut files = vec![];
    collect_files_recursively(&path, &mut files)?;

    println!("found {} tests", files.len());

    let mut file_tokens_errors = vec![];
    for item in files {
        println!(" -- '{}'", item.display());
        let _ = piccolo::do_file(&item)?.map_err(|errors| {
            file_tokens_errors.push((item.display().to_string(), errors));
        });
    }

    println!("\nreported {} failures", file_tokens_errors.len());

    for (file, errors) in file_tokens_errors {
        println!(" -- test '{}' failed", file);
        if errors.len() == 1 {
            println!("        Error {}", errors[0])
        } else {
            println!("        {} Errors:", errors.len());
            for e in errors.iter() {
                println!("            {}", e);
            }
        }
    }

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
