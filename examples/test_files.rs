extern crate piccolo;

use piccolo::{ErrorKind, PiccoloError};

use std::fs;
use std::{io, path};

fn main() -> Result<(), PiccoloError> {
    let path: path::PathBuf = "examples\\test_files".into();
    let mut files = Vec::new();
    collect_files_recursively(&path, &mut files)?;

    println!("found {} tests", files.len());

    let mut ignored = 0;
    let mut file_tokens_errors = Vec::new();
    for item in files.iter() {
        let name = item.display().to_string();
        if name.ends_with("ignore") {
            ignored += 1;
        } else if !name.ends_with("_fail.pc") {
            println!(" -- '{}'", name);
            let _ = piccolo::do_file(&item).map_err(|errors| {
                file_tokens_errors.push((name, errors));
            });
        } else {
            println!(" xx '{}'", name);
            let _ = piccolo::do_file(&item).map(|v| {
                file_tokens_errors.push((
                    name.clone(),
                    vec![PiccoloError::new(ErrorKind::AssertFailed)
                        .file(name)
                        .msg_string(format!(
                            "should have failed to complete, but resulted in {:?}",
                            v
                        ))],
                ))
            });
        }
    }

    println!();

    for (file, errors) in file_tokens_errors.iter() {
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

    println!(
        "reported {} successful, {} failures, {} ignored",
        files.len() - file_tokens_errors.len() - ignored,
        file_tokens_errors.len(),
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
