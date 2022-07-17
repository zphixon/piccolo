extern crate piccolo;

use piccolo::{error::PiccoloError, make_error};
use std::{fs, io, path};

fn main() -> Result<(), PiccoloError> {
    #[cfg(feature = "logging")]
    my_log::init();

    let mut path = path::PathBuf::new();
    path.push("examples");
    path.push("test_files");
    let mut files = Vec::new();
    collect_files_recursively(&path, &mut files)?;

    println!("found {} tests", files.len());

    let mut ok_results = 0;
    let mut ignored = 0;
    let mut test_errors = Vec::new();
    for item in files.iter() {
        let name = item.display().to_string();
        if name.ends_with("ignore") {
            ignored += 1;
        } else if !name.ends_with("_fail.pc") && name.ends_with(".pc") {
            println!(" -- '{name}'");
            match piccolo::do_file(item) {
                Ok(_) => ok_results += 1,
                Err(errors) => test_errors.push(errors),
            }
        } else if name.ends_with("_fail.pc") {
            println!(" xx '{name}'");
            match piccolo::do_file(item) {
                Ok((env, v)) => test_errors.push(vec![make_error!(AssertFailed {
                    assertion: String::from("test file would fail"),
                })
                .file(name)
                .msg_string(format!("resulted in {}", env.format(v)))]),
                Err(_) => ok_results += 1,
            }
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
                println!("        {e}");
            }
        }
    }

    println!(
        "\nreported {} successful, {} failures, {ignored} ignored",
        ok_results - test_errors.len(),
        test_errors.len(),
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
