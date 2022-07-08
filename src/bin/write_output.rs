#[cfg(not(debug_assertions))]
use std::{fs, io, path, process::Command};

#[cfg(debug_assertions)]
fn main() {
    panic!("must be run as release");
}

#[cfg(not(debug_assertions))]
fn collect_files_recursively<'a, P: AsRef<path::Path> + ?Sized>(
    dir: &P,
    vec: &'a mut Vec<path::PathBuf>,
) -> Result<&'a mut Vec<path::PathBuf>, io::Error> {
    let dir = dir.as_ref();
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

#[cfg(not(debug_assertions))]
fn do_one(name: &str, file: &path::PathBuf) {
    let output = Command::new("target/release/simple")
        .arg(name)
        .output()
        .unwrap();
    std::fs::write(file.with_extension("out"), output.stdout).unwrap();
}

#[cfg(not(debug_assertions))]
fn main() {
    #[cfg(feature = "log")]
    my_log::init();

    let mut remove = false;

    let args = std::env::args().collect::<Vec<_>>();
    if args.len() == 2 && (args[1] == "--remove" || args[1] == "-r") {
        remove = true;
    } else if args.len() == 2 {
        let file = path::PathBuf::from(&args[1]);
        do_one(&args[1], &file);
        return;
    }

    let mut test_files = Vec::new();
    collect_files_recursively("examples/test_files", &mut test_files).unwrap();

    for file in test_files {
        let name = file.display().to_string();

        if remove {
            if name.ends_with(".out") {
                println!("remove {name}");
                std::fs::remove_file(file).unwrap();
            }
            continue;
        }

        if name.ends_with(".pc") {
            println!("{name}");
            do_one(&name, &file);
        }
    }
}
