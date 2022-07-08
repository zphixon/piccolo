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
fn main() {
    #[cfg(feature = "log")]
    my_log::init();

    use std::str::from_utf8;

    println!("building... (again...)");
    let output = Command::new("cargo")
        .arg("b")
        .arg("--release")
        .output()
        .unwrap();

    if !output.status.success() {
        panic!(
            "{}\n{}",
            from_utf8(&output.stdout).unwrap(),
            from_utf8(&output.stderr).unwrap()
        );
    }

    println!(
        "{}\n{}",
        from_utf8(&output.stdout).unwrap(),
        from_utf8(&output.stderr).unwrap()
    );

    let mut test_files = Vec::new();
    collect_files_recursively("examples/test_files", &mut test_files).unwrap();

    for file in test_files {
        let name = file.display().to_string();

        if name.ends_with(".pc") {
            println!("{name}");
            let output = Command::new("target/release/simple")
                .arg(name)
                .output()
                .unwrap();
            let result = from_utf8(&output.stdout).unwrap();
            let result_should_be = std::fs::read_to_string(file.with_extension("out")).unwrap();
            assert_eq!(result_should_be, result, "expected LHS");
        }
    }
}
