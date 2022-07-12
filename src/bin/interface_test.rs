#[cfg(feature = "log")]
fn main() {
    panic!("must be run without log feature enabled");
}

#[cfg(not(feature = "log"))]
mod not_log {
    use std::{
        fs, io,
        path::{Path, PathBuf},
        process::Command,
    };

    fn collect_files_recursively<'a, P: AsRef<Path> + ?Sized>(
        dir: &P,
        vec: &'a mut Vec<PathBuf>,
    ) -> Result<&'a mut Vec<PathBuf>, io::Error> {
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

    fn check_rebuild() {
        let binary_modified = std::fs::metadata("target/release/libpiccolo.rlib")
            .unwrap()
            .modified()
            .unwrap();

        let mut source_files = Vec::new();
        collect_files_recursively("src", &mut source_files).unwrap();
        for file in source_files {
            let source_modified = std::fs::metadata(&file).unwrap().modified().unwrap();
            if source_modified > binary_modified {
                panic!("release rebuild required (with no log feature)");
            }
        }
    }

    fn do_one(name: &str, file: &Path) {
        let output = Command::new("target/release/simple")
            .arg(name)
            .output()
            .unwrap();
        println!("writing {name}");
        std::fs::write(file.with_extension("out"), output.stdout).unwrap();
    }

    fn write_all(dir: &str) {
        let mut test_files = Vec::new();
        collect_files_recursively(dir, &mut test_files).unwrap();

        for file in test_files {
            let name = file.display().to_string();

            if name.ends_with(".pc") {
                do_one(&name, &file);
            }
        }
    }

    fn clean(dir: &str) {
        let mut test_files = Vec::new();
        collect_files_recursively(dir, &mut test_files).unwrap();

        for file in test_files {
            let name = file.display().to_string();

            if name.ends_with(".out") {
                println!("remove {name}");
                std::fs::remove_file(file).unwrap();
            }
        }
    }

    fn check_all(dir: &str) {
        let mut test_files = Vec::new();
        collect_files_recursively(dir, &mut test_files).unwrap();

        for file in test_files {
            let name = file.display().to_string();
            if name.ends_with(".pc") {
                println!("checking {name}");
                let output = Command::new("target/release/simple")
                    .arg(name)
                    .output()
                    .unwrap();
                let result = std::str::from_utf8(&output.stdout).unwrap();
                let result_should_be = std::fs::read_to_string(file.with_extension("out")).unwrap();
                if result != result_should_be {
                    println!("failed: {result:?}");
                    println!("        {result_should_be:?}");
                }
            }
        }
    }

    pub fn main() {
        check_rebuild();
        let dir = "examples/test_files";

        let args = std::env::args().collect::<Vec<_>>();
        let first = args.get(1).map(|s| s.as_str());
        if matches!(first, Some("-h" | "--help")) {
            println!("--write-all\trewrite every test file output");
            println!("--clean\t\tremove every test file output");
            println!("[filename]\trewrite single filename");
            println!("no args\t\tcheck all output");
        } else if first == Some("--write-all") {
            write_all(dir);
        } else if first == Some("--clean") {
            clean(dir);
        } else if let Some(filename) = args.get(1) {
            let path = PathBuf::from(filename);
            do_one(filename, &path);
        } else {
            check_all(dir);
        }
    }
}

#[cfg(not(feature = "log"))]
fn main() {
    not_log::main();
}