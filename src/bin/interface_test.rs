fn main() {
    #[cfg(not(all(feature = "cli", not(feature = "log"))))]
    panic!("Must be run with features cli and not log");

    #[cfg(all(feature = "cli", not(feature = "log")))]
    not_log::main();
}

#[cfg(all(feature = "cli", not(feature = "log")))]
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
            if file.parent().unwrap().ends_with("bin") && !file.ends_with("simple.rs") {
                continue;
            }

            let source_modified = std::fs::metadata(&file).unwrap().modified().unwrap();
            if source_modified > binary_modified {
                let name = file.display().to_string();
                panic!("{name} was modified: release rebuild required (with no log feature)");
            }
        }
    }

    fn do_one(name: &str, file: &Path) {
        let output = Command::new("target/release/simple")
            .arg(name)
            .output()
            .unwrap();

        let contents = std::str::from_utf8(&output.stdout).unwrap();
        println!("writing {name}\n{}", contents);

        std::fs::write(file.with_extension("out"), output.stdout).unwrap();
    }

    fn check_all() {
        let mut test_files = Vec::new();
        collect_files_recursively("examples/test_files", &mut test_files).unwrap();

        let mut skipped = Vec::new();
        let mut failed = Vec::new();

        for file in test_files {
            let name = file.display().to_string();

            let mut output_path = PathBuf::from("examples/output");
            output_path.push(
                file.with_extension("out")
                    .strip_prefix("examples/test_files")
                    .unwrap(),
            );

            if !name.ends_with("ignore") && !output_path.exists() {
                skipped.push(name);
                continue;
            }

            if name.ends_with(".pc") {
                println!("checking {name}");
                let output = Command::new("target/release/simple")
                    .arg(&name)
                    .output()
                    .unwrap();
                let result = std::str::from_utf8(&output.stdout).unwrap();
                let result_should_be = std::fs::read_to_string(output_path).unwrap();
                if result != result_should_be {
                    failed.push((name, result.to_string(), result_should_be))
                }
            }
        }

        if !skipped.is_empty() {
            println!();
            for skipped in skipped {
                println!("skipped {skipped}");
            }
        }

        if !failed.is_empty() {
            println!();
            for (name, result, rsb) in failed {
                println!("failed {name}");
                println!("expected {rsb:?}");
                println!("got      {result:?}");
            }
        }
    }

    pub fn main() {
        check_rebuild();

        let args = std::env::args().collect::<Vec<_>>();
        let first = args.get(1).map(|s| s.as_str());
        if matches!(first, Some("-h" | "--help")) {
            println!("[filename]\trewrite single filename");
            println!("no args\t\tcheck all output");
        } else if let Some(filename) = args.get(1) {
            let path = PathBuf::from(filename);
            do_one(filename, &path);
        } else {
            check_all();
        }
    }
}
