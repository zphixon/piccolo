use std::{fs, io, path};

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

fn main() {
    #[cfg(feature = "log")]
    my_log::init();

    let mut test_files = Vec::new();
    collect_files_recursively("examples", &mut test_files).unwrap();

    for file in test_files {
        let name = file.display().to_string();
        if !name.ends_with("_fail.pc") && name.ends_with(".pc") {
            println!(" -- {name}");
            let src = fs::read_to_string(&name).unwrap();
            let ast = piccolo::compiler::parser::parse(&src).unwrap();
            if let Err(e) = piccolo::compiler::emitter::analyze_ns(&ast) {
                println!("{}", e);
            }
        } else if name.ends_with("_fail.pc") {
            let src = fs::read_to_string(&name).unwrap();
            if let Ok(ast) = piccolo::compiler::parser::parse(&src) {
                println!(" xx {name}");
                let result = piccolo::compiler::emitter::analyze_ns(&ast);
                match result {
                    Ok(()) => println!("    did not fail!"),
                    _ => {}
                }
            } else {
                println!(" .. (parse failed) {name}");
            }
        }
    }
}
