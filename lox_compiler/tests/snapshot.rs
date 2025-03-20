use std::path::Path;

use insta::glob;
use lox_compiler::Compiler;

fn run_test(file_path: &Path) -> String {
    use std::fs;

    let in_contents = fs::read_to_string(file_path).expect("Should have been able to read in file");

    Compiler::new_from_source(&in_contents)
        .expect("Could not create compiler")
        .compile()
        .expect("Could not compile code")
        .disassemble()
        .join("\n")
}

#[test]
fn snapshots() {
    glob!("fixtures/*.lox", |path| {
        println!("running test for {:?}", path);
        insta::assert_toml_snapshot!(run_test(path));
    });
}
