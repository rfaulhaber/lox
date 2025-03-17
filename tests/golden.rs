use std::path::{Path, PathBuf};

use lox_compiler::Compiler;
use lox_vm::Interpreter;

use insta::glob;

fn run_test(file_path: &Path) -> String {
    use std::fs;

    let in_contents = fs::read_to_string(file_path).expect("Should have been able to read in file");

    let mut actual_out = Vec::<u8>::new();

    let bytecode = Compiler::new_from_source(&*in_contents)
        .expect("Could not create compiler")
        .compile()
        .expect("Could not compile code");

    let mut interpreter = Interpreter::new_with_writer(&mut actual_out);

    let _ = interpreter.eval(bytecode).expect("Could not evaluate file");

    String::from_utf8_lossy(&actual_out).into()
}

#[test]
fn golden() {
    // I know that /technically/ these aren't "golden" tests, however `insta` is
    // functionally exactly what I'd want for golden tests, so I just reuse it
    glob!("fixtures/*.lox", |path| {
        println!("running test for {:?}", path);
        insta::assert_yaml_snapshot!(run_test(path));
    });
}
