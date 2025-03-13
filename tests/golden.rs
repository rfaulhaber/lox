use lox_compiler::Compiler;
use lox_vm::Interpreter;
use std::{fs, path::PathBuf};

struct GoldenTest<'s> {
    test_name: &'s str,
    test_file: PathBuf,
}

impl<'s> GoldenTest<'s> {
    fn new(test_name: &'s str, test_file: PathBuf) -> Self {
        Self {
            test_name,
            test_file,
        }
    }

    fn run(self) {
        let in_contents =
            std::fs::read_to_string(self.test_file).expect("Should have been able to read in file");

        let mut out_contents = String::new();

        let bytecode = Compiler::new_from_source(&*in_contents)
            .expect("Could not create compiler")
            .compile()
            .expect("Could not compile code");

        let mut interpreter = Interpreter::new_with_writer(&mut out_contents);

        let _ = interpreter.eval(bytecode).expect("Could not evaluate file");

        if !fs::exists("./golden_tests").unwrap() {
            fs::create_dir("./golden_tests").expect("Could not create golden tests file");
        }

        fs::write(
            format!("./golden_tests/{}_output.txt", self.test_name),
            out_contents,
        );
    }
}

#[test]
fn works() {
    assert_eq!(2 + 2, 4);
}
