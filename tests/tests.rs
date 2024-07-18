use anyhow::Result;
use lox::interpreter::Interpreter;
use lox::{parser::ast::program::Program, parser::Parser};

#[macro_export]
macro_rules! make_interpreter_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let test_case = TestCase::new(stringify!($name)).unwrap();

            let TestCase {
                mut interpreter,
                ast,
                expected,
                ..
            } = test_case;

            let result = interpreter.eval(ast);

            assert!(result.is_ok(), "received err result: {:?}", result);

            let output = interpreter.get_output();

            assert_eq!(*output, expected);
        }
    };

    ($name:ident, $override:expr) => {
        #[test]
        fn $name() {
            let test_case = TestCase::new(stringify!($name)).unwrap();

            let TestCase {
                mut interpreter,
                ast,
                ..
            } = test_case;

            let result = interpreter.eval(ast);

            assert!(result.is_ok());

            let output = interpreter.get_output();

            $override(output);
        }
    };
}

pub struct TestCase<'t> {
    pub name: String,
    pub code: String,
    pub expected: String,
    pub interpreter: Interpreter<&'t [u8], String>,
    pub ast: Program,
}

impl<'t> TestCase<'t> {
    pub fn new<'f>(name: &'f str) -> Result<Self> {
        use std::fs::File;
        use std::io::prelude::*;
        use std::path::Path;

        let formatted_code = &format!("./fixtures/{}/code.lox", name);
        let formatted_expected = &format!("./fixtures/{}/expected.txt", name);

        let code_path = Path::new(formatted_code);
        let expected_path = Path::new(formatted_expected);

        let mut code_file = File::open(code_path)?;
        let mut expected_file = File::open(expected_path)?;

        let interpreter = Interpreter::new(&b""[..], String::new());

        let mut code = String::new();
        let mut expected = String::new();

        code_file.read_to_string(&mut code)?;
        expected_file.read_to_string(&mut expected)?;

        let ast = Parser::from_source(&code).parse()?;

        Ok(Self {
            name: name.into(),
            code,
            expected,
            ast,
            interpreter,
        })
    }
}
