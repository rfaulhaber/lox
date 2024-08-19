use anyhow::Result;
use lox::interpreter::Interpreter;
use lox::{parser::ast::program::Program, parser::Parser};

#[macro_export]
macro_rules! make_interpreter_test_inner {
    ($name:ident) => {
        let test_case = TestCase::new(stringify!($name)).unwrap();

        let TestCase {
            mut interpreter,
            ast,
            expected,
            ..
        } = test_case;

        let result = interpreter.eval(ast);

        if !result.is_ok() {
            println!("interpreter backtrace: {:?}", interpreter.backtrace());
        }

        assert!(result.is_ok(), "received err result: {:?}", result);

        let output = interpreter.output();

        assert_eq!(*output, expected);
    };

    ($name:ident, $override:expr) => {
        let test_case = TestCase::new(stringify!($name)).unwrap();

        let TestCase {
            mut interpreter,
            ast,
            ..
        } = test_case;

        let result = interpreter.eval(ast);

        if !result.is_ok() {
            println!("interpreter backtrace: {:?}", interpreter.backtrace());
        }

        assert!(result.is_ok(), "received err result: {:?}", result);

        let output = interpreter.output();

        $override(output);
    };
}

#[macro_export]
macro_rules! make_interpreter_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            make_interpreter_test_inner!($name);
        }
    };

    ($name:ident, ignore) => {
        #[test]
        #[ignore]
        fn $name() {
            make_interpreter_test_inner!($name);
        }
    };

    ($name:ident, $override:expr) => {
        #[test]
        fn $name() {
            make_interpreter_test_inner!($name, $override);
        }
    };

    ($name:ident, $override:expr, ignore) => {
        #[test]
        #[ignore]
        fn $name() {
            make_interpreter_test_inner!($name, $override);
        }
    };
}

pub struct TestCase<'t> {
    pub expected: String,
    pub interpreter: Interpreter<&'t [u8], String, String>,
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

        let mut code = String::new();
        let mut expected = String::new();

        code_file.read_to_string(&mut code)?;
        expected_file.read_to_string(&mut expected)?;

        let interpreter = Interpreter::new(&b""[..], String::new(), code.clone(), name);

        let ast = Parser::from_source(&code).parse()?;

        Ok(Self {
            expected,
            ast,
            interpreter,
        })
    }
}
