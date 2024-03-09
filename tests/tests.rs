use anyhow::Result;
use lox::{interpreter::Interpreter, parser::ast::program::Program, parser::Parser};

pub struct TestCase<'t> {
    pub name: String,
    pub code: String,
    pub expected: String,
    pub interpreter: Interpreter<'t>,
    pub ast: Program,
    pub writer: Box<String>,
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

        let writer = Box::new(String::new());

        let interpreter = Interpreter::new(Box::new(&b""[..]), writer.clone());

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
            writer,
        })
    }
}
