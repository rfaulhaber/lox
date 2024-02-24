use anyhow::Result;
use lox::parser::ast::printer::AstPrinter;
use lox::parser::Parser;
use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use thiserror::Error;

#[derive(Error, Debug)]
enum PrintError {
    #[error("No file specified")]
    NoFileSpecified,
    #[error("Could not open file")]
    OpenFileError(#[from] io::Error),
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    println!("debug: args: {:?}", args);

    if args.len() <= 1 {
        return Err(PrintError::NoFileSpecified.into());
    }

    let file = args.get(1).unwrap();

    let mut code_file = File::open(file)?;
    let mut code = String::new();

    code_file.read_to_string(&mut code)?;

    let ast = Parser::from_source(&code).parse()?;

    let ast_printed = AstPrinter::new().print(ast);

    println!("{}", ast_printed);

    Ok(())
}
