use anyhow::Result;
use rustyline::{error::ReadlineError, DefaultEditor, Result as RlResult};
use std::io::Read;
use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, Parser)]
#[command(version, about)]
pub struct Args {
    #[arg(long)]
    pub r#use: Option<UseOption>,

    pub file: Option<PathBuf>,
}

#[derive(Debug, Clone, ValueEnum)]
pub enum UseOption {
    VM,
    JIT,
}

pub fn repl() -> RlResult<()> {
    let mut rl = DefaultEditor::new()?;

    let vm = lox_vm::vm::Interpreter::new();

    loop {
        let readline = rl.readline("lox >>");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                println!("Line: {}", line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

fn eval_file(path: String) -> Result<()> {
    let vm = lox_vm::vm::Interpreter::new();

    let mut source_file = std::fs::File::open(path)?;
    let mut source_buf = String::new();
    source_file.read_to_string(&mut source_buf)?;

    let program = lox_source::parser::Parser::from_source(&source_buf).parse()?;

    todo!();
}
