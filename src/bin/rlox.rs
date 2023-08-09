use std::{env, fs, io};

use anyhow::Result;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as RustylineResult};
use thiserror::Error;

type EvalResult = Result<(), EvalError>;

#[derive(Error, Debug)]
enum EvalError {
    #[error("Usage: rlox [script]")]
    InvalidUsage,
    #[error("could not read file")]
    OpenFileError(#[from] io::Error),
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    match args[2..].len() {
        1 => {
            let result = eval_file(args.get(0).unwrap().to_owned())?;
            Ok(result)
        }
        i if i > 1 => Err(EvalError::InvalidUsage.into()),
        _ => {
            let res = start_repl()?;
            Ok(res)
        }
    }
}

fn eval_file(path: String) -> Result<(), EvalError> {
    let file = fs::read_to_string(path)?;
    todo!("eval file");
}

fn start_repl() -> RustylineResult<()> {
    let mut rl = DefaultEditor::new()?;
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                println!("Line: {}", line);
                // TODO eval(line)
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
