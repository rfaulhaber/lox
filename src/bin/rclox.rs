use anyhow::Result;
use clap::{Parser, Subcommand};
use rustyline::{error::ReadlineError, DefaultEditor, Result as RlResult};
use std::io::Read;

#[derive(Debug, Parser)]
#[command(about)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Eval {
        #[arg(long, short)]
        file: String,
    },
    Repl {},
}

fn main() -> Result<()> {
    let args = Args::parse();

    match args.command {
        Command::Eval { file } => todo!(),
        Command::Repl {} => todo!(),
    }
}

fn repl() -> RlResult<()> {
    let mut rl = DefaultEditor::new()?;

    let vm = lox::vm::Interpreter::new();

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
    let vm = lox::vm::Interpreter::new();

    let mut source_file = std::fs::File::open(path)?;
    let mut source_buf = String::new();
    source_file.read_to_string(&mut source_buf)?;

    let program = lox::parser::Parser::from_source(&source_buf).parse()?;

    todo!();
}
