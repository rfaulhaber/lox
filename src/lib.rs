use anyhow::Result;
use rustyline::{error::ReadlineError, DefaultEditor, Result as RlResult};
use std::io::Read;
use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};

#[derive(Debug, Clone, Parser)]
#[command(name = "rclox")]
#[command(about = "clox implementation in Rust")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Debug, Clone, Subcommand)]
pub enum Commands {
    #[command(arg_required_else_help = true)]
    #[command(name = "dsm")]
    #[command(about = "dissassemble a file")]
    Disassemble {
        #[arg(required = true)]
        file: PathBuf,
    },
    #[command(about = "run repl")]
    Repl {
        #[arg(default_value_t = VmOptions::Bytecode)]
        vm: VmOptions,
    },
    #[command(arg_required_else_help = true)]
    Eval {
        #[arg(required = true)]
        file: PathBuf,
        #[arg(default_value_t = VmOptions::Bytecode)]
        vm: VmOptions,
    },
}

#[derive(Debug, Clone, ValueEnum)]
pub enum VmOptions {
    Bytecode,
    TreeWalk,
}

impl std::fmt::Display for VmOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.to_possible_value()
            .expect("no values are skipped")
            .get_name()
            .fmt(f)
    }
}

pub fn repl() -> RlResult<()> {
    let mut rl = DefaultEditor::new()?;

    let mut vm = lox_vm::Interpreter::new();

    loop {
        let readline = rl.readline("lox>> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                if line == "exit" {
                    break;
                }

                let compiler = lox_compiler::Compiler::new_from_source(&line)
                    .expect("could not create compiler");
                let bytecode = compiler.compile().expect("compilation failed");

                let _ = vm.eval(bytecode).expect("eval failed");
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
    let vm = lox_vm::Interpreter::new();

    let mut source_file = std::fs::File::open(path)?;
    let mut source_buf = String::new();
    source_file.read_to_string(&mut source_buf)?;

    let program = lox_source::parser::Parser::from_source(&source_buf).parse()?;

    todo!();
}
