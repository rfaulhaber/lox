use anyhow::Result;
use rustyline::{error::ReadlineError, DefaultEditor, Result as RlResult};
use std::io::Write;
use std::path::PathBuf;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

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
        #[arg(short, long)]
        #[arg(default_value_t = VmOptions::Bytecode)]
        vm: VmOptions,
        #[arg(short, long)]
        #[arg(help = "Print bytecode with each line")]
        bytecode: bool,
    },
    #[command(arg_required_else_help = true)]
    Eval {
        #[arg(required = true)]
        file: PathBuf,
        #[arg(short, long)]
        #[arg(help = "Print bytecode with each line")]
        bytecode: bool,
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

pub struct ReplOptions {
    pub bytecode: bool,
}

pub fn repl(options: ReplOptions) -> RlResult<()> {
    let mut rl = DefaultEditor::new()?;

    let mut vm = lox_vm::Interpreter::new_with_writer(std::io::stdout());

    let mut err_spec = ColorSpec::new();
    err_spec.set_fg(Some(Color::Red));
    err_spec.set_bold(true);

    let mut std_spec = ColorSpec::new();
    std_spec.set_fg(None);

    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    loop {
        let readline = rl.readline("lox>> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;

                if line == "exit" {
                    break;
                }

                let compiler = match lox_compiler::Compiler::new_from_source(&line) {
                    Ok(comp) => comp,
                    Err(e) => {
                        stdout.set_color(&err_spec)?;
                        writeln!(&mut stdout, "compile error: {}", e)?;
                        stdout.reset()?;
                        stdout.flush()?;
                        continue;
                    }
                };
                let bytecode = compiler.compile().expect("compilation failed");

                if options.bytecode {
                    writeln!(&mut stdout, "{}", bytecode.disassemble().join("\n"))?;
                }

                match vm.eval(bytecode) {
                    Err(e) => {
                        stdout.set_color(&err_spec)?;
                        writeln!(&mut stdout, "error: {}", e)?;
                        stdout.reset()?;
                        stdout.flush()?;
                    }
                    _ => {}
                };
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

pub fn eval_file(vm: VmOptions, print_bytecode: bool, file: PathBuf) -> Result<()> {
    let file_contents = std::fs::read_to_string(file).expect("Could not open file");

    let mut stdout = StandardStream::stdout(ColorChoice::Always);

    match vm {
        VmOptions::Bytecode => {
            let compiler = lox_compiler::Compiler::new_from_source(&file_contents)?;

            let bytecode = compiler.compile().expect("compilation failed");

            if print_bytecode {
                writeln!(&mut stdout, "{}", bytecode.disassemble().join("\n"))?;
            }

            let mut vm = lox_vm::Interpreter::new_with_writer(std::io::stdout());

            let _ = vm.eval(bytecode)?;

            Ok(())
        }
        VmOptions::TreeWalk => todo!("treewalk eval not implemented"),
    }
}

pub fn disassemble(path: PathBuf) -> Result<()> {
    let file_contents = std::fs::read_to_string(path).expect("Could not open file");

    let compiler = lox_compiler::Compiler::new_from_source(&file_contents)?;

    let res = compiler.compile().expect("compilation failed");

    let dsm = res.disassemble();

    println!("{}", dsm.join("\n"));

    Ok(())
}
