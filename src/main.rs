use clap::Parser as _;
use lox::{Cli, Commands};

// NOTE the error handling here sucks, but it's temproary! I'll add better error
// handling later. I'm just writing this now to make sure it works!

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Disassemble { file } => {
            let file_contents = std::fs::read_to_string(file).expect("Could not open file");

            let mut compiler = lox_compiler::Compiler::new_from_source(&file_contents)
                .expect("could not compile file");

            let _ = compiler.compile().expect("compilation failed");

            let dsm = compiler.bytecode().disassemble();

            println!("{}", dsm.join("\n"));
        }
        Commands::Repl { vm } => lox::repl().expect("repl failed :( rewrite to find out why! :)"),
        Commands::Eval { vm, file } => {
            let file_contents = std::fs::read_to_string(file).expect("Could not open file");
            match vm {
                lox::VmOptions::Bytecode => {
                    let mut compiler = lox_compiler::Compiler::new_from_source(&file_contents)
                        .expect("could not compile file");

                    let _ = compiler.compile().expect("compilation failed");

                    let bytecode = compiler.bytecode();

                    let mut vm = lox_vm::Interpreter::new();

                    let _ = vm.eval(bytecode).expect("eval failed");
                }
                lox::VmOptions::TreeWalk => todo!("treewalk eval not implemented"),
            }
        }
    }
}
