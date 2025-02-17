use clap::Parser as _;
use lox::{Cli, Commands};

// NOTE the error handling here sucks, but it's temproary! I'll add better error
// handling later. I'm just writing this now to make sure it works!

fn main() {
    let args = Cli::parse();

    match args.command {
        Commands::Disassemble { file } => lox::disassemble(file).expect("disassembly failed"),
        Commands::Repl {
            vm: _,
            print_bytecode,
        } => lox::repl(lox::ReplOptions { print_bytecode })
            .expect("repl failed :( rewrite to find out why! :)"),
        Commands::Eval { vm, file } => lox::eval_file(vm, file).expect("eval failed"),
    }
}
