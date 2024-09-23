use clap::{Parser, Subcommand};

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

fn main() {
    let args = Args::parse();
}

fn repl() {

}
