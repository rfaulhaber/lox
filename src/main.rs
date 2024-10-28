use clap::Parser as _;
use lox::Args;

fn main() {
    let args = Args::parse();

    if let Some(path) = args.file {
        println!("eval file! {:?}", path);
    } else {
        println!("start repl!");
    }
}
