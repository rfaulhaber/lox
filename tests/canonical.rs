use std::io::{BufRead, BufReader};
use std::process::Command;

#[ignore]
#[test]
fn interpreter_canonical_tests() {
    let mut cmd = Command::new("dart")
        .arg("./craftinginterpreters/tool/bin/test.dart")
        .arg("jlox")
        .arg("--interpreter")
        .arg("./target/debug/rlox")
        .spawn()
        .expect("could not spawn");

    let mut reader = BufReader::new(cmd.stdout.take().expect("could not take stdout"));

    for line in reader.lines() {
        println!("{}", line.unwrap().trim());
    }
}
