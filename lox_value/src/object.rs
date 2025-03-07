use lox_bytecode::Function;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function(Function),
}
