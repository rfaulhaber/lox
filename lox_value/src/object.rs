use lox_bytecode::Function;

use crate::native::NativeFunction;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFunction),
}
