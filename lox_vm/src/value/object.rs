use super::{ Function, NativeFunction, Closure };

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function(Function),
    Native(NativeFunction),
    Closure(Closure),
}
