use thiserror::Error;

use super::value::LoxValue;

#[derive(Debug, PartialEq, Error)]
pub enum EvalError {
    #[error("Incompatible types for operation {0}: {1}, {2}")]
    TypeMismatch(String, LoxValue, LoxValue),
    #[error("Reference to undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Not callable: {0}")]
    NotCallable(LoxValue),
    #[error("Function {0} not called with enough arguments (expecting {1} but got {2})")]
    NotEnoughArguments(String, usize, usize),
    #[error("Internal return")]
    InternalReturn(LoxValue),
}
