use crate::error::EvalError;

mod builtins;
mod env;
mod error;
mod interpreter;
mod value;

pub use interpreter::Interpreter;
pub use value::{Function, LoxValue, NativeFunction};

pub type EvalResult = Result<LoxValue, EvalError>;
