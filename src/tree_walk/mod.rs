use crate::tree_walk::error::EvalError;

mod env;
mod error;
mod interpreter;
mod value;

pub use interpreter::Interpreter;
pub use value::{Function, LoxValue, NativeFunction};

pub type EvalResult = Result<LoxValue, EvalError>;
