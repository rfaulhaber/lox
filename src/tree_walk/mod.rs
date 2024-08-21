use crate::tree_walk::error::EvalError;
use crate::tree_walk::value::LoxValue;

mod env;
mod error;
mod interpreter;
mod value;

pub use interpreter::Interpreter;

pub type EvalResult = Result<LoxValue, EvalError>;
