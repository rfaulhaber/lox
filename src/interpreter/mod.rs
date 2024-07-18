use crate::interpreter::error::EvalError;
use crate::interpreter::value::LoxValue;

mod env;
mod error;
mod interpreter;
mod resolver;
mod value;

pub use interpreter::Interpreter;

pub type EvalResult = Result<LoxValue, EvalError>;
