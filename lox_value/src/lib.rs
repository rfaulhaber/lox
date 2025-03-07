use lox_bytecode::Function;
pub use number::Number;
pub use object::{Object};
use thiserror::Error;

mod number;
mod object;

#[derive(Debug, Clone, Error, PartialEq)]
pub enum ValueOperatorError {
    #[error("Cannot use {0} between {1} and {2}")]
    IncompatibleTypes(String, String, String),
    #[error("Cannot apply {0} to {1}")]
    IncompatibleUnaryOperation(String, String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Number),
    Bool(bool),
    Nil,
    Object(Object),
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Number(Number::Float(value))
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Value::Number(Number::Int(value))
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Object(Object::String(value))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Object(Object::Function(value))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Nil => String::from("nil"),
                Value::Object(Object::String(s)) => format!("\"{}\"", s),
                Value::Object(Object::Function(f)) => match f.name() {
                    Some(name) => format!("function {}/{}", name, f.arity()),
                    None => format!("function anonymous/{}", f.arity()),
                },
            }
        )
    }
}

impl std::ops::Add for Value {
    type Output = Result<Value, ValueOperatorError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (Value::Object(Object::String(left)), Value::Object(Object::String(right))) => {
                Ok(Value::from(left + &right))
            }
            (left, right) => Err(ValueOperatorError::IncompatibleTypes(
                "+".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Result<Value, ValueOperatorError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
            (left, right) => Err(ValueOperatorError::IncompatibleTypes(
                "-".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Value, ValueOperatorError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
            (left, right) => Err(ValueOperatorError::IncompatibleTypes(
                "*".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Value, ValueOperatorError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
            (left, right) => Err(ValueOperatorError::IncompatibleTypes(
                "/".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Value, ValueOperatorError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            val => Err(ValueOperatorError::IncompatibleUnaryOperation(
                "negation".into(),
                val.to_string(),
            )),
        }
    }
}

impl Value {
    pub fn is_falsy(&self) -> bool {
        match self {
            Value::Bool(false) | Value::Nil => true,
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Number(left), Value::Number(right)) => left.partial_cmp(right),
            (Value::Object(Object::String(left)), Value::Object(Object::String(right))) => {
                left.partial_cmp(right)
            }
            _ => None,
        }
    }
}
