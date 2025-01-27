pub use number::Number;
use thiserror::Error;

mod number;

#[derive(Debug, Clone, Error, PartialEq)]
pub enum ValueArithmeticError {
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Nil => String::from("nil"),
            }
        )
    }
}

impl std::ops::Add for Value {
    type Output = Result<Value, ValueArithmeticError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
            (left, right) => Err(ValueArithmeticError::IncompatibleTypes(
                "+".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Sub for Value {
    type Output = Result<Value, ValueArithmeticError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
            (left, right) => Err(ValueArithmeticError::IncompatibleTypes(
                "-".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Mul for Value {
    type Output = Result<Value, ValueArithmeticError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
            (left, right) => Err(ValueArithmeticError::IncompatibleTypes(
                "*".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Div for Value {
    type Output = Result<Value, ValueArithmeticError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
            (left, right) => Err(ValueArithmeticError::IncompatibleTypes(
                "/".into(),
                left.to_string(),
                right.to_string(),
            )),
        }
    }
}

impl std::ops::Neg for Value {
    type Output = Result<Value, ValueArithmeticError>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Number(n) => Ok(Value::Number(-n)),
            val => Err(ValueArithmeticError::IncompatibleUnaryOperation(
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
