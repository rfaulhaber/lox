pub use closure::Closure;
pub use function::Function;
use native::NativeFunction;
pub use number::Number;
pub use object::Object;
use thiserror::Error;

mod closure;
mod function;
pub mod native;
mod number;
mod object;

#[derive(Debug, Clone, Error, PartialEq)]
pub enum ValueOperatorError {
    #[error("Cannot use {0} between {1} and {2}")]
    IncompatibleTypes(String, String, String),
    #[error("Cannot apply {0} to {1}")]
    IncompatibleUnaryOperation(String, String),
}

#[derive(Debug, Clone, PartialEq, Error)]
pub enum ValueConvertError {
    #[error("Wrong type ({0})")]
    IncorrectType(String),
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
        Value::Object(Object::String(String::from(value.trim_matches('"'))))
    }
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Object(Object::Function(value))
    }
}

impl From<NativeFunction> for Value {
    fn from(value: NativeFunction) -> Self {
        Value::Object(Object::Native(value))
    }
}

impl From<Number> for Value {
    fn from(value: Number) -> Self {
        Value::Number(value)
    }
}

impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        Value::Object(Object::Closure(value))
    }
}

impl<'s> From<&'s str> for Value {
    fn from(value: &'s str) -> Self {
        Value::Object(Object::String(String::from(value.trim_matches('"'))))
    }
}

impl Into<Number> for lox_source::ast::expr::Number {
    fn into(self) -> Number {
        match self {
            lox_source::ast::expr::Number::Int(i) => Number::Int(i),
            lox_source::ast::expr::Number::Float(f) => Number::Float(f),
        }
    }
}

impl Into<Value> for lox_source::ast::expr::Number {
    fn into(self) -> Value {
        Value::Number(self.into())
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
                Value::Object(Object::String(s)) => format!("\"{}\"", s.trim_matches('"')),
                Value::Object(Object::Function(f)) => match f.name() {
                    Some(name) => format!("function {}/{}", name, f.arity()),
                    None => format!("function anonymous/{}", f.arity()),
                },
                Value::Object(Object::Native(f)) => {
                    format!("<native {}/{}>", f.name(), f.arity())
                }
                Value::Object(Object::Closure(cl)) => {
                    format!(
                        "<closure {}/{}>",
                        cl.name().map_or("anonymous", |v| v),
                        cl.arity()
                    )
                }
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

impl TryInto<bool> for Value {
    type Error = ValueConvertError;

    fn try_into(self) -> Result<bool, Self::Error> {
        match self {
            Value::Bool(b) => Ok(b),
            _ => Err(ValueConvertError::IncorrectType(format!("{:?}", self))),
        }
    }
}

impl TryInto<Number> for Value {
    type Error = ValueConvertError;

    fn try_into(self) -> Result<Number, Self::Error> {
        match self {
            Value::Number(n) => Ok(n),
            _ => Err(ValueConvertError::IncorrectType(format!("{:?}", self))),
        }
    }
}

impl TryInto<Function> for Value {
    type Error = ValueConvertError;

    fn try_into(self) -> Result<Function, Self::Error> {
        match self {
            Value::Object(Object::Function(f)) => Ok(f),
            _ => Err(ValueConvertError::IncorrectType(format!("{:?}", self))),
        }
    }
}

impl TryInto<Closure> for Value {
    type Error = ValueConvertError;

    fn try_into(self) -> Result<Closure, Self::Error> {
        match self {
            Value::Object(Object::Closure(c)) => Ok(c),
            _ => Err(ValueConvertError::IncorrectType(format!("{:?}", self))),
        }
    }
}

impl TryInto<String> for Value {
    type Error = ValueConvertError;

    fn try_into(self) -> Result<String, Self::Error> {
        match self {
            Value::Object(Object::String(s)) => Ok(s),
            _ => Err(ValueConvertError::IncorrectType(format!("{:?}", self))),
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
