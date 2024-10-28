use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Float(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(float) => write!(f, "{}", float),
        }
    }
}
