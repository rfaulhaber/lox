use std::{
    cell::RefCell,
    cmp::Ordering,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::parser::ast::stmt::Stmt;

use super::{env::Env, EvalError, EvalResult};

macro_rules! binary_number_arithmetic_impl {
    ($func_name:ident, $op_name:ident) => {
        pub fn $func_name(self, rhs: Self) -> EvalResult {
            match (self.clone(), rhs.clone()) {
                (LoxValue::Int(l), LoxValue::Int(r)) => Ok(LoxValue::Int(l.$op_name(r))),
                (LoxValue::Float(l), LoxValue::Float(r)) => Ok(LoxValue::Float(l.$op_name(r))),
                (LoxValue::Int(l), LoxValue::Float(r)) => {
                    Ok(LoxValue::Float((l as f64).$op_name(r)))
                }
                (LoxValue::Float(l), LoxValue::Int(r)) => {
                    Ok(LoxValue::Float(l.$op_name((r as f64))))
                }
                _ => Err(EvalError::TypeMismatch(
                    "arithmetic".into(),
                    self.clone(),
                    rhs.clone(),
                )),
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Callable(Callable),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Int(i) => write!(f, "{}", i),
            LoxValue::Float(fl) => write!(f, "{}", fl),
            LoxValue::String(s) => write!(f, "\"{}\"", s),
            LoxValue::Callable(c) => write!(f, "{}", c),
        }
    }
}

impl Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Callable::Native { name, .. } => {
                write!(f, "<builtin func {}>", name)
            }
            Callable::Function { name, .. } => {
                write!(f, "<func {}>", name)
            }
        }
    }
}

impl From<i64> for LoxValue {
    fn from(value: i64) -> Self {
        LoxValue::Int(value)
    }
}

impl From<f64> for LoxValue {
    fn from(value: f64) -> Self {
        LoxValue::Float(value)
    }
}

impl From<String> for LoxValue {
    fn from(value: String) -> Self {
        LoxValue::String(value)
    }
}

impl<'s> From<&'s str> for LoxValue {
    fn from(value: &'s str) -> Self {
        LoxValue::String(String::from(value))
    }
}

impl From<bool> for LoxValue {
    fn from(value: bool) -> Self {
        LoxValue::Bool(value)
    }
}

impl From<Callable> for LoxValue {
    fn from(value: Callable) -> Self {
        LoxValue::Callable(value)
    }
}

impl LoxValue {
    // binary_number_arithmetic_impl!(try_add, add);
    binary_number_arithmetic_impl!(try_sub, sub);
    binary_number_arithmetic_impl!(try_mul, mul);
    binary_number_arithmetic_impl!(try_div, div);

    pub fn try_add(self, rhs: Self) -> EvalResult {
        match (self.clone(), rhs.clone()) {
            (LoxValue::Int(l), LoxValue::Int(r)) => Ok(LoxValue::Int(l.add(r))),
            (LoxValue::Float(l), LoxValue::Float(r)) => Ok(LoxValue::Float(l.add(r))),
            (LoxValue::Int(l), LoxValue::Float(r)) => Ok(LoxValue::Float((l as f64).add(r))),
            (LoxValue::Float(l), LoxValue::Int(r)) => Ok(LoxValue::Float(l.add(r as f64))),
            (LoxValue::String(s), right) => {
                let right_str: String = match right {
                    LoxValue::Nil => "nil".into(),
                    LoxValue::Bool(true) => "true".into(),
                    LoxValue::Bool(false) => "false".into(),
                    LoxValue::Int(i) => i.to_string(),
                    LoxValue::Float(f) => f.to_string(),
                    LoxValue::String(s) => s,
                    LoxValue::Callable(c) => format!("{}", c),
                };

                let mut new_str = String::new();
                new_str.push_str(&s);
                new_str.push_str(&right_str);

                Ok(LoxValue::String(new_str))
            }
            _ => Err(EvalError::TypeMismatch(
                "arithmetic".into(),
                self.clone(),
                rhs.clone(),
            )),
        }
    }

    pub fn try_cmp(self, rhs: Self) -> Result<Ordering, EvalError> {
        match (self.clone(), rhs.clone()) {
            (LoxValue::Int(l), LoxValue::Int(r)) => Ok(l.cmp(&r)),
            (LoxValue::Float(l), LoxValue::Float(r)) => Ok(l.total_cmp(&r)),
            (LoxValue::Int(l), LoxValue::Float(r)) => Ok((l as f64).total_cmp(&r)),
            (LoxValue::Float(l), LoxValue::Int(r)) => Ok(l.total_cmp(&(r as f64))),
            _ => Err(EvalError::TypeMismatch("cmp".into(), self, rhs)),
        }
    }

    pub fn eq(self, rhs: Self) -> bool {
        match (self, rhs) {
            (LoxValue::Nil, LoxValue::Nil) => true,
            (LoxValue::Nil, _) => false,
            (_, LoxValue::Nil) => false,
            (LoxValue::Bool(l), LoxValue::Bool(r)) => l.eq(&r),
            (LoxValue::Bool(_), _) => false,
            (LoxValue::Int(l), LoxValue::Int(r)) => l.eq(&r),
            (LoxValue::Float(l), LoxValue::Float(r)) => l.eq(&r),
            (LoxValue::String(l), LoxValue::String(r)) => l.eq(&r),
            (LoxValue::Int(_), _) => false,
            (LoxValue::Float(_), _) => false,
            (LoxValue::String(_), _) => false,
            (
                LoxValue::Callable(Callable::Native { name: left, .. }),
                LoxValue::Callable(Callable::Native { name: right, .. }),
            ) => left == right,
            (
                LoxValue::Callable(Callable::Function { name: left, .. }),
                LoxValue::Callable(Callable::Function { name: right, .. }),
            ) => left == right,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Native {
        name: String,
        arity: usize,
        func: fn(Vec<LoxValue>) -> EvalResult,
    },
    Function {
        name: String,
        parameters: Vec<String>,
        body: Stmt,
        closure: RefCell<Env>,
    },
}

impl Callable {
    fn name(&self) -> String {
        match self {
            Callable::Native { name, .. } => name.to_string(),
            Callable::Function { name, .. } => name.to_string(),
        }
    }

    fn is_builtin(&self) -> bool {
        match self {
            Callable::Native { .. } => true,
            Callable::Function { .. } => false,
        }
    }
}
