use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::parser::ast::stmt::Stmt;

use super::{env::Env, interpreter::Interpreter, EvalError, EvalResult};

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

pub trait Callable<R, W>
where
    R: std::io::BufRead,
    W: std::fmt::Write,
{
    fn arity(&self) -> u8;
    fn call(&self, interpreter: &mut Interpreter<R, W>, args: &[LoxValue]) -> EvalResult;
    fn name(&self) -> &str;
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Native(NativeFunction),
    Function(Function),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Int(i) => write!(f, "{}", i),
            LoxValue::Float(fl) => write!(f, "{}", fl),
            LoxValue::String(s) => write!(f, "\"{}\"", s),
            LoxValue::Native(func) => write!(f, "<builtin: {}/{}>", func.name, func.arity),
            LoxValue::Function(func) => write!(f, "<func: {}/{}>", func.name, func.arity()),
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
                    LoxValue::Native(n) => n.name,
                    LoxValue::Function(f) => f.name,
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
                LoxValue::Native(NativeFunction { name: left, .. }),
                LoxValue::Native(NativeFunction { name: right, .. }),
            ) => left == right,
            (
                LoxValue::Function(Function { name: left, .. }),
                LoxValue::Function(Function { name: right, .. }),
            ) => left == right,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub function: fn(&[LoxValue]) -> EvalResult,
}

impl<R: std::io::BufRead, W: std::fmt::Write> Callable<R, W> for NativeFunction {
    fn arity(&self) -> u8 {
        self.arity
    }

    fn call(&self, _: &mut Interpreter<R, W>, args: &[LoxValue]) -> EvalResult {
        (self.function)(args)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Stmt,
    pub closure: Env,
}

impl Function {
    fn arity(&self) -> u8 {
        self.parameters.len().try_into().unwrap()
    }
}

impl<R: std::io::BufRead, W: std::fmt::Write> Callable<R, W> for Function {
    fn arity(&self) -> u8 {
        Function::arity(&self)
    }

    fn call(&self, interpreter: &mut Interpreter<R, W>, args: &[LoxValue]) -> EvalResult {
        let args_env: HashMap<String, Option<LoxValue>> = self
            .parameters
            .iter()
            .zip(args.iter())
            .map(|(param, arg)| (param.clone(), Some(arg.clone())))
            .collect();

        let current_env = interpreter.env.clone();
        let current_ret = interpreter.ret_val.clone();

        let mut env = self.closure.clone();
        env.values.extend(current_env.values.clone());
        env.values.extend(args_env);

        interpreter.env = env;

        let res = interpreter.eval_call(self.body.clone())?;

        interpreter.env = current_env;

        interpreter.ret_val = current_ret;

        Ok(res)
    }

    fn name(&self) -> &str {
        &self.name
    }
}
