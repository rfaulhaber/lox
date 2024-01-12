use thiserror::Error;

use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::ast::{
    expr::{BinaryOperator, Expr, Literal, Number, UnaryOperator},
    visitor::Visitor,
};

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

pub type EvalResult = Result<LoxValue, EvalError>;

#[derive(Debug, Clone, PartialEq)]
pub enum LoxValue {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

impl Display for LoxValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxValue::Nil => write!(f, "nil"),
            LoxValue::Bool(b) => write!(f, "{}", b),
            LoxValue::Int(i) => write!(f, "{}", i),
            LoxValue::Float(fl) => write!(f, "{}", fl),
            LoxValue::String(s) => write!(f, "{}", s),
        }
    }
}

impl LoxValue {
    binary_number_arithmetic_impl!(try_add, add);
    binary_number_arithmetic_impl!(try_sub, sub);
    binary_number_arithmetic_impl!(try_mul, mul);
    binary_number_arithmetic_impl!(try_div, div);

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
        }
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Incompatible types for operation {0}: {1}, {2}")]
    TypeMismatch(String, LoxValue, LoxValue),
}

pub struct Interpreter {}

impl Interpreter {
    pub fn eval(&mut self, expr: Expr) -> EvalResult {
        self.visit_expr(expr)
    }
}

struct EvalVisitor {}

impl Visitor for Interpreter {
    type Value = EvalResult;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
        }
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        let right = self.visit_expr(expr)?;

        match op {
            UnaryOperator::Neg => match right {
                LoxValue::Int(i) => Ok(LoxValue::Int(-i)),
                LoxValue::Float(f) => Ok(LoxValue::Float(-f)),
                _ => todo!(),
            },
            UnaryOperator::Not => match is_truthy(right) {
                LoxValue::Bool(b) => Ok(LoxValue::Bool(!b)),
                _ => unreachable!(),
            },
        }
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value {
        let lhs = self.visit_expr(left)?;
        let rhs = self.visit_expr(right)?;

        match op {
            BinaryOperator::Eq => Ok(LoxValue::Bool(lhs.eq(rhs))),
            BinaryOperator::Neq => Ok(LoxValue::Bool(!lhs.eq(rhs))),
            BinaryOperator::Lt => Ok(LoxValue::Bool(lhs.try_cmp(rhs)?.is_lt())),
            BinaryOperator::Lte => Ok(LoxValue::Bool(lhs.try_cmp(rhs)?.is_le())),
            BinaryOperator::Gt => Ok(LoxValue::Bool(lhs.try_cmp(rhs)?.is_gt())),
            BinaryOperator::Gte => Ok(LoxValue::Bool(lhs.try_cmp(rhs)?.is_ge())),
            BinaryOperator::Add => lhs.try_add(rhs),
            BinaryOperator::Sub => lhs.try_sub(rhs),
            BinaryOperator::Mul => lhs.try_mul(rhs),
            BinaryOperator::Div => lhs.try_div(rhs),
        }
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        Ok(match literal {
            Literal::Number(n) => n.into(),
            Literal::String(s) => LoxValue::String(s),
            Literal::Bool(b) => LoxValue::Bool(b),
            Literal::Nil => LoxValue::Nil,
        })
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        self.visit_expr(expr)
    }
}

impl Into<LoxValue> for Number {
    fn into(self) -> LoxValue {
        match self {
            Number::Int(i) => LoxValue::Int(i),
            Number::Float(f) => LoxValue::Float(f),
        }
    }
}

fn is_truthy(val: LoxValue) -> LoxValue {
    match val {
        LoxValue::Nil => LoxValue::Bool(false),
        LoxValue::Bool(_) => val,
        _ => LoxValue::Bool(true),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn eval_trivial() {
        let ast = Parser::new(Lexer::new("-123 * (45.67)")).parse().unwrap();

        let expected = LoxValue::Float(-5617.41);

        let mut interpreter = Interpreter {};

        let result = interpreter.eval(ast);

        assert!(result.is_ok());

        assert_eq!(result.unwrap(), expected);
    }
}
