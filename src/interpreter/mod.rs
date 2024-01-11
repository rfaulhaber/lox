use thiserror::Error;

use crate::ast::{
    expr::{BinaryOperator, Expr, Literal, UnaryOperator},
    visitor::Visitor,
};

pub type EvalResult = Result<LoxValue, EvalError>;

pub enum LoxValue {}

#[derive(Debug, Error)]
pub enum EvalError {}

pub struct Interpreter {}

impl Interpreter {
    pub fn eval(&mut self, expr: Expr) -> EvalResult {
        todo!();
    }
}

struct EvalVisitor {
    stack: Vec<LoxValue>,
}

impl Visitor for Interpreter {
    type Value = LoxValue;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        todo!()
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        todo!()
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value {
        todo!()
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        todo!()
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        todo!()
    }
}
