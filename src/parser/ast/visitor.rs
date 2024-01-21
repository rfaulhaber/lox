use super::{
    expr::{BinaryOperator, Expr, Literal, UnaryOperator},
    program::Program,
    stmt::Stmt,
};

pub trait Visitor: Sized {
    type Value;

    fn visit_program(&mut self, program: Program) -> Self::Value;

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value;

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value;

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value;

    fn visit_literal(&mut self, literal: Literal) -> Self::Value;

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value;
}
