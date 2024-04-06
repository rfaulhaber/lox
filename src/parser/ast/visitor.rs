use super::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, UnaryOperator},
    program::Program,
    stmt::Stmt,
};

pub trait Visitor: Sized {
    type Value;

    // exprs
    fn visit_expr(&mut self, expr: Expr) -> Self::Value;

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value;

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value;

    fn visit_literal(&mut self, literal: Literal) -> Self::Value;

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value;

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value;

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value;

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value;

    // stmts
    fn visit_program(&mut self, program: Program) -> Self::Value;

    fn visit_declaration(&mut self, decl: Decl) -> Self::Value;

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value;

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value;

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value;

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value;

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value;

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value;
}
