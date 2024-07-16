use std::{collections::HashMap, fmt::Write, io::BufRead};

use crate::{
    interpreter::{EvalResult, Interpreter},
    parser::ast::{
        decl::Decl,
        expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, UnaryOperator},
        program::Program,
        stmt::Stmt,
        visitor::Visitor,
    },
};

trait Resolvable: Sized {}

enum ResolveState {
    Resolved,
    Unresolved,
}

impl ResolveState {
    pub fn is_resolved(self) {
        match self {
            ResolveState::Resolved => true,
            ResolveState::Unresolved => false,
        }
    }
}

pub struct Resolver<R, W>
where
    R: BufRead,
    W: Write,
{
    interpreter: Interpreter<R, W>,
    scopes: Vec<HashMap<String, ResolveState>>,
}

impl<R: BufRead, W: Write> Resolver<R, W> {
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn define(&mut self, name: dyn ToString) {
        self.scopes
            .last_mut()
            .and_then(|last| last.insert(name.to_string(), ResolveState::Resolved))
    }

    fn declare(&mut self, name: dyn ToString) {
        self.scopes
            .last_mut()
            .and_then(|last| last.insert(name.to_string(), ResolveState::Unresolved))
    }

    fn resolve_local(&mut self, expr: Expr, name: dyn ToString) {
        self.scopes
            .iter()
            .enumerate()
            .rev()
            .for_each(|(level, scope)| {
                if scope.contains_key(name.into()) {
                    self.interpreter.resolve(expr, level - 1)
                }
            });
    }
}

impl<R: BufRead, W: Write> Visitor for Resolver<R, W> {
    type Value = ();

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

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        todo!()
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        todo!()
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        todo!()
    }

    fn visit_program(&mut self, program: Program) -> Self::Value {
        todo!()
    }

    fn visit_declaration(&mut self, decl: Decl) -> Self::Value {
        match decl {
            Decl::Var(id, expr) => {
                self.declare(id);

                if expr.is_some() {
                    self.visit_expr(expr);
                }

                self.define(id);
            }
            Decl::Func(_, _, _) => todo!(),
            Decl::Stmt(_) => todo!(),
        }
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value {
        todo!()
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        self.begin_scope();
        block
            .into_iter()
            .for_each(|d| self.visit_declaration(d.to_owned()));
        self.end_scope();
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        todo!()
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        todo!()
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        todo!()
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        todo!()
    }
}
