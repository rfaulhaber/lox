use std::{collections::HashMap, fmt::Write, io::BufRead};

use crate::{
    interpreter::{error::EvalError, interpreter::Interpreter},
    parser::ast::{
        decl::Decl,
        expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, UnaryOperator},
        program::Program,
        stmt::Stmt,
        visitor::Visitor,
    },
};

pub type ResolutionResult = Result<(), EvalError>;

trait Resolvable: Sized {}

enum ResolveState {
    Resolved,
    Unresolved,
}

impl ResolveState {
    pub fn is_resolved(self) -> bool {
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

    fn define(&mut self, name: impl ToString) -> Option<ResolveState> {
        self.scopes
            .last_mut()
            .and_then(|last| last.insert(name.to_string(), ResolveState::Resolved))
    }

    fn declare(&mut self, name: impl ToString) -> Option<ResolveState> {
        self.scopes
            .last_mut()
            .and_then(|last| last.insert(name.to_string(), ResolveState::Unresolved))
    }

    fn resolve_local(&mut self, expr: Expr, name: impl ToString) {
        self.scopes
            .iter()
            .enumerate()
            .rev()
            .for_each(|(level, scope)| {
                if scope.contains_key(&name.to_string()) {
                    self.interpreter.resolve_expr(expr.clone(), level - 1)
                }
            });
    }
}

impl<R: BufRead, W: Write> Visitor for Resolver<R, W> {
    type Value = ResolutionResult;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr.clone() {
            Expr::Literal(_) => todo!(),
            Expr::Unary(_, _) => todo!(),
            Expr::Call(_, _) => todo!(),
            Expr::Binary(_, _, _) => todo!(),
            Expr::Logical(_, _, _) => todo!(),
            Expr::Grouping(_) => todo!(),
            Expr::Var(id) => match self.scopes.last().and_then(|last| last.get(&id.name)) {
                Some(ResolveState::Resolved) => {
                    self.resolve_local(expr, id.name);
                    Ok(())
                }
                Some(ResolveState::Unresolved) => todo!("throw error"),
                None => todo!(),
            },
            Expr::Assignment(_, _) => todo!(),
        }
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
                self.declare(id.name.clone());

                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }

                self.define(id.name);
                Ok(())
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
        let res = block
            .into_iter()
            .try_for_each(|d| self.visit_declaration(d.to_owned()));
        self.end_scope();

        res
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
