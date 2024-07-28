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
            Expr::Literal(lit) => self.visit_literal(lit),
            Expr::Unary(op, expr) => self.visit_unary_expr(op, *expr),
            Expr::Call(callee, arguments) => self.visit_call_expr(*callee, arguments),
            Expr::Binary(left, op, right) => self.visit_binary_expr(*left, op, *right),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
            Expr::Var(id) => match self.scopes.last().and_then(|last| last.get(&id.name)) {
                Some(ResolveState::Resolved) => {
                    self.resolve_local(expr, id.name);
                    Ok(())
                }
                Some(ResolveState::Unresolved) => todo!("throw error"),
                None => todo!("none"),
            },
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
            Expr::Get(_, _) => todo!(),
            Expr::Set(_, _, _) => todo!(),
        }
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        self.visit_expr(expr)?;

        Ok(())
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value {
        self.visit_expr(left)?;
        self.visit_expr(right)?;

        Ok(())
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        self.visit_expr(expr)?;

        Ok(())
    }

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        self.visit_expr(expr.clone())?;
        self.resolve_local(expr, id.name);

        Ok(())
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        self.visit_expr(left)?;
        self.visit_expr(right)?;

        Ok(())
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        self.visit_expr(callee)?;

        for arg in arguments {
            self.visit_expr(arg)?;
        }

        Ok(())
    }

    fn visit_program(&mut self, program: Program) -> Self::Value {
        program
            .declarations
            .iter()
            .map(|d| self.visit_declaration(d.to_owned()))
            .last()
            .unwrap_or(Ok(()))
    }

    fn visit_declaration(&mut self, decl: Decl) -> Self::Value {
        match decl {
            Decl::Class(id, superclass, funcs) => {
                self.visit_class_delcaration(id, superclass, funcs)
            }
            Decl::Var(id, expr) => {
                self.declare(id.name.clone());

                if let Some(expr) = expr {
                    self.visit_expr(expr)?;
                }

                self.define(id.name);
                Ok(())
            }
            Decl::Func(id, params, body) => self.visit_func_declaration(id, params, body),
            Decl::Stmt(_) => todo!(),
        }
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value {
        match stmt {
            Stmt::Block(block) => self.visit_block(block),
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Print(expr) => self.visit_expr(expr),
            Stmt::Return(expr) => self.visit_return_stmt(expr),
            Stmt::If(cond, stmt, else_stmt) => {
                self.visit_if_stmt(cond, *stmt, else_stmt.map(|stmt| *stmt))
            }
            Stmt::While(cond, body) => self.visit_while_stmt(cond, *body),
        }
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
        self.visit_expr(cond)?;
        self.visit_stmt(stmt)?;

        if let Some(else_stmt) = else_stmt {
            self.visit_stmt(else_stmt)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        self.visit_expr(cond)?;
        self.visit_stmt(body)?;

        Ok(())
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        self.begin_scope();

        parameters.into_iter().for_each(|id| {
            self.declare(id.name.clone());
            self.define(id.name);
        });

        self.visit_stmt(body)?;

        self.end_scope();

        Ok(())
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        if let Some(e) = expr {
            self.visit_expr(e)?;
        }

        Ok(())
    }

    fn visit_class_delcaration(
        &mut self,
        id: Identifier,
        superclass: Option<Identifier>,
        funcs: Vec<Decl>,
    ) -> Self::Value {
        todo!();
    }
}
