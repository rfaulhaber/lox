use thiserror::Error;

use std::{
    cmp::Ordering,
    collections::HashMap,
    fmt::Display,
    ops::{Add, Div, Mul, Sub},
};

use crate::parser::ast::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
    visitor::{ExprVisitor, StmtVisitor},
};

// TODO implement proc macro

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
    #[error("Reference to undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Type error: {0}")]
    TypeError(String),
}

#[derive(Debug, Clone)]
struct Env {
    outer: Option<Box<Env>>,
    values: HashMap<String, LoxValue>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            outer: None,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: LoxValue) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: LoxValue) -> Result<(), EvalError> {
        if self.values.contains_key(&name) {
            self.values.insert(name.clone(), value.clone());
        }

        match self.outer {
            Some(_) => self.outer.as_mut().unwrap().assign(name, value),
            None => Err(EvalError::UndefinedVariable(name)),
        }
    }

    pub fn get(&mut self, name: String) -> Option<LoxValue> {
        self.values
            .get(&name)
            .cloned()
            .or_else(|| match &self.outer {
                Some(_) => self.outer.as_mut().unwrap().get(name),
                None => None,
            })
    }

    pub fn from_outer(outer: Env) -> Env {
        Env {
            outer: Some(Box::new(outer)),
            values: HashMap::new(),
        }
    }
}

// TODO implement builder pattern?
#[derive(Debug)]
pub struct Interpreter<R: std::io::BufRead, W: std::io::Write> {
    reader: R,
    writer: W,
    state: InterpreterState,
    env: Env,
}

#[derive(Debug)]
enum InterpreterState {
    Ready,
    Evaluating,
    Done,
    Error(EvalError),
}

impl<R: std::io::BufRead, W: std::io::Write> Interpreter<R, W> {
    pub fn new(reader: R, writer: W) -> Self {
        Self {
            reader,
            writer,
            state: InterpreterState::Ready,
            env: Env::new(),
        }
    }

    pub fn eval(&mut self, program: Program) {
        self.state = InterpreterState::Evaluating;
        self.visit_program(program);
        self.state = InterpreterState::Done;
    }

    // TODO do better
    pub fn get_writer(&mut self) -> &W {
        &self.writer
    }
}

impl<R: std::io::BufRead, W: std::io::Write> StmtVisitor for Interpreter<R, W> {
    fn visit_program(&mut self, program: Program) {
        for decl in program.declarations {
            self.visit_declaration(decl)
        }
    }

    fn visit_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                // TODO do better?
                self.visit_expr(expr);
                ()
            }
            Stmt::Print(expr) => {
                let expr = self.visit_expr(expr);

                let output = match expr {
                    Ok(ref val) => format!("{}", val),
                    Err(ref e) => format!("{}", e),
                };

                writeln!(self.writer, "{}", output).unwrap();
            }
            Stmt::Block(block) => {
                self.visit_block(block);
            }
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.visit_if_stmt(cond, *if_stmt, else_stmt.map(|stmt| *stmt))
            }
        };
    }

    fn visit_declaration(&mut self, decl: Decl) {
        match decl {
            Decl::Var(id, expr) => match expr {
                Some(expr) => match self.visit_expr(expr) {
                    Ok(value) => {
                        self.env.define(id.name, value);
                    }
                    Err(err) => {
                        self.state = InterpreterState::Error(err);
                    }
                },
                None => self.env.define(id.name, LoxValue::Nil),
            },
            Decl::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) {
        // TODO avoid cloning
        let previous = self.env.clone();
        let block_env = Env::from_outer(self.env.clone());

        self.env = block_env;

        block
            .into_iter()
            .for_each(|stmt| self.visit_declaration(stmt));

        self.env = previous;
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) {
        let expr = self.visit_expr(cond);

        match expr {
            Ok(val) => match is_truthy(val) {
                LoxValue::Bool(true) => self.visit_stmt(stmt),
                LoxValue::Bool(false) => match else_stmt {
                    Some(stmt) => self.visit_stmt(stmt),
                    None => (),
                },
                _ => unreachable!(),
            },
            Err(e) => {
                self.state = InterpreterState::Error(e);
            }
        }
    }
}

impl<R: std::io::BufRead, W: std::io::Write> ExprVisitor for Interpreter<R, W> {
    type Value = EvalResult;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
            Expr::Var(var) => self
                .env
                .get(var.name.clone())
                .ok_or_else(|| EvalError::UndefinedVariable(var.name)),
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
        }
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        let right = self.visit_expr(expr)?;

        match op {
            UnaryOperator::Neg => match right {
                LoxValue::Int(i) => Ok(LoxValue::Int(-i)),
                LoxValue::Float(f) => Ok(LoxValue::Float(-f)),
                _ => Err(EvalError::TypeError(
                    "Cannot apply - operator to non-number.".into(),
                )),
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

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        let value = self.visit_expr(expr)?;
        self.env.assign(id.name, value.clone())?;

        Ok(value)
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        let left_value = self.visit_expr(left)?;

        match (is_truthy(left_value.clone()), op) {
            (LoxValue::Bool(true), LogicalOperator::Or) => Ok(left_value),
            (LoxValue::Bool(false), LogicalOperator::Or) => Ok(self.visit_expr(right)?),
            (LoxValue::Bool(false), LogicalOperator::And) => Ok(left_value),
            (LoxValue::Bool(true), LogicalOperator::And) => Ok(self.visit_expr(right)?),
            _ => unreachable!(),
        }
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
        let ast = Parser::new(Lexer::new("print -123 * (45.67);"))
            .parse()
            .unwrap();

        let mock_reader = b"test";

        let mut interpreter = Interpreter::new(&mock_reader[..], Vec::new());

        interpreter.eval(ast);

        let result = String::from_utf8(interpreter.get_writer().clone()).unwrap();

        assert_eq!(result.trim(), "-5617.41");
    }

    #[test]
    fn eval_variable() {
        let ast = Parser::new(Lexer::new(
            std::str::from_utf8(b"var test = \"hello world\";\nprint test;").unwrap(),
        ))
        .parse()
        .unwrap();

        let mock_reader = b"";

        let mut interpreter = Interpreter::new(&mock_reader[..], Vec::new());

        interpreter.eval(ast);

        let result = String::from_utf8(interpreter.get_writer().clone()).unwrap();

        assert_eq!(result.trim(), "\"hello world\"");
    }
}
