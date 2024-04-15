mod env;
mod value;

use env::Env;
use thiserror::Error;
use value::{Callable, LoxValue};

use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Write, io::BufRead, rc::Rc};

use crate::parser::ast::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
    visitor::Visitor,
};

// TODO implement proc macro

pub type EvalResult = Result<LoxValue, EvalError>;

#[derive(Debug, PartialEq, Error)]
pub enum EvalError {
    #[error("Incompatible types for operation {0}: {1}, {2}")]
    TypeMismatch(String, LoxValue, LoxValue),
    #[error("Reference to undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Not callable: {0}")]
    NotCallable(LoxValue),
    #[error("Function {0} not called with enough arguments (expecting {1} but got {2})")]
    NotEnoughArguments(String, usize, usize),
    #[error("Internal return")]
    InternalReturn(LoxValue),
}

// TODO implement builder pattern?
pub struct Interpreter<R, W>
where
    R: BufRead,
    W: Write,
{
    reader: R,
    writer: W,
    env: Rc<RefCell<Env>>,
}

impl<R: BufRead, W: Write> Interpreter<R, W> {
    pub fn new(reader: R, writer: W) -> Self {
        Self {
            reader,
            writer,
            env: Rc::new(RefCell::new(Env::new_with_builtins())),
        }
    }

    pub fn eval(&mut self, program: Program) -> EvalResult {
        self.visit_program(program)
    }

    pub fn get_output(&mut self) -> &W {
        self.writer.borrow()
    }

    fn env_define<S: ToString>(&mut self, name: S, value: LoxValue) {
        (*self.env).borrow_mut().define(name, value);
    }

    fn env_assign<S: ToString>(&mut self, name: S, value: LoxValue) -> Result<(), EvalError> {
        (*self.env).borrow_mut().assign(name, value)
    }

    fn env_get<S: ToString>(&mut self, name: S) -> Option<LoxValue> {
        (*self.env).borrow_mut().get(name)
    }

    fn call(&mut self, callable: Callable, args: Vec<LoxValue>) -> EvalResult {
        match callable {
            Callable::Native { func, .. } => func(args),
            Callable::Function {
                name,
                parameters,
                body,
                closure,
            } => {
                if args.len() != parameters.len() {
                    return Err(EvalError::NotEnoughArguments(
                        name,
                        parameters.len(),
                        args.len(),
                    ));
                }

                let current_env = self.env.clone();

                self.env = Rc::new(closure);

                parameters
                    .into_iter()
                    .zip(args.into_iter())
                    .for_each(|(parameter, arg)| {
                        self.env_define(parameter, arg);
                    });

                let result = match body {
                    Stmt::Block(v) => self.eval_fn_block(v),
                    _ => unreachable!("didn't get a block"),
                };

                self.env = current_env;

                result
            }
        }
    }

    fn eval_fn_block(&mut self, block: Vec<Decl>) -> EvalResult {
        let mut result = Ok(LoxValue::Nil);

        for stmt in block.into_iter() {
            result = match self.visit_declaration(stmt) {
                Ok(val) => Ok(val),
                Err(EvalError::InternalReturn(val)) => return Ok(val),
                Err(e) => return Err(e),
            };
        }

        result
    }
}

impl<R: BufRead, W: Write> Visitor for Interpreter<R, W> {
    type Value = EvalResult;

    fn visit_program(&mut self, program: Program) -> Self::Value {
        program
            .declarations
            .iter()
            .map(|d| self.visit_declaration(d.to_owned()))
            .last()
            .unwrap_or(Ok(LoxValue::Nil))
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value {
        match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Print(expr) => {
                let expr = self.visit_expr(expr);

                let output = match expr {
                    Ok(ref val) => format!("{}", val),
                    Err(ref e) => format!("{}", e),
                };

                writeln!(self.writer, "{}", output).unwrap();

                Ok(LoxValue::Nil)
            }
            Stmt::Block(block) => self.visit_block(block),
            Stmt::If(cond, if_stmt, else_stmt) => {
                self.visit_if_stmt(cond, *if_stmt, else_stmt.map(|stmt| *stmt))
            }
            Stmt::While(cond, body) => self.visit_while_stmt(cond, *body),
            Stmt::Return(expr) => self.visit_return_stmt(expr),
        }
    }

    fn visit_declaration(&mut self, decl: Decl) -> Self::Value {
        match decl {
            Decl::Var(id, expr) => match expr {
                Some(expr) => match self.visit_expr(expr) {
                    Ok(value) => {
                        self.env_define(id.name, value.clone());
                        Ok(value)
                    }
                    v => v,
                },
                None => {
                    self.env_define(id.name, LoxValue::Nil);
                    Ok(LoxValue::Nil)
                }
            },
            Decl::Stmt(stmt) => self.visit_stmt(stmt),
            Decl::Func(name, parameters, body) => {
                self.visit_func_declaration(name, parameters, body)
            }
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        let current_env = self.env.clone();

        self.env = Env::from_outer(current_env.clone());

        let result = block
            .into_iter()
            .map(|stmt| self.visit_declaration(stmt))
            .last()
            .unwrap_or(Ok(LoxValue::Nil));

        self.env = current_env;

        result
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        let expr = self.visit_expr(cond);

        match expr {
            Ok(val) => match is_truthy(val) {
                LoxValue::Bool(true) => self.visit_stmt(stmt),
                LoxValue::Bool(false) => match else_stmt {
                    Some(stmt) => self.visit_stmt(stmt),
                    None => Ok(LoxValue::Nil),
                },
                _ => unreachable!(),
            },
            Err(e) => Err(e),
        }
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        loop {
            let expr = self.visit_expr(cond.clone());
            match expr {
                Ok(val) => {
                    if is_truthy(val) == LoxValue::Bool(true) {
                        let _ = self.visit_stmt(body.clone())?;
                    } else {
                        break;
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok(LoxValue::Nil)
    }

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
            Expr::Var(var) => self
                .env_get(var.name.clone())
                .ok_or_else(|| EvalError::UndefinedVariable(var.name)),
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
            Expr::Call(callee, arguments) => self.visit_call_expr(*callee, arguments),
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
            // for lox values, we trim the surrounding double quotes
            Literal::String(s) => LoxValue::String(String::from(s.get(1..(s.len() - 1)).unwrap())),
            Literal::Bool(b) => LoxValue::Bool(b),
            Literal::Nil => LoxValue::Nil,
        })
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        self.visit_expr(expr)
    }

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        let value = self.visit_expr(expr)?;
        self.env_assign(id.name, value.clone())?;

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

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        let callee_val = self.visit_expr(callee)?;

        let arguments: Result<Vec<LoxValue>, EvalError> = arguments
            .into_iter()
            .map(|expr| Ok(self.visit_expr(expr)?))
            .collect();

        let callee = match callee_val {
            LoxValue::Callable(c) => c,
            v => return Err(EvalError::NotCallable(v)),
        };

        self.call(callee, arguments?)
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        let name = name.name;

        let closure = RefCell::new(Env {
            outer: Some(self.env.clone()),
            values: HashMap::new(),
        });

        let func = LoxValue::Callable(Callable::Function {
            name: name.clone(),
            parameters: parameters.into_iter().map(|i| i.name).collect(),
            body,
            closure,
        });

        self.env_define(name, func);

        Ok(LoxValue::Nil)
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        let ret_val = expr.map_or(Ok(LoxValue::Nil), |expr| self.visit_expr(expr))?;
        Err(EvalError::InternalReturn(ret_val))
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

        let mock_reader = Box::new(&b""[..]);

        let mut writer = String::new();

        let mut interpreter = Interpreter::new(mock_reader, &mut writer);

        let result = interpreter.eval(ast);

        assert!(matches!(result, Ok(LoxValue::Nil)));

        assert_eq!(writer.trim(), "-5617.41");
    }

    #[test]
    fn eval_variable() {
        let ast = Parser::new(Lexer::new(
            std::str::from_utf8(b"var test = \"hello world\";\nprint test;").unwrap(),
        ))
        .parse()
        .unwrap();

        let mock_reader = Box::new(&b""[..]);

        let mut writer = String::new();

        let mut interpreter = Interpreter::new(mock_reader, &mut writer);

        let result = interpreter.eval(ast);

        assert!(matches!(result, Ok(LoxValue::Nil)));

        assert_eq!(writer.trim(), "\"hello world\"");
    }

    #[test]
    fn string_concat() {
        let ast = Parser::new(Lexer::new(
            std::str::from_utf8(b"var test = \"hello\" + \" \" + \"world\";\nprint test;").unwrap(),
        ))
        .parse()
        .unwrap();

        let mock_reader = Box::new(&b""[..]);

        let mut writer = String::new();

        let mut interpreter = Interpreter::new(mock_reader, &mut writer);

        let result = interpreter.eval(ast);

        assert!(matches!(result, Ok(LoxValue::Nil)));

        assert_eq!(writer.trim(), "\"hello world\"");
    }
}
