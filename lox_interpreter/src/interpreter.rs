use lox_source::ast::expr::Number;
use std::borrow::Borrow;
use std::{fmt::Write, io::BufRead};

use lox_source::ast::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, UnaryOperator},
    program::Program,
    stmt::Stmt,
    visitor::Visitor,
};

use super::value::Function;
use super::{
    env::Env,
    error::EvalError,
    value::{Callable, LoxValue},
    EvalResult,
};

// TODO implement builder pattern?
pub struct Interpreter<R, W, S>
where
    R: BufRead,
    W: Write,
    S: miette::SourceCode + 'static,
{
    reader: R,
    writer: W,
    pub(super) env: Env,
    pub(super) ret_val: Option<LoxValue>,

    source: miette::NamedSource<S>,
    pub(super) backtrace: Vec<String>,
}

impl<R: BufRead, W: Write, S: miette::SourceCode + 'static> Interpreter<R, W, S> {
    pub fn new(reader: R, writer: W, source: S, source_name: impl AsRef<str>) -> Self {
        Self {
            reader,
            writer,
            env: Env::new_with_builtins(),
            ret_val: None,
            source: miette::NamedSource::new(source_name, source),
            backtrace: vec!["toplevel".into()],
        }
    }

    pub fn eval(&mut self, program: Program) -> EvalResult {
        if let Some(value) = self.ret_val.clone() {
            return Ok(value);
        }

        self.visit_program(program)
    }

    pub fn output(&mut self) -> &W {
        self.writer.borrow()
    }

    pub fn backtrace(&self) -> Vec<String> {
        self.backtrace.clone()
    }

    pub(super) fn eval_call(&mut self, stmt: Stmt) -> EvalResult {
        self.visit_stmt(stmt)
    }
}

impl<R: BufRead, W: Write, S: miette::SourceCode> Visitor for Interpreter<R, W, S> {
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
                let expr = self.visit_expr(expr)?;

                let output = format!("{}", expr);

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
        if let Some(val) = self.ret_val.clone() {
            return Ok(val);
        }

        match decl {
            Decl::Class(id, superclass, funcs) => {
                self.visit_class_delcaration(id, superclass, funcs)
            }
            Decl::Var(id, expr) => match expr {
                Some(expr) => match self.visit_expr(expr) {
                    Ok(value) => {
                        self.env.define(id.name, Some(value.clone()));
                        Ok(value)
                    }
                    v => v,
                },
                None => {
                    self.env.define(id.name, Some(LoxValue::Nil));
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
        self.env = Env::with_enclosing(self.env.clone());

        let result = block
            .into_iter()
            .map(|stmt| self.visit_declaration(stmt))
            .last()
            .unwrap_or(Ok(LoxValue::Nil));

        if self.env.enclosing.is_some() {
            self.env = *self.env.enclosing.clone().unwrap(); // clunky
        } else {
            self.env.enclosing = None;
        }

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
        match expr.clone() {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
            Expr::Var(var) => self
                .env
                .get(var.name.clone())
                .ok_or_else(|| EvalError::UndefinedVariable(var.name))
                .cloned(),
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
            Expr::Call(callee, arguments) => self.visit_call_expr(*callee, arguments),
            Expr::Get(_, _) => todo!(),
            Expr::Set(_, _, _) => todo!(),
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
        self.env.assign(id.name, Some(value.clone()))?;

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

        let callable: Box<dyn Callable<R, W, S>> = match callee_val {
            LoxValue::Native(native) => Box::new(native),
            LoxValue::Function(func) => Box::new(func),
            _ => unreachable!("found uncallable stmt"), // TODO is this really unreachable?
        };

        let res = callable.call(self, &arguments?);

        res
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        let Identifier { name } = name;

        let func = LoxValue::Function(Function {
            name: name.clone(),
            parameters: parameters.into_iter().map(|i| i.name).collect(),
            body,
            closure: self.env.clone(),
        });

        self.env.define(name, Some(func));

        Ok(LoxValue::Nil)
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        let ret_val = expr.map_or(Ok(LoxValue::Nil), |expr| self.visit_expr(expr))?;
        self.ret_val = Some(ret_val.clone());

        Ok(ret_val)
    }

    fn visit_class_delcaration(
        &mut self,
        id: Identifier,
        superclass: Option<Identifier>,
        funcs: Vec<Decl>,
    ) -> Self::Value {
        todo!()
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
    use lox_source::{lexer::Lexer, parser::Parser};

    use super::*;

    struct TestEnv<R, W, S>
    where
        R: BufRead,
        W: Write,
        S: miette::SourceCode + 'static,
    {
        source: S,
        interpreter: Interpreter<R, W, S>,
        program: Program,
    }

    fn setup(source: &str) -> TestEnv<Box<&[u8]>, String, &str> {
        let program = Parser::new(Lexer::new(source)).parse().unwrap();

        let mock_reader = Box::new(&b""[..]);

        let writer = String::new();

        let interpreter = Interpreter::new(mock_reader, writer, source, "source");

        TestEnv {
            source,
            interpreter,
            program,
        }
    }

    #[test]
    fn eval_trivial() {
        let source = "print -123 * (45.67);";

        let TestEnv {
            mut interpreter,
            program,
            ..
        } = setup(source);

        let result = interpreter.eval(program);

        assert!(matches!(result, Ok(LoxValue::Nil)));
        assert_eq!(interpreter.output().trim(), "-5617.41");
    }

    #[test]
    fn eval_variable() {
        let source = std::str::from_utf8(b"var test = \"hello world\";\nprint test;").unwrap();

        let TestEnv {
            mut interpreter,
            program,
            ..
        } = setup(source);

        let result = interpreter.eval(program);

        assert!(matches!(result, Ok(LoxValue::Nil)));
        assert_eq!(interpreter.output().trim(), "\"hello world\"");
    }

    #[test]
    fn string_concat() {
        let source =
            std::str::from_utf8(b"var test = \"hello\" + \" \" + \"world\";\nprint test;").unwrap();

        let TestEnv {
            mut interpreter,
            program,
            ..
        } = setup(source);

        let result = interpreter.eval(program);

        assert!(matches!(result, Ok(LoxValue::Nil)));
        assert_eq!(interpreter.output().trim(), "\"hello world\"");
    }

    #[test]
    fn capture_env() {
        let source =
            std::str::from_utf8(b"var test = \"hello\" + \" \" + \"world\";\nprint test;").unwrap();

        let TestEnv {
            mut interpreter, ..
        } = setup(source);

        interpreter.env.define("n", Some(LoxValue::Int(123)));

        let mut captured_env = interpreter.env.clone();

        let _ = captured_env.assign("n", Some(LoxValue::Int(456)));

        assert_ne!(interpreter.env.get("n"), captured_env.get("n"));
    }

    #[test]
    #[ignore]
    fn fib_eval() {
        let source = r#"fun count(n) {
  if (n > 1) count(n - 1);
  print n;
}

count(3);
"#;

        let TestEnv {
            mut interpreter,
            program,
            ..
        } = setup(source);

        let result = interpreter.eval(program);

        assert_eq!(result, Ok(LoxValue::Nil));
        assert_eq!(interpreter.output().trim(), "1\n2\n3");
    }
}
