use std::ops::{Add, Div, Mul, Sub};

use crate::value::Value;
use bytecode::{Context, Op};
use lox_source::source::Span;
use thiserror::Error;

pub(crate) mod bytecode;

pub type InterpretResult = Result<(), InterpreterError>;

#[derive(Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Invalid operand provided for {0}: expected {1}")]
    InvalidOperand(String, String),
    #[error("Insufficient stack length for operation {0}: {1}")]
    InsufficientStackLengthForOperation(String, usize),
}

#[derive(Debug)]
pub enum InterpreterMode {
    Normal,
    Debug,
}

#[derive(Debug)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub struct Interpreter {
    chunk: Context,
    ip: usize,
    stack: Vec<Value>,
    mode: InterpreterMode,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter::new_vm(InterpreterMode::Normal)
    }

    pub fn new_debug() -> Self {
        Interpreter::new_vm(InterpreterMode::Debug)
    }

    pub fn set_mode(&mut self, mode: InterpreterMode) {
        self.mode = mode;
    }

    pub fn eval(&mut self, chunk: Context) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    fn new_vm(mode: InterpreterMode) -> Self {
        Interpreter {
            chunk: Context::new(),
            ip: 0,
            stack: Vec::new(),
            mode,
        }
    }

    fn run(&mut self) -> InterpretResult {
        todo!();
    }

    fn step(&mut self) -> InterpretResult {
        let (op, _) = self.next_op_and_advance();

        match op {
            Op::Constant(index) => {
                let constant = self.chunk.constant_at(index).unwrap().clone();
                self.stack.push(constant);
            }
            Op::Return => return Ok(()),
            Op::Negate => match self.stack.pop() {
                Some(n) => match n {
                    Value::Float(f) => self.stack.push(Value::Float(-f)),
                    _ => {
                        return Err(InterpreterError::InvalidOperand(
                            "negation".into(),
                            "number".into(),
                        ))
                    }
                },
                None => unreachable!("negate operation found without operand"),
            },
            Op::Add => {
                let res = self.binary_op(BinaryOp::Add)?;
                self.stack.push(res)
            }
            Op::Subtract => {
                let res = self.binary_op(BinaryOp::Sub)?;
                self.stack.push(res)
            }
            Op::Multiply => {
                let res = self.binary_op(BinaryOp::Mul)?;
                self.stack.push(res)
            }
            Op::Divide => {
                let res = self.binary_op(BinaryOp::Div)?;
                self.stack.push(res)
            }
        }

        Ok(())
    }

    fn step_n(&mut self, n: usize) -> InterpretResult {
        for _ in 0..n {
            let res = self.step()?;
        }

        Ok(())
    }

    fn next_op(&self) -> (Op, Span) {
        self.chunk.code[self.ip].clone()
    }

    fn next_op_and_advance(&mut self) -> (Op, Span) {
        let op = self.next_op();

        self.ip = self.ip + 1;

        op
    }

    fn binary_op(&mut self, op: BinaryOp) -> Result<Value, InterpreterError> {
        if self.stack.len() < 2 {
            let op_name = match op {
                BinaryOp::Add => "addition",
                BinaryOp::Sub => "subtraction",
                BinaryOp::Mul => "multiplication",
                BinaryOp::Div => "division",
            };

            return Err(InterpreterError::InsufficientStackLengthForOperation(
                op_name.into(),
                self.stack.len(),
            ));
        }

        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        let value = match op {
            BinaryOp::Add => match (a, b) {
                (Value::Float(l), Value::Float(r)) => Value::Float(l.add(r)),
            },
            BinaryOp::Sub => match (a, b) {
                (Value::Float(l), Value::Float(r)) => Value::Float(l.sub(r)),
            },
            BinaryOp::Mul => match (a, b) {
                (Value::Float(l), Value::Float(r)) => Value::Float(l.mul(r)),
            },
            BinaryOp::Div => match (a, b) {
                (Value::Float(l), Value::Float(r)) => Value::Float(l.div(r)),
            },
        };

        Ok(value)
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use lox_source::source::Span;

    #[test]
    fn stack_calculation() {
        let mut code = Context::new();
        code.add_const(Value::Float(1.2));
        code.add_const(Value::Float(3.4));

        code.write_code(Op::Constant(0), Span::new(0, 0));
        code.write_code(Op::Constant(1), Span::new(0, 0));
        code.write_code(Op::Add, Span::new(0, 0));

        let mut vm = Interpreter::new();
        vm.chunk = code;

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack.first().unwrap(), &Value::Float(4.6));
    }
}
