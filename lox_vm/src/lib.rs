use lox_bytecode;
use lox_value::{Value, ValueArithmeticError};

use lox_bytecode::{Chunk, Op};
use lox_source::source::Span;
use thiserror::Error;

pub type InterpretResult = Result<(), InterpreterError>;

#[derive(Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Invalid operand provided for {0}: expected {1}")]
    InvalidOperand(String, String),
    #[error("Insufficient stack length for operation {0}: {1}")]
    InsufficientStackLengthForOperation(String, usize),
    #[error("Arithmetic error")]
    ArithmeticError(#[from] ValueArithmeticError),
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
    chunk: Chunk,
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

    pub fn eval(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    fn new_vm(mode: InterpreterMode) -> Self {
        Interpreter {
            chunk: Chunk::new(),
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
                let constant = self.chunk.number_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Op::Return => return Ok(()),
            Op::Negate => match self.stack.pop() {
                Some(Value::Number(n)) => self.stack.push(Value::Number(-n)),
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
        self.chunk.code_at(self.ip).unwrap().clone()
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
            BinaryOp::Add => (a + b)?,
            BinaryOp::Sub => (a - b)?,
            BinaryOp::Mul => (a * b)?,
            BinaryOp::Div => (a / b)?,
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
        let mut code = Chunk::new();
        code.add_number(1.2);
        code.add_number(3.4);

        code.add_op(Op::Constant(0), Span::new(0, 0));
        code.add_op(Op::Constant(1), Span::new(0, 0));
        code.add_op(Op::Add, Span::new(0, 0));

        let mut vm = Interpreter::new();
        vm.chunk = code;

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack.first().unwrap(), &Value::from(4.6));
    }
}
