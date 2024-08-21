use bytecode::{Chunk, Op, Source, Value};
use thiserror::Error;

mod bytecode;

pub type InterpretResult = Result<(), InterpreterError>;

#[derive(Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Invalid operand provided for {0}: expected {1}")]
    InvalidOperand(String, String),
}

#[derive(Debug)]
pub struct Interpreter {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn eval(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
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
                None => todo!(),
            },
        }

        Ok(())
    }

    fn next_op(&self) -> (Op, Source) {
        self.chunk.code[self.ip].clone()
    }

    fn next_op_and_advance(&mut self) -> (Op, Source) {
        let op = self.next_op();

        self.ip = self.ip + 1;

        op
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
