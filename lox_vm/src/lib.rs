use lox_bytecode;
use lox_value::{Value, ValueArithmeticError};

use lox_bytecode::{Chunk, Op};
use thiserror::Error;

// macro_rules! binary_op {
//     ($left:ident, $right:ident, $err:expr) => {
//         match (left, right) {

//         }

//     };
// }

pub type InterpretResult = Result<(), InterpreterError>;

#[derive(Debug, PartialEq, Error)]
pub enum InterpreterError {
    #[error("Invalid operand provided for {0}: expected {1}")]
    InvalidOperand(String, String),
    #[error("Insufficient stack length for operation {0}: {1}")]
    InsufficientStackLengthForOperation(BinaryOp, usize),
    #[error("Arithmetic error")]
    ArithmeticError(#[from] ValueArithmeticError),
    #[error("Cannot negate something that isn't a number ({0})")]
    NegateError(Value),
}

#[derive(Debug)]
pub enum InterpreterMode {
    Normal,
    Debug,
}

#[derive(Debug, PartialEq)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::Add => "+",
                BinaryOp::Sub => "-",
                BinaryOp::Mul => "*",
                BinaryOp::Div => "/",
                BinaryOp::Eq => "==",
                BinaryOp::Gt => ">",
                BinaryOp::Lt => "<",
            }
        )
    }
}

#[derive(Debug)]
enum UnaryOp {
    Neg,
    Not,
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
        let op = self.next_op_and_advance();

        match op {
            Op::Float(index) => {
                let constant = self.chunk.float_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Op::Integer(index) => {
                let constant = self.chunk.int_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Op::Return => return Ok(()),
            Op::Negate => match self.stack.pop() {
                Some(Value::Number(n)) => self.stack.push(Value::Number(-n)),
                Some(v) => return Err(InterpreterError::NegateError(v)),
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
            Op::True => self.stack.push(Value::Bool(true)),
            Op::False => self.stack.push(Value::Bool(false)),
            Op::Nil => self.stack.push(Value::Nil),
            Op::Not => match self.stack.pop() {
                Some(value) => self.stack.push(Value::Bool(value.is_falsy())),
                None => unreachable!("not operation found without operand"),
            },
            Op::Equal => {
                let binary_op = self.binary_op(BinaryOp::Eq)?;
                self.stack.push(binary_op)
            }
            Op::Greater => {
                let binary_op = self.binary_op(BinaryOp::Gt)?;
                self.stack.push(binary_op)
            }
            Op::Less => {
                let binary_op = self.binary_op(BinaryOp::Lt)?;
                self.stack.push(binary_op)
            }
            Op::String(_) => todo!(),
        }

        Ok(())
    }

    fn step_n(&mut self, n: usize) -> InterpretResult {
        for _ in 0..n {
            let res = self.step()?;
        }

        Ok(())
    }

    fn next_op(&self) -> Op {
        self.chunk.code_at(self.ip).unwrap().clone()
    }

    fn next_op_and_advance(&mut self) -> Op {
        let op = self.next_op();

        self.ip = self.ip + 1;

        op
    }

    fn binary_op(&mut self, op: BinaryOp) -> Result<Value, InterpreterError> {
        if self.stack.len() < 2 {
            return Err(InterpreterError::InsufficientStackLengthForOperation(
                op,
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
            BinaryOp::Eq => Value::Bool(a == b),
            // TODO refactor, put these elsewhere
            BinaryOp::Gt => match (a, b) {
                (Value::Number(left), Value::Number(right)) => Value::Bool(left > right),
                (left, right) => {
                    return Err(InterpreterError::ArithmeticError(
                        ValueArithmeticError::IncompatibleTypes(
                            ">".into(),
                            left.to_string(),
                            right.to_string(),
                        ),
                    ));
                }
            },
            BinaryOp::Lt => match (a, b) {
                (Value::Number(left), Value::Number(right)) => Value::Bool(left < right),
                (left, right) => {
                    return Err(InterpreterError::ArithmeticError(
                        ValueArithmeticError::IncompatibleTypes(
                            ">".into(),
                            left.to_string(),
                            right.to_string(),
                        ),
                    ));
                }
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

    #[test]
    fn stack_calculation() {
        let mut code = Chunk::new();
        code.add_float(1.2);
        code.add_float(3.4);

        code.add_op(Op::Float(0));
        code.add_op(Op::Float(1));
        code.add_op(Op::Add);

        let mut vm = Interpreter::new();
        vm.chunk = code;

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack.first().unwrap(), &Value::from(4.6));
    }
}
