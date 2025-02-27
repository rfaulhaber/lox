use std::{collections::HashMap, io::Write};

use lox_bytecode;
use lox_value::{Value, ValueOperatorError};

use lox_bytecode::{Chunk, Op};
use thiserror::Error;

pub type InterpretResult = Result<(), InterpreterError>;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("Invalid operand provided for {0}: expected {1}")]
    InvalidOperand(String, String),
    #[error("Insufficient stack length for operation {0}: {1}")]
    InsufficientStackLengthForOperation(BinaryOp, usize),
    #[error("Arithmetic error")]
    ArithmeticError(#[from] ValueOperatorError),
    #[error("Cannot negate something that isn't a number ({0})")]
    NegateError(Value),
    #[error("Stack is empty")]
    EmptyStack,
    #[error("IO error")]
    IOError(#[from] std::io::Error),
    #[error("Missing value at index {0}")]
    NoValueAtIndex(usize),
    #[error("Undefined variable {0}")]
    UndefinedVariable(String),
    #[error("Local not found {0}")]
    LocalNotFound(usize),
}

#[derive(Debug)]
pub enum InterpreterMode {
    Normal,
    Debug,
}

#[derive(Debug, PartialEq)]
pub enum InterpreterState {
    Running,
    Finished,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
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
pub struct Interpreter {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
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

    pub fn eval(&mut self, chunk: Chunk) -> Result<InterpreterState, InterpreterError> {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    fn new_vm(mode: InterpreterMode) -> Self {
        Interpreter {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            mode,
        }
    }

    fn run(&mut self) -> Result<InterpreterState, InterpreterError> {
        loop {
            match self.step() {
                Ok(InterpreterState::Running) => continue,
                Ok(InterpreterState::Finished) => return Ok(InterpreterState::Finished),
                Err(e) => return Err(e),
            }
        }
    }

    fn step(&mut self) -> Result<InterpreterState, InterpreterError> {
        let op = self.next_op_and_advance();

        match op {
            Some(Op::Float(index)) => {
                let constant = self.chunk.float_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Some(Op::Integer(index)) => {
                let constant = self.chunk.int_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Some(Op::String(index)) => {
                let constant = self.chunk.string_at(index).unwrap();
                self.stack.push(Value::from(constant));
            }
            Some(Op::Return) | None => return Ok(InterpreterState::Finished),
            Some(Op::Negate) => match self.stack.pop() {
                Some(Value::Number(n)) => self.stack.push(Value::Number(-n)),
                Some(v) => return Err(InterpreterError::NegateError(v)),
                None => unreachable!("negate operation found without operand"),
            },
            Some(Op::Add) => {
                let res = self.binary_op(BinaryOp::Add)?;
                self.stack.push(res)
            }
            Some(Op::Subtract) => {
                let res = self.binary_op(BinaryOp::Sub)?;
                self.stack.push(res)
            }
            Some(Op::Multiply) => {
                let res = self.binary_op(BinaryOp::Mul)?;
                self.stack.push(res)
            }
            Some(Op::Divide) => {
                let res = self.binary_op(BinaryOp::Div)?;
                self.stack.push(res)
            }
            Some(Op::True) => self.stack.push(Value::Bool(true)),
            Some(Op::False) => self.stack.push(Value::Bool(false)),
            Some(Op::Nil) => self.stack.push(Value::Nil),
            Some(Op::Not) => match self.stack.pop() {
                Some(value) => self.stack.push(Value::Bool(value.is_falsy())),
                None => unreachable!("not operation found without operand"),
            },
            Some(Op::Equal) => {
                let binary_op = self.binary_op(BinaryOp::Eq)?;
                self.stack.push(binary_op)
            }
            Some(Op::Greater) => {
                let binary_op = self.binary_op(BinaryOp::Gt)?;
                self.stack.push(binary_op)
            }
            Some(Op::Less) => {
                let binary_op = self.binary_op(BinaryOp::Lt)?;
                self.stack.push(binary_op)
            }
            Some(Op::Print) => {
                let value = self.stack.pop();

                match value {
                    Some(v) => {
                        let mut stdout = std::io::stdout().lock();
                        writeln!(&mut stdout, "{}", v)?;
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Some(Op::Pop) => {
                let _ = self.stack.pop();
            }
            Some(Op::DefineGlobal(index)) => {
                let value = self.stack.pop();
                let name = match self.chunk.string_at(index) {
                    Some(value) => value,
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                match value {
                    Some(v) => {
                        self.globals.insert(name, v);
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Some(Op::GetGlobal(index)) => {
                let name = match self.chunk.string_at(index) {
                    Some(value) => value,
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                let value = match self.globals.get(&name) {
                    Some(value) => value,
                    None => return Err(InterpreterError::UndefinedVariable(name)),
                };

                self.stack.push(value.clone());
            }
            Some(Op::SetGlobal(index)) => {
                let name = match self.chunk.string_at(index) {
                    Some(value) => value,
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                if !self.globals.contains_key(&name) {
                    return Err(InterpreterError::UndefinedVariable(name));
                }

                let value = match self.stack.pop() {
                    Some(value) => value,
                    None => return Err(InterpreterError::EmptyStack),
                };

                let _ = self.globals.insert(name, value);
            }
            Some(Op::GetLocal(index)) => {
                let value = self.stack.get(index);

                if value.is_none() {
                    return Err(InterpreterError::LocalNotFound(index));
                }

                self.stack.push(value.cloned().unwrap());
            }
            Some(Op::SetLocal(index)) => {
                let top = self.stack.get(index);

                if top.is_none() {
                    return Err(InterpreterError::EmptyStack);
                }

                self.stack[index] = top.cloned().unwrap();
            }
            Some(Op::JumpIfFalse(pos)) => {
                let value = self.stack.last();

                let is_falsy = match value {
                    Some(v) => v.is_falsy(),
                    None => return Err(InterpreterError::EmptyStack),
                };

                if is_falsy {
                    self.ip += pos;
                }
            }
            Some(Op::Jump(pos)) => {
                self.ip += pos;
            }
            Some(Op::Loop(pos)) => {
                self.ip -= pos;
            }
        }

        Ok(InterpreterState::Running)
    }

    fn next_op(&self) -> Option<Op> {
        self.chunk.code_at(self.ip).cloned()
    }

    fn next_op_and_advance(&mut self) -> Option<Op> {
        let op = self.next_op();

        if op.is_some() {
            self.ip = self.ip + 1;
        }

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
            BinaryOp::Gt => Value::Bool(a > b),
            BinaryOp::Lt => Value::Bool(a < b),
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
        code.push_float(1.2);
        code.push_float(3.4);

        code.add_op(Op::Add);

        let mut vm = Interpreter::new();
        vm.chunk = code;

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack.first().unwrap(), &Value::from(4.6));
    }

    #[test]
    fn string_concat() {
        let mut code = Chunk::new();
        code.push_string("foo".into());
        code.push_string("bar".into());
        code.push_string("baz".into());

        code.add_op(Op::Add);
        code.add_op(Op::Add);

        let mut vm = Interpreter::new();
        vm.chunk = code;

        let _ = vm.run();

        assert_eq!(
            vm.stack.first().unwrap(),
            &Value::from(String::from("foobarbaz"))
        )
    }
}
