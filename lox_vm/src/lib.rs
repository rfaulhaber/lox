use std::{collections::HashMap, io::Write};

use lox_bytecode::{self, Function};
use lox_value::{Object, Value, ValueOperatorError};

use lox_bytecode::{Chunk, Op};
use thiserror::Error;

pub const FRAME_MAX: usize = 64;
pub const STACK_SIZE: usize = FRAME_MAX * u8::MAX as usize;

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
    #[error("Not enough call frames")]
    InsufficientCallFrameLength,
    #[error("{0} is not callable")]
    ValueNotCallable(String),
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
pub struct CallFrame {
    // function: Function,
    slots: Vec<Value>,
    ip: usize,
    chunk: Chunk,
}

impl CallFrame {
    pub fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            slots: Vec::with_capacity(FRAME_MAX),
            ip: 0,
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    globals: HashMap<String, Value>,
    frames: Vec<CallFrame>,
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
        self.frames.push(CallFrame::new(chunk));

        self.run()
    }

    fn new_vm(mode: InterpreterMode) -> Self {
        Interpreter {
            globals: HashMap::new(),
            frames: Vec::with_capacity(FRAME_MAX),
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
        let op = self.next_op_and_advance()?;

        match op {
            Some(Op::Float(index)) => {
                let constant = self.float_at(index).unwrap();
                self.stack_push(Value::from(constant));
            }
            Some(Op::Integer(index)) => {
                let constant = self.int_at(index).unwrap();
                self.stack_push(Value::from(constant));
            }
            Some(Op::String(index)) => {
                let constant = self.string_at(index).unwrap();
                self.stack_push(Value::from(constant));
            }
            Some(Op::Fn(index)) => {
                let constant = self.fn_at(index).unwrap();
                self.stack_push(Value::from(constant));
            }
            Some(Op::Return) | None => return Ok(InterpreterState::Finished),
            Some(Op::Negate) => match self.stack_pop() {
                Some(Value::Number(n)) => self.stack_push(Value::Number(-n)),
                Some(v) => return Err(InterpreterError::NegateError(v)),
                None => unreachable!("negate operation found without operand"),
            },
            Some(Op::Add) => {
                let res = self.binary_op(BinaryOp::Add)?;
                self.stack_push(res)
            }
            Some(Op::Subtract) => {
                let res = self.binary_op(BinaryOp::Sub)?;
                self.stack_push(res)
            }
            Some(Op::Multiply) => {
                let res = self.binary_op(BinaryOp::Mul)?;
                self.stack_push(res)
            }
            Some(Op::Divide) => {
                let res = self.binary_op(BinaryOp::Div)?;
                self.stack_push(res)
            }
            Some(Op::True) => self.stack_push(Value::Bool(true)),
            Some(Op::False) => self.stack_push(Value::Bool(false)),
            Some(Op::Nil) => self.stack_push(Value::Nil),
            Some(Op::Not) => match self.stack_pop() {
                Some(value) => self.stack_push(Value::Bool(value.is_falsy())),
                None => unreachable!("not operation found without operand"),
            },
            Some(Op::Equal) => {
                let binary_op = self.binary_op(BinaryOp::Eq)?;
                self.stack_push(binary_op)
            }
            Some(Op::Greater) => {
                let binary_op = self.binary_op(BinaryOp::Gt)?;
                self.stack_push(binary_op)
            }
            Some(Op::Less) => {
                let binary_op = self.binary_op(BinaryOp::Lt)?;
                self.stack_push(binary_op)
            }
            Some(Op::Print) => {
                let value = self.stack_pop();

                match value {
                    Some(v) => {
                        let mut stdout = std::io::stdout().lock();
                        writeln!(&mut stdout, "{}", v)?;
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Some(Op::Pop) => {
                let _ = self.stack_pop();
            }
            Some(Op::DefineGlobal(index)) => {
                let value = self.stack_pop();
                let name = match self.string_at(index) {
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
                let name = match self.string_at(index) {
                    Some(value) => value,
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                let value = match self.globals.get(&name) {
                    Some(value) => value,
                    None => return Err(InterpreterError::UndefinedVariable(name)),
                };

                self.stack_push(value.clone());
            }
            Some(Op::SetGlobal(index)) => {
                let name = match self.string_at(index) {
                    Some(value) => value,
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                if !self.globals.contains_key(&name) {
                    return Err(InterpreterError::UndefinedVariable(name));
                }

                let value = match self.stack_pop() {
                    Some(value) => value,
                    None => return Err(InterpreterError::EmptyStack),
                };

                let _ = self.globals.insert(name, value);
            }
            Some(Op::GetLocal(index)) => {
                let value = self.stack_get(index);

                if value.is_none() {
                    return Err(InterpreterError::LocalNotFound(index));
                }

                self.stack_push(value.cloned().unwrap());
            }
            Some(Op::SetLocal(index)) => {
                let top = self.stack_top();

                if top.is_none() {
                    return Err(InterpreterError::EmptyStack);
                }

                self.stack_set(index, top.cloned().unwrap())?;
            }
            Some(Op::JumpIfFalse(pos)) => {
                let value = self.stack_top();

                let is_falsy = match value {
                    Some(v) => v.is_falsy(),
                    None => return Err(InterpreterError::EmptyStack),
                };

                if is_falsy {
                    self.jump(pos)?;
                }
            }
            Some(Op::Jump(pos)) => self.jump(pos)?,
            Some(Op::Loop(pos)) => self.jump_loop(pos)?,
            Some(Op::Call(arg_count)) => {
                self.call_fn(arg_count)?;
            }
        }

        Ok(InterpreterState::Running)
    }

    fn next_op(&self) -> Option<Op> {
        self.frames
            .last()
            .and_then(|frame| frame.chunk.code_at(frame.ip).cloned())
    }

    fn next_op_and_advance(&mut self) -> Result<Option<Op>, InterpreterError> {
        let op = self.next_op();

        if op.is_some() {
            let _ = self.advance()?;
        }

        Ok(op)
    }

    fn advance(&mut self) -> Result<(), InterpreterError> {
        self.offset_ip(1, false)
    }

    fn binary_op(&mut self, op: BinaryOp) -> Result<Value, InterpreterError> {
        if self.stack_len() < 2 {
            return Err(InterpreterError::InsufficientStackLengthForOperation(
                op,
                self.stack_len(),
            ));
        }

        let b = self.stack_pop().unwrap();
        let a = self.stack_pop().unwrap();

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

    fn jump(&mut self, offset: usize) -> Result<(), InterpreterError> {
        self.offset_ip(offset, false)
    }

    fn jump_loop(&mut self, offset: usize) -> Result<(), InterpreterError> {
        self.offset_ip(offset, true)
    }

    // this is stupid I'm sorry. offset should probably not be a `usize`
    fn offset_ip(&mut self, offset: usize, negative: bool) -> Result<(), InterpreterError> {
        self.frames
            .last_mut()
            .map(|frame| {
                if negative {
                    frame.ip -= offset
                } else {
                    frame.ip += offset
                }
            })
            .ok_or(InterpreterError::InsufficientCallFrameLength)
    }

    fn stack_pop(&mut self) -> Option<Value> {
        self.frames.last_mut().and_then(|frame| frame.slots.pop())
    }

    fn stack_top(&self) -> Option<&Value> {
        self.frames.last().and_then(|frame| frame.slots.last())
    }

    fn stack_get(&self, index: usize) -> Option<&Value> {
        self.frames.last().and_then(|frame| frame.slots.get(index))
    }

    fn stack_set(&mut self, index: usize, value: Value) -> Result<(), InterpreterError> {
        self.frames
            .last_mut()
            .map(|frame| frame.slots[index] = value)
            .ok_or(InterpreterError::EmptyStack)
    }

    fn stack_push(&mut self, value: Value) {
        let frame = self.frames.last_mut();

        if let Some(frame) = frame {
            frame.slots.push(value);
        }
    }

    fn stack_len(&self) -> usize {
        self.frames.last().map(|f| f.slots.len()).unwrap_or(0)
    }

    fn float_at(&self, index: usize) -> Option<f64> {
        self.frames.last().and_then(|f| f.chunk.float_at(index))
    }

    fn int_at(&self, index: usize) -> Option<i64> {
        self.frames.last().and_then(|f| f.chunk.int_at(index))
    }

    fn string_at(&self, index: usize) -> Option<String> {
        self.frames.last().and_then(|f| f.chunk.string_at(index))
    }

    fn fn_at(&self, index: usize) -> Option<Function> {
        self.frames.last().and_then(|f| f.chunk.fn_at(index))
    }

    fn get_ip(&self) -> Result<usize, InterpreterError> {
        self.frames
            .last()
            .map(|frame| frame.ip)
            .ok_or(InterpreterError::InsufficientCallFrameLength)
    }

    fn call_fn(&mut self, arg_count: usize) -> Result<(), InterpreterError> {
        let callee = self.stack_get(self.stack_len() - arg_count - 1);

        match callee {
            Some(Value::Object(Object::Function(f))) => {
                let mut call_frame = CallFrame::new(f.chunk());

                if self.stack_len() < arg_count {
                    // TODO insufficient argument length
                    return Err(InterpreterError::InsufficientCallFrameLength);
                }

                call_frame.slots = self
                    .frames
                    .last_mut()
                    .map(|f| {
                        let mut vals = Vec::new();

                        for _ in 0..arg_count {
                            vals.push(f.slots.pop().unwrap())
                        }

                        f.slots.pop(); // pop callee

                        vals
                    })
                    .unwrap_or(Vec::new());

                self.frames.push(call_frame);

                Ok(())
            }
            Some(v) => return Err(InterpreterError::ValueNotCallable(v.to_string())),
            None => return Err(InterpreterError::EmptyStack),
        }
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
        vm.frames.push(CallFrame::new(code));

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack_top().unwrap(), &Value::from(4.6));
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
        vm.frames.push(CallFrame::new(code));

        let _ = vm.run();

        assert_eq!(
            vm.stack_top().unwrap(),
            &Value::from(String::from("foobarbaz"))
        )
    }
}
