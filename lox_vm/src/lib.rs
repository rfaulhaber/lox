// TODO refactor vm into separate file, tidy up lib.rs

use std::{collections::HashMap, io::Write};

use crate::bytecode::{Chunk, Op};
use crate::value::{Function, Object, Value, ValueOperatorError, native::NativeFunctionError};

use native::native_functions;
use thiserror::Error;
use value::{Closure, ValueConvertError};

pub mod bytecode;
mod native;
pub mod value;

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
    #[error("Undefined function {0}")]
    UndefinedFunction(String),
    #[error("Native function encountered error")]
    NativeFunctionError(#[from] NativeFunctionError),
    #[error("Wrong type at index {0}, got {1}, expected {2}")]
    WrongTypeAtIndex(usize, String, String),
    #[error("Value conversion error")]
    ValueConversionError(#[from] ValueConvertError),
    #[error("Upvalue not found at frame {0}, var {1}")]
    UpvalueNotFound(usize, usize),
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
pub struct Interpreter<W: Write> {
    globals: HashMap<String, Value>,
    frames: Vec<CallFrame>,
    mode: InterpreterMode,
    writer: Option<W>,
}

impl<W: Write> Interpreter<W> {
    pub fn new() -> Self {
        Interpreter::new_vm(InterpreterMode::Normal, None)
    }

    pub fn new_debug() -> Self {
        Interpreter::new_vm(InterpreterMode::Debug, None)
    }

    pub fn new_with_writer(writer: W) -> Self {
        Interpreter::new_vm(InterpreterMode::Normal, Some(writer))
    }

    pub fn set_mode(&mut self, mode: InterpreterMode) {
        self.mode = mode;
    }

    pub fn eval(&mut self, chunk: Chunk) -> Result<InterpreterState, InterpreterError> {
        self.frames.push(CallFrame::new(chunk));

        self.run()
    }

    fn new_vm(mode: InterpreterMode, writer: Option<W>) -> Self {
        let mut globals = HashMap::new();

        for f in native_functions() {
            globals.insert(f.name().into(), f.into());
        }

        Interpreter {
            globals,
            frames: Vec::with_capacity(FRAME_MAX),
            writer,
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
            Some(Op::Const(index)) => {
                let constant = self.const_at(index).cloned().unwrap();
                self.stack_push(constant);
            }
            Some(Op::Return) => {
                let result = self.stack_pop();

                if result.is_none() {
                    return Err(InterpreterError::EmptyStack);
                }

                let _ = self.frames.pop();

                if self.frames.is_empty() {
                    return Ok(InterpreterState::Finished);
                }

                self.stack_push(result.unwrap());
            }
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
                        if let Some(writer) = &mut self.writer {
                            writeln!(writer, "{}", v)?;
                        }
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Some(Op::Pop) => {
                let _ = self.stack_pop();
            }
            Some(Op::DefineGlobal(index)) => {
                let value = self.stack_pop();
                let name = match self.const_at(index) {
                    Some(Value::Object(Object::String(s))) => s,
                    v => {
                        return Err(InterpreterError::WrongTypeAtIndex(
                            index,
                            "string".to_string(),
                            format!("{:?}", v),
                        ));
                    }
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                match value {
                    Some(v) => {
                        self.globals.insert(name.to_string(), v);
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Some(Op::GetGlobal(index)) => {
                let name = match self.const_at(index) {
                    Some(Value::Object(Object::String(s))) => s,
                    v => {
                        return Err(InterpreterError::WrongTypeAtIndex(
                            index,
                            "String".into(),
                            format!("{:?}", v),
                        ));
                    }
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                let value = match self.globals.get(name) {
                    Some(value) => value,
                    None => return Err(InterpreterError::UndefinedVariable(name.to_string())),
                };

                self.stack_push(value.clone());
            }
            Some(Op::SetGlobal(index)) => {
                let name = match self.const_at(index) {
                    Some(Value::Object(Object::String(s))) => s,
                    v => {
                        return Err(InterpreterError::WrongTypeAtIndex(
                            index,
                            "String".into(),
                            format!("{:?}", v),
                        ));
                    }
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                if !self.globals.contains_key(name) {
                    return Err(InterpreterError::UndefinedVariable(name.to_string()));
                }

                let value = match self.stack_top() {
                    Some(value) => value.clone(),
                    None => return Err(InterpreterError::EmptyStack),
                };

                let _ = self.globals.insert(name.to_string(), value);
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
            Some(Op::Closure(index)) => {
                let func = match self.const_at(index) {
                    Some(value) => value.clone(),
                    None => return Err(InterpreterError::NoValueAtIndex(index)),
                };

                let func_value: Closure = func.try_into()?;

                if func_value.name().is_none() {
                    let closure = Value::from(func_value);
                    self.stack_push(closure);
                } else {
                    let _ = self
                        .globals
                        .insert(func_value.name().unwrap().to_string(), func_value.into());
                }
            }
            Some(Op::GetUpvalue(frame_idx, var_idx)) => {
                todo!()
            }
            Some(Op::SetUpvalue(frame_idx, var_idx)) => {
                todo!()
            }
            None => {
                if self.frames.len() <= 1 {
                    return Ok(InterpreterState::Finished);
                }

                let _ = self.frames.pop();
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

    fn const_at(&self, index: usize) -> Option<&Value> {
        self.frames.last().and_then(|f| f.chunk.const_at(index))
    }

    fn get_ip(&self) -> Result<usize, InterpreterError> {
        self.frames
            .last()
            .map(|frame| frame.ip)
            .ok_or(InterpreterError::InsufficientCallFrameLength)
    }

    fn call_fn(&mut self, arg_count: usize) -> Result<(), InterpreterError> {
        let callee = self.stack_get(self.stack_len() - arg_count - 1).cloned();

        match callee {
            Some(Value::Object(Object::Closure(closure))) => {
                self.eval_callable(closure.func().chunk(), arg_count)
            }
            Some(Value::Object(Object::Function(func))) => {
                self.eval_callable(func.chunk(), arg_count)
            }
            Some(Value::Object(Object::Native(f))) => {
                let mut arguments = Vec::new();

                for _ in 0..f.arity() {
                    arguments.push(self.stack_pop().ok_or(InterpreterError::EmptyStack)?);
                }

                let value = f.func()(&arguments)?;
                self.stack_push(value);

                Ok(())
            }
            Some(v) => return Err(InterpreterError::ValueNotCallable(v.to_string())),
            None => return Err(InterpreterError::EmptyStack),
        }
    }

    fn eval_callable(&mut self, chunk: Chunk, arg_count: usize) -> Result<(), InterpreterError> {
        let mut call_frame = CallFrame::new(chunk);

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

                vals.into_iter().rev().collect()
            })
            .unwrap_or(Vec::new());

        self.frames.push(call_frame);

        Ok(())
    }
}

impl<W: Write> Default for Interpreter<W> {
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
        code.push_const(1.2);
        code.push_const(3.4);

        code.add_op(Op::Add);

        let mut vm: Interpreter<std::io::Empty> = Interpreter::new();
        vm.frames.push(CallFrame::new(code));

        let _ = vm.step();
        let _ = vm.step();
        let _ = vm.step();

        assert_eq!(vm.stack_top().unwrap(), &Value::from(4.6));
    }

    #[test]
    fn string_concat() {
        let mut code = Chunk::new();
        code.push_const("foo");
        code.push_const("bar");
        code.push_const("baz");

        code.add_op(Op::Add);
        code.add_op(Op::Add);

        let mut vm: Interpreter<std::io::Empty> = Interpreter::new();
        vm.frames.push(CallFrame::new(code));

        let _ = vm.run();

        assert_eq!(
            vm.stack_top().unwrap(),
            &Value::from(String::from("foobarbaz"))
        )
    }
}
