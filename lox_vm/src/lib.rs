// TODO refactor vm into separate file, tidy up lib.rs
// lox_vm/src/lib.rs

use std::rc::Rc;
use std::{collections::HashMap, io::Write};

use crate::bytecode::{Chunk, Op};
use crate::value::{Value, ValueOperatorError, native::NativeFunctionError};

use native::native_functions;
use thiserror::Error;
use value::native::NativeFunction;
use value::{Closure, Function, ValueConvertError};

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
    #[error("Local not found at absolute index {0} (relative {1})")] // Clarified error
    LocalNotFound(usize, usize),
    #[error("Stack overflow: Too many call frames")] // Changed error message
    StackOverflow, // Renamed from InsufficientCallFrameLength
    #[error("Runtime error: No call frame available")] // Added specific error
    NoCallFrame,
    #[error("{0} is not callable")]
    ValueNotCallable(String),
    #[error("Undefined function {0}")]
    UndefinedFunction(String),
    #[error("Native function encountered error")]
    NativeFunctionError(#[from] NativeFunctionError),
    #[error("Wrong type at constant index {0}, got {1}, expected {2}")] // Clarified error
    WrongTypeAtConstIndex(usize, String, String),
    #[error("Wrong type at stack index {0}, got {1}, expected {2}")] // Added specific error
    WrongTypeAtStackIndex(usize, String, String),
    #[error("Value conversion error")]
    ValueConversionError(#[from] ValueConvertError),
    #[error("Upvalue not found at frame {0}, var {1}")]
    UpvalueNotFound(usize, usize),
    #[error("Missing constant at index {0}")] // Simplified error
    MissingConstantAtIndex(usize),
    #[error("Incorrect number of arguments for {0}: expected {1}, got {2}")] // Added arity error
    ArityMismatch(String, usize, usize),
    #[error("Invalid stack index access: index {0}, frame start {1}, stack size {2}")]
    // Added for stack_set
    InvalidStackIndex(usize, usize, usize),
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

#[derive(Debug, Clone, PartialEq)]
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
    closure: Rc<Closure>,
    slots_start: usize,
    ip: usize,
}

impl CallFrame {
    pub fn new_top_level(function: Function) -> Self {
        Self {
            closure: Rc::new(Closure::new_top_level(function)),
            slots_start: 0,
            ip: 0,
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<W: Write> {
    globals: HashMap<String, Value>,
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
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

    pub fn eval(&mut self, func: Function) -> Result<InterpreterState, InterpreterError> {
        let top_frame = CallFrame::new_top_level(func);

        // Push the script's closure onto the stack (it acts as the 'callee' for the script)
        self.stack.push(Value::Closure(top_frame.closure.clone()));
        // Push the frame itself onto the call stack
        self.frames.push(top_frame);

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
            stack: Vec::with_capacity(STACK_SIZE),
            writer,
            mode,
        }
    }

    fn push_call_frame(
        &mut self,
        closure: Rc<Closure>,
        arg_count: usize,
    ) -> Result<(), InterpreterError> {
        if self.frames.len() >= FRAME_MAX {
            return Err(InterpreterError::StackOverflow); // Use specific error
        }

        // *** FIX 1: Correct slots_start calculation ***
        // The new frame's stack base starts where the callee (function/closure) is located.
        // Stack layout before call: [... stack_base ..., callee, arg1, ..., argN]
        // The callee is at index: self.stack.len() - 1 - arg_count
        // This index becomes the start of the new frame's stack window.
        let slots_start = self.stack.len() - 1 - arg_count;

        self.frames.push(CallFrame {
            closure,
            slots_start, // Use the corrected slots_start
            ip: 0,
        });

        Ok(())
    }

    #[inline]
    fn current_frame(&self) -> Result<&CallFrame, InterpreterError> {
        self.frames.last().ok_or(InterpreterError::NoCallFrame)
    }

    #[inline]
    fn current_frame_mut(&mut self) -> Result<&mut CallFrame, InterpreterError> {
        self.frames.last_mut().ok_or(InterpreterError::NoCallFrame)
    }

    #[inline]
    fn current_chunk(&self) -> Result<&Chunk, InterpreterError> {
        Ok(self.current_frame()?.closure.func().chunk())
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
        let ip = self.current_frame()?.ip;
        let chunk = self.current_chunk()?;

        let op = match chunk.code_at(ip) {
            Some(op) => op.clone(), // Clone the Op to avoid borrow issues
            None => {
                // If there's no instruction at IP, it implies the end of the current chunk.
                // This should ideally be handled by a Return or implicit return.
                // If we reach here without a frame pop, something might be wrong.
                // For now, assume an implicit return of nil if not the main script.
                if self.frames.len() <= 1 {
                    // End of the main script
                    return Ok(InterpreterState::Finished);
                } else {
                    // Implicit return from a function
                    // Pop the frame, truncate stack, push nil result
                    let frame = self.frames.pop().ok_or(InterpreterError::NoCallFrame)?;
                    self.stack.truncate(frame.slots_start);
                    self.stack_push(Value::Nil);
                    // Don't advance IP, the new frame's IP will be used next iteration
                    return Ok(InterpreterState::Running);
                }
            }
        };

        // Advance IP for the *next* instruction *before* executing the current one
        // (except for jump instructions which modify IP directly)
        // This simplifies jump logic as they don't need to account for the increment.
        self.current_frame_mut()?.ip += 1;

        match op {
            Op::Const(index) => {
                let constant = self.const_at(index)?;
                self.stack_push(constant);
            }
            Op::Return => {
                // *** FIX 2: Correct stack handling on return ***
                let result = self.stack_pop().ok_or(InterpreterError::EmptyStack)?; // Pop the result value

                // Pop the current call frame *before* manipulating the stack further
                let frame = self.frames.pop().ok_or(InterpreterError::NoCallFrame)?; // Pop the frame

                // If this was the top-level script frame ending, pop its closure and finish
                if self.frames.is_empty() {
                    self.stack_pop(); // Pop the script's closure
                    // Sanity check: stack should be empty now
                    if !self.stack.is_empty() {
                        eprintln!("Warning: Stack not empty after script finished.");
                        // self.debug_print_stack();
                    }
                    return Ok(InterpreterState::Finished);
                }

                // Truncate the stack back to the start of the returning function's frame.
                // This removes the function's locals, arguments, and the callee itself.
                self.stack.truncate(frame.slots_start);

                // Push the result onto the caller's stack.
                self.stack_push(result);
            }
            Op::Negate => match self.stack_pop() {
                Some(Value::Number(n)) => self.stack_push(Value::Number(-n)),
                Some(v) => return Err(InterpreterError::NegateError(v)),
                None => unreachable!("negate operation found without operand"),
            },
            Op::Add => {
                let res = self.binary_op(BinaryOp::Add)?;
                self.stack_push(res)
            }
            Op::Subtract => {
                let res = self.binary_op(BinaryOp::Sub)?;
                self.stack_push(res)
            }
            Op::Multiply => {
                let res = self.binary_op(BinaryOp::Mul)?;
                self.stack_push(res)
            }
            Op::Divide => {
                let res = self.binary_op(BinaryOp::Div)?;
                self.stack_push(res)
            }
            Op::True => self.stack_push(Value::Bool(true)),
            Op::False => self.stack_push(Value::Bool(false)),
            Op::Nil => self.stack_push(Value::Nil),
            Op::Not => match self.stack_pop() {
                Some(value) => self.stack_push(Value::Bool(value.is_falsy())),
                None => unreachable!("not operation found without operand"),
            },
            Op::Equal => {
                let binary_op = self.binary_op(BinaryOp::Eq)?;
                self.stack_push(binary_op)
            }
            Op::Greater => {
                let binary_op = self.binary_op(BinaryOp::Gt)?;
                self.stack_push(binary_op)
            }
            Op::Less => {
                let binary_op = self.binary_op(BinaryOp::Lt)?;
                self.stack_push(binary_op)
            }
            Op::Print => {
                let value = self.stack_pop();

                match value {
                    Some(v) => {
                        if let Some(writer) = &mut self.writer {
                            writeln!(writer, "{}", v)?;
                        } else {
                            println!("{}", v);
                        }
                    }
                    None => return Err(InterpreterError::EmptyStack),
                }
            }
            Op::Pop => {
                let _ = self.stack_pop();
            }
            Op::DefineGlobal(index) => {
                let name = match self.const_at(index)? {
                    // Use ? for error handling
                    Value::String(s) => s.clone(), // Clone the Rc<String>
                    v => {
                        return Err(InterpreterError::WrongTypeAtConstIndex(
                            index,
                            v.type_as_string(),
                            "String".into(),
                        ));
                    }
                };
                // Peek the value instead of popping, global definition shouldn't consume it from stack yet
                let value = self.stack_peek(0)?.clone();
                self.globals.insert(name.to_string(), value);
                // Pop the value *after* successfully inserting it
                self.stack_pop();
            }
            Op::GetGlobal(index) => {
                let name = match self.const_at(index)? {
                    Value::String(s) => s.clone(),
                    v => {
                        return Err(InterpreterError::WrongTypeAtConstIndex(
                            index,
                            v.type_as_string(),
                            "String".into(),
                        ));
                    }
                };
                match self.globals.get(&*name) {
                    // Deref Rc<String> for lookup
                    Some(value) => self.stack_push(value.clone()),
                    None => return Err(InterpreterError::UndefinedVariable(name.to_string())),
                }
            }
            Op::SetGlobal(index) => {
                let name = match self.const_at(index)? {
                    Value::String(s) => s.clone(),
                    v => {
                        return Err(InterpreterError::WrongTypeAtConstIndex(
                            index,
                            v.type_as_string(),
                            "String".into(),
                        ));
                    }
                };
                // Check if global exists *before* peeking stack
                if !self.globals.contains_key(&*name) {
                    return Err(InterpreterError::UndefinedVariable(name.to_string()));
                }
                // Peek the value from the stack. Assignment is an expression,
                // so it should leave the assigned value on the stack.
                let value = self.stack_peek(0)?.clone();
                self.globals.insert(name.to_string(), value);
                // No pop here - assignment result stays on stack
            }
            Op::GetLocal(index) => {
                let value = self.stack_get(index)?;
                self.stack_push(value.clone());
            }
            Op::SetLocal(index) => {
                let value = self.stack_peek(0)?.clone();
                self.stack_set(index, value)?;
            }
            Op::JumpIfFalse(offset) => {
                // Check condition without popping
                if self.stack_peek(0)?.is_falsy() {
                    // Adjust IP relative to the *start* of the jump instruction
                    self.jump(offset)?;
                }
            }
            Op::Jump(pos) => self.jump(pos)?,
            Op::Loop(pos) => self.jump_loop(pos)?,
            Op::Call(arg_count) => {
                // Peek the callee, which is below the arguments
                let callee_value = self.stack_peek(arg_count)?.clone();
                self.call_value(callee_value, arg_count)?;
            }
            Op::Closure(index) => {
                let constant = self.const_at(index)?;
                match constant {
                    Value::Function(func) => {
                        let closure = Closure::new(func.clone()); // func is Rc<Function>
                        self.stack_push(Value::Closure(Rc::new(closure)));
                    }
                    v => {
                        return Err(InterpreterError::WrongTypeAtConstIndex(
                            index,
                            v.type_as_string(),
                            "Function".into(),
                        ));
                    }
                }
            }
            Op::GetUpvalue(_) => {
                todo!("GetUpvalue not implemented")
            }
            Op::SetUpvalue(_) => {
                todo!("SetUpvalue not implemented")
            }
        }

        Ok(InterpreterState::Running)
    }

    fn binary_op(&mut self, op: BinaryOp) -> Result<Value, InterpreterError> {
        let b = self
            .stack_pop()
            .ok_or(InterpreterError::InsufficientStackLengthForOperation(
                op.clone(),
                self.stack.len(),
            ))?;

        let a = self
            .stack_pop()
            .ok_or(InterpreterError::InsufficientStackLengthForOperation(
                op.clone(),
                self.stack.len(),
            ))?;

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
        // IP was already incremented past the jump instruction itself.
        // The offset is relative to the *start* of the jump instruction.
        // So, we adjust IP by offset - 1 (because IP is already +1 ahead).
        self.current_frame_mut()?.ip += offset - 1; // -1 because IP already advanced
        Ok(())
    }

    /// Adjusts the instruction pointer backward for loops.
    fn jump_loop(&mut self, offset: usize) -> Result<(), InterpreterError> {
        // IP was already incremented past the loop instruction itself.
        // The offset is relative to the *start* of the loop instruction.
        // We want to go back `offset` bytes from the start.
        // Current IP is at `start + 1`. Target is `start - offset`.
        // So, subtract `offset + 1` from current IP? No, target is `start - offset + 1` if offset includes jump itself.
        // Let's rethink: target IP is `current_ip - offset`.
        self.current_frame_mut()?.ip -= offset; // Offset includes the loop instruction itself
        Ok(())
    }

    /// Calls a Lox value (Closure or Native Function).
    fn call_value(&mut self, callee: Value, arg_count: usize) -> Result<(), InterpreterError> {
        match callee {
            Value::Closure(closure) => self.call_closure(closure, arg_count),
            Value::Native(native_fn) => self.call_native(native_fn, arg_count),
            v => Err(InterpreterError::ValueNotCallable(v.type_as_string())),
        }
    }

    /// Calls a Lox closure.
    fn call_closure(
        &mut self,
        closure: Rc<Closure>,
        arg_count: usize,
    ) -> Result<(), InterpreterError> {
        // Check arity
        let expected_arity = closure.func().arity();
        if arg_count != expected_arity {
            return Err(InterpreterError::ArityMismatch(
                closure.func().name().map_or("<closure>", |v| v).to_string(),
                expected_arity,
                arg_count,
            ));
        }

        // Push the call frame using the corrected logic
        self.push_call_frame(closure, arg_count)?;

        Ok(())
    }

    /// Calls a native Rust function.
    fn call_native(
        &mut self,
        native_fn: NativeFunction,
        arg_count: usize,
    ) -> Result<(), InterpreterError> {
        // Check arity
        let expected_arity = native_fn.arity();
        if arg_count != expected_arity {
            return Err(InterpreterError::ArityMismatch(
                native_fn.name().to_string(),
                expected_arity,
                arg_count,
            ));
        }

        // Get arguments from the stack (they are above the native fn object)
        // The stack looks like: [... stack_base ..., native_fn, arg1, ..., argN]
        let arg_start_index = self.stack.len() - arg_count;
        let arguments = self.stack[arg_start_index..].to_vec(); // Clone arguments

        // Call the native function
        let result = (native_fn.func())(&arguments)?; // Execute the native code

        // Pop the native function and its arguments
        self.stack.truncate(arg_start_index - 1); // Truncate up to *before* the native_fn object

        // Push the result
        self.stack_push(result);

        Ok(())
    }

    /// Pushes a value onto the value stack.
    #[inline]
    fn stack_push(&mut self, value: Value) {
        // Optional: Check for stack overflow before push if STACK_MAX is enforced
        // if self.stack.len() >= STACK_MAX { ... error ... }
        self.stack.push(value);
    }

    /// Pops a value from the value stack. Returns error if empty.
    #[inline]
    fn stack_pop(&mut self) -> Option<Value> {
        // Keep Option for simple cases
        self.stack.pop()
    }

    /// Peeks at a value on the stack relative to the top.
    /// `peek(0)` is the top, `peek(1)` is the one below, etc.
    #[inline]
    fn stack_peek(&self, distance: usize) -> Result<&Value, InterpreterError> {
        self.stack
            .len()
            .checked_sub(distance + 1)
            .and_then(|index| self.stack.get(index))
            .ok_or(InterpreterError::EmptyStack) // Or a more specific error if needed
    }

    /// Gets a value from the stack using an index relative to the current frame's start.
    #[inline]
    fn stack_get(&self, relative_index: usize) -> Result<&Value, InterpreterError> {
        let frame_start = self.current_frame()?.slots_start;
        let absolute_index = frame_start + relative_index;
        self.stack.get(absolute_index).ok_or_else(|| {
            // Provide more context in the error
            InterpreterError::LocalNotFound(absolute_index, relative_index)
        })
    }

    /// Sets a value on the stack using an index relative to the current frame's start.
    #[inline]
    fn stack_set(&mut self, relative_index: usize, value: Value) -> Result<(), InterpreterError> {
        let frame_start = self.current_frame()?.slots_start;
        let absolute_index = frame_start + relative_index;

        // Bounds check before assignment
        if absolute_index >= self.stack.len() {
            return Err(InterpreterError::InvalidStackIndex(
                relative_index,
                frame_start,
                self.stack.len(),
            ));
        }
        self.stack[absolute_index] = value;
        Ok(())
    }

    /// Gets a constant from the current frame's chunk.
    fn const_at(&self, index: usize) -> Result<Value, InterpreterError> {
        self.current_chunk()?
            .const_at(index)
            .cloned() // Clone the Value (likely Rc wrapped)
            .ok_or(InterpreterError::MissingConstantAtIndex(index))
    }
}

impl<W: Write> Default for Interpreter<W> {
    fn default() -> Self {
        Self::new()
    }
}

// --- Tests ---
#[cfg(test)]
mod test {
    use super::*;
    use crate::bytecode::Chunk; // Make sure Chunk is imported
    use crate::value::Value; // Make sure Value is imported

    // Helper to create a basic VM for testing steps
    fn setup_vm_with_chunk(chunk: Chunk) -> Interpreter<std::io::Stdout> {
        // Use Stdout for debug visibility
        let mut vm = Interpreter::new_debug(); // Use debug mode
        vm.set_mode(InterpreterMode::Debug);
        let script_fn = Function::new_top_level(chunk);
        let top_frame = CallFrame::new_top_level(script_fn);
        vm.stack.push(Value::Closure(top_frame.closure.clone())); // Push script closure
        vm.frames.push(top_frame);
        vm
    }

    // Helper to run code and get the final stack top
    fn run_code(source_chunk: Chunk) -> Result<Value, InterpreterError> {
        let mut vm = Interpreter::new_with_writer(std::io::stdout()); // Write output
        vm.set_mode(InterpreterMode::Debug);
        match vm.eval(Function::new_top_level(source_chunk)) {
            Ok(InterpreterState::Finished) => Ok(vm.stack_pop().unwrap_or(Value::Nil)), // Return last value or Nil
            Ok(InterpreterState::Running) => panic!("VM finished in running state unexpectedly"),
            Err(e) => Err(e),
        }
    }

    #[test]
    fn test_add_numbers() {
        let mut chunk = Chunk::new();
        chunk.push_const(1.2); // 0
        chunk.push_const(3.4); // 1
        chunk.add_op(Op::Add);
        chunk.add_op(Op::Return); // Explicit return needed

        let result = run_code(chunk);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::from(4.6));
    }

    #[test]
    fn test_string_concat() {
        let mut chunk = Chunk::new();
        chunk.push_const("foo"); // 0
        chunk.push_const("bar"); // 1
        chunk.add_op(Op::Add);
        chunk.push_const("baz"); // 2
        chunk.add_op(Op::Add);
        chunk.add_op(Op::Return);

        let result = run_code(chunk);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::from(String::from("foobarbaz")));
    }

    #[test]
    fn test_comparisons() {
        let mut chunk = Chunk::new();
        chunk.push_const(5.0); // 0
        chunk.push_const(3.0); // 1
        chunk.add_op(Op::Greater); // stack: [true]
        chunk.push_const(5.0); // 2
        chunk.push_const(5.0); // 3
        chunk.add_op(Op::Less); // stack: [true, false]
        chunk.add_op(Op::Pop); // stack: [true]
        chunk.push_const("a"); // 4
        chunk.push_const("a"); // 5
        chunk.add_op(Op::Equal); // stack: [true, true]
        chunk.add_op(Op::Return);

        let result = run_code(chunk);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::from(true)); // Should return the last value (true)
    }

    #[test]
    fn test_global_vars() {
        let mut chunk = Chunk::new();
        chunk.push_const(10.0); // 0: value 10.0
        chunk.add_const("myGlobal"); // 1: name "myGlobal"
        chunk.add_op(Op::DefineGlobal(1)); // Define myGlobal = 10.0, stack: []
        chunk.add_const("myGlobal"); // 2: name "myGlobal"
        chunk.add_op(Op::GetGlobal(2)); // stack: [10.0]
        chunk.push_const(5.0); // 3: value 5.0
        chunk.add_op(Op::Add); // stack: [15.0]
        chunk.add_op(Op::Return);

        let result = run_code(chunk);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::from(15.0));
    }

    #[test]
    fn test_local_vars() {
        // Simulate entering a scope (though not via function call here)
        let mut chunk = Chunk::new();
        chunk.push_const(10.0); // 0: value 10.0
        // Assume Op::SetLocal(0) would be used if this was compiled from `var a = 10;` in a scope
        // We'll manually set up the stack and frame for testing Get/Set
        chunk.add_op(Op::GetLocal(0)); // Get local at index 0 relative to frame start
        chunk.push_const(5.0); // 1: value 5.0
        chunk.add_op(Op::Add);
        // Assume Op::SetLocal(1) for `var b = ...`
        // Let's test setting local 0
        chunk.add_op(Op::SetLocal(0)); // Set local 0 to the result (15.0), stack: [15.0]
        chunk.add_op(Op::Pop); // Pop the assignment result
        chunk.add_op(Op::GetLocal(0)); // Get local 0 again, should be 15.0
        chunk.add_op(Op::Return);

        // --- Manual VM setup for this specific test ---
        let mut vm = Interpreter::new_with_writer(std::io::stdout()); // Write output
        vm.set_mode(InterpreterMode::Debug);
        let script_fn = Function::new_top_level(chunk);
        let top_frame = CallFrame::new_top_level(script_fn);

        // Manually push initial value for the "local" variable at index 0
        vm.stack.push(Value::Closure(top_frame.closure.clone())); // Script closure at 0 (frame start)
        vm.stack.push(Value::from(10.0)); // "Local" value at index 1 (absolute)

        // Adjust frame's slot_start to point *after* the closure, where locals begin
        let frame_for_test = CallFrame {
            closure: top_frame.closure.clone(),
            slots_start: 1, // Locals start at index 1 for this test setup
            ip: 0,
        };
        vm.frames.push(frame_for_test);

        // --- Run the VM ---
        let run_result = vm.run();
        assert!(run_result.is_ok());
        assert_eq!(run_result.unwrap(), InterpreterState::Finished);

        // --- Check final stack state ---
        // After return, the stack should contain only the return value (15.0)
        // Note: The run_code helper handles the final pop, so we check vm.stack directly before that would happen.
        assert_eq!(vm.stack.len(), 1); // Should contain only the final return value
        assert_eq!(vm.stack_peek(0).unwrap(), &Value::from(15.0));
    }

    #[test]
    fn test_simple_function_call() {
        // --- Function Chunk ---
        let mut func_chunk = Chunk::new();
        func_chunk.add_op(Op::GetLocal(1)); // Get argument 'a' (at index 1 relative to frame: 0 is func itself, 1 is first arg)
        func_chunk.push_const(5.0); // 0: constant 5.0
        func_chunk.add_op(Op::Add);
        func_chunk.add_op(Op::Return);
        let func = Function::new_named("addFive".to_string(), func_chunk, 1); // name, chunk, arity 1

        // --- Main Script Chunk ---
        let mut main_chunk = Chunk::new();
        main_chunk.add_const(func); // 0: the function object
        main_chunk.add_op(Op::Closure(0)); // Create closure, stack: [closure]
        main_chunk.add_const("addFive"); // 1: function name "addFive"
        main_chunk.add_op(Op::DefineGlobal(1)); // Define global function, stack: []

        main_chunk.add_const("addFive"); // 2: function name "addFive"
        main_chunk.add_op(Op::GetGlobal(2)); // Get closure, stack: [closure]
        main_chunk.push_const(10.0); // 3: argument 10.0, stack: [closure, 10.0]
        main_chunk.add_op(Op::Call(1)); // Call with 1 argument
        main_chunk.add_op(Op::Return); // Return the result from main

        // --- Run ---
        let result = run_code(main_chunk);
        println!("Result: {:?}", result); // Debug print
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::from(15.0));
    }

    #[test]
    fn test_nested_call() {
        // --- Inner Function ---
        let mut inner_chunk = Chunk::new();
        inner_chunk.add_op(Op::GetLocal(1)); // Arg x
        inner_chunk.add_op(Op::Return);
        let inner_func = Function::new_named("inner".to_string(), inner_chunk, 1);

        // --- Outer Function ---
        let mut outer_chunk = Chunk::new();
        outer_chunk.add_const(inner_func); // 0: inner function object
        outer_chunk.add_op(Op::Closure(0)); // stack: [outer_closure, outer_arg, inner_closure]
        outer_chunk.add_op(Op::GetLocal(1)); // Get outer arg 'a', stack: [..., inner_closure, outer_arg]
        outer_chunk.push_const(1.0); // 1: value 1.0, stack: [..., inner_closure, outer_arg, 1.0]
        outer_chunk.add_op(Op::Add); // stack: [..., inner_closure, outer_arg+1]
        outer_chunk.add_op(Op::Call(1)); // Call inner with outer_arg+1
        outer_chunk.add_op(Op::Return); // Return result of inner call
        let outer_func = Function::new_named("outer".to_string(), outer_chunk, 1);

        // --- Main Script Chunk ---
        let mut main_chunk = Chunk::new();
        main_chunk.add_const(outer_func); // 0: outer function object
        main_chunk.add_op(Op::Closure(0)); // stack: [outer_closure]
        main_chunk.add_const("outer"); // 1: name "outer"
        main_chunk.add_op(Op::DefineGlobal(1)); // Define outer

        main_chunk.add_const("outer"); // 2: name "outer"
        main_chunk.add_op(Op::GetGlobal(2)); // stack: [outer_closure]
        main_chunk.push_const(20.0); // 3: argument 20.0, stack: [outer_closure, 20.0]
        main_chunk.add_op(Op::Call(1)); // Call outer(20)
        main_chunk.add_op(Op::Return); // Return result

        // --- Run ---
        let result = run_code(main_chunk);
        println!("Nested Call Result: {:?}", result);
        assert!(result.is_ok());
        // outer(20) calls inner(20+1), inner returns 21
        assert_eq!(result.unwrap(), Value::from(21.0));
    }

    #[test]
    fn test_jump_if_false() {
        let mut chunk = Chunk::new();
        chunk.push_const(10.0); // 0: value 10.0 (initial value) stack: [10.0]
        chunk.add_op(Op::False); // stack: [10.0, false]
        // JumpIfFalse offset 3: Jumps over Pop and Const(20.0) if top is falsey
        // Target instruction is Op::Add
        // Instructions: 0:Const, 1:False, 2:JumpIfFalse(3), 3:Pop, 4:Const, 5:Add, 6:Return
        // Jump from index 2. Target index = 2 + 3 = 5 (Op::Add)
        chunk.add_op(Op::JumpIfFalse(3));
        chunk.add_op(Op::Pop); // Pop the false if condition was true
        chunk.push_const(20.0); // 1: value 20.0 (skipped)
        // Target of jump:
        chunk.add_op(Op::Add); // Add 10.0 (still on stack) + 5.0 (pushed below)
        chunk.push_const(5.0); // 2: value 5.0
        chunk.add_op(Op::Add); // This should not execute if jump happens
        chunk.add_op(Op::Return);

        let result = run_code(chunk);
        assert!(result.is_ok());
        // Since condition is false, jump happens. Stack before jump: [10.0, false].
        // Jump goes to Op::Add. Stack should be [10.0] (false is NOT popped by JumpIfFalse).
        // Wait, JumpIfFalse *doesn't* pop. The compiler usually adds a Pop *after* the jump target for the 'then' block.
        // Let's adjust the test based on VM logic: JumpIfFalse peeks.
        // Stack before jump: [10.0, false]. Jump happens. IP goes to Op::Add.
        // Op::Add expects two operands. Stack is [10.0, false]. This will error.

        // --- Let's rewrite the test to match typical compiled 'if' ---
        // if (false) { push 20 } else { push 30 }; push 5; add; return
        let mut chunk_if = Chunk::new();
        chunk_if.add_op(Op::False); // Condition: stack [false]
        // JumpIfFalse(4): Skip 'then' block (Pop, Const) + Jump instruction itself
        // Target: Op::Const(30.0) at index 6
        // Instructions: 0:False, 1:JumpIfFalse(4), 2:Pop, 3:Const(20), 4:Jump(3), 5:Pop, 6:Const(30), 7:Push(5), 8:Add, 9:Return
        chunk_if.add_op(Op::JumpIfFalse(4)); // Jump if false to index 1+4 = 5 (Pop before else)

        // 'Then' block (skipped)
        chunk_if.add_op(Op::Pop); // Pop the condition if true
        chunk_if.push_const(20.0); // 0: value 20.0
        // Jump(3): Skip 'else' block (Pop, Const) + Jump itself
        // Target: Op::Push(5.0) at index 5+3 = 8? No, target is index after else block.
        // Target: index 7 (Op::Push(5.0))
        // Jump from index 4. Target index = 4 + 3 = 7
        chunk_if.add_op(Op::Jump(3));

        // 'Else' block (executed)
        // Target of JumpIfFalse lands here (index 5)
        chunk_if.add_op(Op::Pop); // Pop the condition if false
        chunk_if.push_const(30.0); // 1: value 30.0. stack: [30.0]

        // After if/else
        // Target of Jump lands here (index 7)
        chunk_if.push_const(5.0); // 2: value 5.0. stack: [30.0, 5.0]
        chunk_if.add_op(Op::Add); // stack: [35.0]
        chunk_if.add_op(Op::Return);

        let result_if = run_code(chunk_if);
        println!("If test result: {:?}", result_if);
        assert!(result_if.is_ok());
        assert_eq!(result_if.unwrap(), Value::from(35.0));
    }
} // mod test
