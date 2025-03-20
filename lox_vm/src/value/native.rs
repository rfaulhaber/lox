use thiserror::Error;

use crate::Value;

pub type NativeFunctionType = fn(&[Value]) -> NativeFunctionResult;
pub type NativeFunctionResult = Result<Value, NativeFunctionError>;

#[derive(Debug, Error)]
pub enum NativeFunctionError {}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFunction {
    name: &'static str,
    arity: usize,
    func: NativeFunctionType,
}

impl NativeFunction {
    pub fn new(
        name: &'static str,
        arity: usize,
        func: fn(&[Value]) -> NativeFunctionResult,
    ) -> Self {
        Self { name, arity, func }
    }

    pub fn name(&self) -> &'static str {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn func(&self) -> NativeFunctionType {
        self.func
    }
}
