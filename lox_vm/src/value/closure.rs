use std::rc::Rc;

use crate::bytecode::Chunk;

use super::{Function, Upvalue};

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    func: Rc<Function>,
    upvalues: Vec<Upvalue>,
}

impl Closure {
    pub fn new(func: Rc<Function>) -> Self {
        Self {
            func,
            upvalues: Vec::new(),
        }
    }

    pub fn new_top_level(chunk: Chunk) -> Self {
        Self {
            func: Rc::new(Function::new_top_level(chunk)),
            upvalues: Vec::new(),
        }
    }

    pub fn name(&self) -> Option<&String> {
        self.func.name()
    }

    pub fn arity(&self) -> usize {
        self.func.arity()
    }

    pub(crate) fn func(&self) -> &Function {
        &self.func
    }

    pub fn is_anonymous(&self) -> bool {
        self.func.name().is_none()
    }
}

impl From<Function> for Closure {
    fn from(value: Function) -> Self {
        Closure::new(Rc::new(value))
    }
}
