use super::Function;

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    func: Function,
}

impl Closure {
    pub fn new(func: Function) -> Self {
        Self { func }
    }

    pub fn name(&self) -> Option<&String> {
        self.func.name()
    }

    pub fn arity(&self) -> usize {
        self.func.arity()
    }
}
