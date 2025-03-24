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

    pub(crate) fn func(&self) -> &Function {
        &self.func
    }

    pub fn is_anonymous(&self) -> bool {
        self.func.name().is_none()
    }
}

impl From<Function> for Closure {
    fn from(value: Function) -> Self {
        Closure::new(value)
    }
}
