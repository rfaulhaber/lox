use lox_bytecode::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    String(String),
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    name: String,
    chunk: Chunk,
    arity: usize,
}

impl Function {
    pub fn new(name: String, chunk: Chunk, arity: usize) -> Self {
        Self { name, chunk, arity }
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
