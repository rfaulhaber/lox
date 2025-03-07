use super::Chunk;

#[derive(Debug, Clone)]
pub struct Function {
    name: Option<String>,
    chunk: Chunk,
    arity: usize,
}

impl Function {
    pub fn new_named(name: String, chunk: Chunk, arity: usize) -> Self {
        Self {
            name: Some(name),
            chunk,
            arity,
        }
    }

    pub fn new_anonymous(chunk: Chunk, arity: usize) -> Self {
        Self {
            name: None,
            chunk,
            arity,
        }
    }

    pub fn name(&self) -> Option<&String> {
        self.name.as_ref()
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn chunk(&self) -> Chunk {
        // TODO avoid cloning
        self.chunk.clone()
    }
}

impl std::cmp::PartialEq for Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
