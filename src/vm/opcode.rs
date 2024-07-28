#[derive(Debug)]
pub enum OpCode {
    Constant,
    Return,
}

#[derive(Debug)]
pub enum Value {
    Float(f64),
}

#[derive(Debug)]
pub struct Chunk {
    code: Vec<OpCode>,
    consts: Vec<Value>,
    lines: Vec<usize>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub(super) fn new() -> Self {
        Chunk {
            code: Vec::new(),
            consts: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub(super) fn write(&mut self, code: OpCode, line: usize) {
        self.code.push(code);
        self.lines.push(line);
    }

    pub(super) fn add_const(&mut self, constant: Value) -> usize {
        self.consts.push(constant);
        self.consts.len() - 1
    }

    pub(super) fn code_at(&self, index: usize) -> Option<&OpCode> {
        self.code.get(index)
    }

    pub(super) fn constant_at(&self, index: usize) -> Option<&Value> {
        self.consts.get(index)
    }
}
