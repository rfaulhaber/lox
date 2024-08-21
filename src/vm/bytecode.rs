use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Op {
    Constant(usize),
    Return,
    Negate,
}

#[derive(Debug, Clone)]
pub enum Value {
    Float(f64),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Float(float) => write!(f, "{}", float),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Source {
    line: usize,
}

impl Source {
    pub fn new(line: usize) -> Self {
        Self { line }
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub(super) code: Vec<(Op, Source)>,
    pub(super) consts: Vec<Value>,
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
        }
    }

    pub(super) fn write_code(&mut self, code: Op, line: usize) {
        self.code.push((code, Source::new(line)));
    }

    pub(super) fn add_const(&mut self, constant: Value) -> usize {
        let idx = self.consts.len();
        self.consts.push(constant);

        idx
    }

    pub(super) fn code_at(&self, index: usize) -> Option<&(Op, Source)> {
        self.code.get(index)
    }

    pub(super) fn constant_at(&self, index: usize) -> Option<&Value> {
        self.consts.get(index)
    }

    pub fn disassemble(&self) -> Vec<String> {
        self.code
            .iter()
            .enumerate()
            .map(|(idx, (op, source))| {
                let formatted_op = match op {
                    Op::Constant(index) => format!(
                        "OP_CONSTANT (index={}) {}",
                        index,
                        self.constant_at(*index).unwrap(),
                    ),
                    Op::Return => "OP_RETURN".into(),
                    Op::Negate => "OP_NEAGATE".into(),
                };

                format!("{:04}    {:<20}    line {}", idx, formatted_op, source.line)
            })
            .collect()
    }
}
