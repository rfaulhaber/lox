use lox_source::source::Span;

#[derive(Debug, Clone)]
pub enum Op {
    Constant(usize),
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    code: Vec<(Op, Span)>,
    numbers: Vec<f64>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            numbers: Vec::new(),
        }
    }

    pub fn add_op(&mut self, code: Op, location: Span) {
        self.code.push((code, location));
    }

    pub fn add_number(&mut self, number: f64) -> usize {
        let idx = self.numbers.len();
        self.numbers.push(number);

        idx
    }

    fn code_at(&self, index: usize) -> Option<&(Op, Span)> {
        self.code.get(index)
    }

    fn number_at(&self, index: usize) -> Option<f64> {
        self.numbers.get(index).copied()
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
                        self.number_at(*index).unwrap(),
                    ),
                    Op::Return => "OP_RETURN".into(),
                    Op::Negate => "OP_NEAGATE".into(),
                    Op::Add => "OP_ADD".into(),
                    Op::Subtract => "OP_SUBTRACT".into(),
                    Op::Multiply => "OP_MULTIPLY".into(),
                    Op::Divide => "OP_DIVIDE".into(),
                };

                format!(
                    "{:04}    {:<20}    offset/length {}/{}",
                    idx, formatted_op, source.offset, source.length
                )
            })
            .collect()
    }
}
