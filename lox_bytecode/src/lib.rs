use lox_source::source::Span;

#[derive(Debug, Clone)]
pub enum Op {
    Integer(usize),
    Float(usize),
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    code: Vec<Op>,
    floats: Vec<f64>,
    ints: Vec<i64>,
    locations: Vec<(usize, Span)>,
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
            floats: Vec::new(),
            ints: Vec::new(),
            locations: Vec::new(),
        }
    }

    pub fn add_sourced_op(&mut self, code: Op, location: Span) {
        let idx = self.code.len();
        self.code.push(code);
        self.locations.push((idx, location));
    }

    pub fn add_op(&mut self, code: Op) {
        self.code.push(code);
    }

    pub fn add_float(&mut self, number: f64) -> usize {
        let idx = self.floats.len();
        self.floats.push(number);

        idx
    }

    pub fn add_int(&mut self, number: i64) -> usize {
        let idx = self.ints.len();
        self.ints.push(number);

        idx
    }

    pub fn code_at(&self, index: usize) -> Option<&Op> {
        self.code.get(index)
    }

    pub fn float_at(&self, index: usize) -> Option<f64> {
        self.floats.get(index).copied()
    }

    pub fn int_at(&self, index: usize) -> Option<i64> {
        self.ints.get(index).copied()
    }

    pub fn disassemble(&self) -> Vec<String> {
        self.code
            .iter()
            .enumerate()
            .map(|(idx, op)| {
                let source = self.locations.iter().find(|(location, _)| *location == idx);
                let formatted_op = match op {
                    Op::Integer(index) => format!(
                        "OP_INTEGER (index={}) {}",
                        index,
                        self.int_at(*index).unwrap(),
                    ),
                    Op::Float(index) => format!(
                        "OP_FLOAT (index={}) {}",
                        index,
                        self.float_at(*index).unwrap(),
                    ),
                    Op::Return => "OP_RETURN".into(),
                    Op::Negate => "OP_NEAGATE".into(),
                    Op::Add => "OP_ADD".into(),
                    Op::Subtract => "OP_SUBTRACT".into(),
                    Op::Multiply => "OP_MULTIPLY".into(),
                    Op::Divide => "OP_DIVIDE".into(),
                };

                if let Some((_, source)) = source {
                    return format!(
                        "{:04}    {:<20}    offset/length {}/{}",
                        idx, formatted_op, source.offset, source.length
                    );
                } else {
                    return format!("{:04}    {:<20}", idx, formatted_op,);
                }
            })
            .collect()
    }
}
