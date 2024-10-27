use crate::value::Value;

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
pub struct Context {
    pub(super) code: Vec<(Op, Source)>,
    pub(super) consts: Vec<Value>,
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

impl Context {
    pub(crate) fn new() -> Self {
        Context {
            code: Vec::new(),
            consts: Vec::new(),
        }
    }

    pub(crate) fn write_code(&mut self, code: Op, line: usize) {
        self.code.push((code, Source::new(line)));
    }

    pub(crate) fn add_const(&mut self, constant: Value) -> usize {
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
                    Op::Add => "OP_ADD".into(),
                    Op::Subtract => "OP_SUBTRACT".into(),
                    Op::Multiply => "OP_MULTIPLY".into(),
                    Op::Divide => "OP_DIVIDE".into(),
                };

                format!("{:04}    {:<20}    line {}", idx, formatted_op, source.line)
            })
            .collect()
    }

    pub fn merge(&mut self, other: &mut Context) -> &Context {
        self.code.append(&mut other.code);
        self.consts.append(&mut other.consts);

        self
    }
}
