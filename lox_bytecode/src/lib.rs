use lox_source::source::Span;

mod function;

pub use function::Function;

#[derive(Debug, Clone)]
pub enum Op {
    Integer(usize),
    Float(usize),
    String(usize),
    Fn(usize),
    Return,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    True,
    False,
    Nil,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal(usize),
    GetGlobal(usize),
    SetGlobal(usize),
    GetLocal(usize),
    SetLocal(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(usize),
}

#[derive(Debug, Clone)]
pub struct Chunk {
    code: Vec<Op>,
    floats: Vec<f64>,
    ints: Vec<i64>,
    strings: Vec<String>,
    fns: Vec<Function>,
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
            strings: Vec::new(),
            fns: Vec::new(),
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

    pub fn add_string(&mut self, string: String) -> usize {
        let idx = self.strings.len();
        self.strings.push(string.trim_matches('"').to_string());

        idx
    }

    pub fn add_fn(&mut self, f: Function) -> usize {
        let idx = self.fns.len();
        self.fns.push(f);

        idx
    }

    pub fn push_float(&mut self, number: f64) {
        let idx = self.add_float(number);
        self.add_op(Op::Float(idx));
    }

    pub fn push_int(&mut self, number: i64) {
        let idx = self.add_int(number);
        self.add_op(Op::Integer(idx));
    }

    pub fn push_string(&mut self, string: String) {
        let idx = self.add_string(string);
        self.add_op(Op::String(idx));
    }

    pub fn push_fn(&mut self, f: Function) {
        let idx = self.add_fn(f);
        self.add_op(Op::Fn(idx));
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

    pub fn string_at(&self, index: usize) -> Option<String> {
        self.strings.get(index).cloned()
    }

    pub fn fn_at(&self, index: usize) -> Option<Function> {
        self.fns.get(index).cloned()
    }

    pub fn code_len(&self) -> usize {
        self.code.len()
    }

    pub fn set_op(&mut self, index: usize, op: Op) {
        self.code[index] = op;
    }

    pub fn insert_op(&mut self, index: usize, op: Op) {
        self.code.insert(index, op);
    }

    pub fn disassemble(&self) -> Vec<String> {
        let mut main_body: Vec<String> = self
            .code
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
                    Op::String(index) => format!(
                        "OP_STRING (index={}) {}",
                        index,
                        self.string_at(*index).unwrap(),
                    ),
                    Op::Fn(index) => format!(
                        "OP_FN (index={}) {}",
                        index,
                        self.fn_at(*index)
                            .unwrap()
                            .name()
                            .map_or("anonymous", |v| v)
                    ),
                    Op::DefineGlobal(index) => format!(
                        "OP_DEFINE_GLOBAL (index={}) {}",
                        index,
                        self.string_at(*index).unwrap(),
                    ),
                    Op::GetGlobal(index) => format!(
                        "OP_GET_GLOBAL (index={}) {}",
                        index,
                        self.string_at(*index).unwrap(),
                    ),
                    Op::SetGlobal(index) => format!(
                        "OP_SET_GLOBAL (index={}) {}",
                        index,
                        self.string_at(*index).unwrap(),
                    ),
                    Op::GetLocal(index) => format!("OP_GET_LOCAL (index={})", index),
                    Op::SetLocal(index) => format!("OP_SET_LOCAL (index={})", index),
                    Op::Return => "OP_RETURN".into(),
                    Op::Negate => "OP_NEAGATE".into(),
                    Op::Add => "OP_ADD".into(),
                    Op::Subtract => "OP_SUBTRACT".into(),
                    Op::Multiply => "OP_MULTIPLY".into(),
                    Op::Divide => "OP_DIVIDE".into(),
                    Op::True => "OP_TRUE".into(),
                    Op::False => "OP_FALSE".into(),
                    Op::Nil => "OP_NIL".into(),
                    Op::Not => "OP_NOT".into(),
                    Op::Equal => "OP_EQUAL".into(),
                    Op::Greater => "OP_GREATER".into(),
                    Op::Less => "OP_LESS".into(),
                    Op::Print => "OP_PRINT".into(),
                    Op::Pop => "OP_POP".into(),
                    Op::JumpIfFalse(pos) => format!("OP_JUMP_IF_FALSE (pos={})", pos),
                    Op::Jump(pos) => format!("OP_JUMP (pos={})", pos),
                    Op::Loop(pos) => format!("OP_LOOP (pos=-{})", pos),
                    Op::Call(count) => format!("OP_CALL (count={})", count),
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
            .collect();

        for (i, f) in self.fns.iter().enumerate() {
            main_body.push(format!(
                "FN_DEF (index={}): ({})",
                i,
                f.name().map_or("anonymous", |v| v)
            ));

            main_body.append(&mut f.chunk().disassemble());
        }

        main_body
    }
}
