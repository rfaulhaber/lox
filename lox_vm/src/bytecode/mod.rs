use crate::value::{Function, Object, Value};
use lox_source::source::Span;

#[derive(Debug, Clone)]
pub enum Op {
    Const(usize),
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
    GetUpvalue(usize),
    SetUpvalue(usize),
    JumpIfFalse(usize),
    Jump(usize),
    Loop(usize),
    Call(usize),
    Closure(usize),
}

#[derive(Debug, Clone)]
pub struct Chunk {
    code: Vec<Op>,
    consts: Vec<Value>,
    locations: Vec<Span>,
}

impl Default for Chunk {
    fn default() -> Self {
        Chunk::new()
    }
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            consts: Vec::new(),
            code: Vec::new(),
            locations: Vec::new(),
        }
    }

    pub fn add_sourced_op(&mut self, code: Op, location: Span) {
        self.code.push(code);
        self.locations.push(location);
    }

    pub fn add_op(&mut self, code: Op) {
        self.code.push(code);
    }

    pub fn add_const<T: Into<Value>>(&mut self, value: T) -> usize {
        let idx = self.consts.len();
        self.consts.push(value.into());

        idx
    }

    pub fn push_const<T: Into<Value>>(&mut self, value: T) -> usize {
        let idx = self.add_const(value);
        self.add_op(Op::Const(idx));

        idx
    }

    pub fn code_at(&self, index: usize) -> Option<&Op> {
        self.code.get(index)
    }

    pub fn const_at(&self, index: usize) -> Option<&Value> {
        self.consts.get(index)
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
                let source = self
                    .locations
                    .iter()
                    .enumerate()
                    .find(|(location, _)| *location == idx);
                let formatted_op = match op {
                    Op::Const(index) => format!(
                        "OP_CONST (index={}) {}",
                        index,
                        self.const_at(*index).unwrap()),
                    Op::DefineGlobal(index) => format!(
                        "OP_DEFINE_GLOBAL (index={}) {}",
                        index,
                        self.const_at(*index).unwrap(),
                    ),
                    Op::GetGlobal(index) => format!(
                        "OP_GET_GLOBAL (index={}) {}",
                        index,
                        self.const_at(*index).unwrap(),
                    ),
                    Op::SetGlobal(index) => format!(
                        "OP_SET_GLOBAL (index={}) {}",
                        index,
                        self.const_at(*index).unwrap(),
                    ),
                    Op::GetLocal(index) => format!("OP_GET_LOCAL (index={})", index),
                    Op::SetLocal(index) => format!("OP_SET_LOCAL (index={})", index),
                    Op::GetUpvalue(index) => format!("OP_GET_UPVALUE (index={})", index),
                    Op::SetUpvalue(index) => format!("OP_SET_UPVALUE (index={})", index),
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
                    Op::Closure(index) => format!("OP_CLOSURE (index={})", index),
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

        for (i, f) in self.consts.iter().enumerate() {
            match f {
                Value::Object(Object::Function(func)) => {
                    main_body.push(format!(
                        "FN_DEF (index={}): ({})",
                        i,
                        func.name().map_or("anonymous", |v| v)
                    ));

                    main_body.append(&mut func.chunk().disassemble());
                }
                _ => {
                    continue;
                }
            }
        }

        main_body
    }
}
