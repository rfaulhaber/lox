use opcode::{Chunk, OpCode, Value};
use thiserror::Error;

mod opcode;

pub type EvalResult = Result<(), EvalError>;

#[derive(Debug, PartialEq, Error)]
pub enum EvalError {}

#[derive(Debug)]
pub struct VM {
    chunk: Chunk,
    ip: usize,
}

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
        }
    }

    pub fn eval_chunk(&mut self, chunk: Chunk) -> EvalResult {
        self.chunk = chunk;
        self.ip = 0;

        self.run()
    }

    fn run(&mut self) -> EvalResult {
        loop {
            match self.read_opcode() {
                Some(&OpCode::Constant) => todo!(),
                Some(&OpCode::Return) => todo!(),
                None => todo!(),
            }
        }
    }

    fn read_opcode(&mut self) -> Option<&OpCode> {
        let code = self.chunk.code_at(self.ip);

        self.ip = self.ip + 1;

        code
    }

    fn read_constant(&mut self) -> Option<&Value> {
        self.ip = self.ip + 1;
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
