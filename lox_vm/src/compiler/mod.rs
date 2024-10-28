use std::iter::Peekable;

use crate::vm::bytecode::{Context, Op};
use lox_source::lexer::{token::TokenType, Lexer};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilerError {}

pub type CompilerResult = Result<Context, CompilerError>;

pub struct Compiler<'c> {
    lexer: Peekable<Lexer<'c>>,
}

impl<'c> Compiler<'c> {
    pub fn new(source: &'c str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
        }
    }

    pub fn compile(&mut self) -> CompilerResult {
        let mut chunk = Context::new();

        todo!();

        // while self.lexer.peek().is_some() {
        //     let mut result = self.emit()?;
        //     chunk.merge(&mut result);
        // }

        Ok(chunk)
    }
}
