use crate::value::Value;
use crate::vm::bytecode::{Context, Op};
use lox_source::lexer::token::Token;
use lox_source::lexer::{token::TokenType, Lexer};
use lox_source::miette;
use lox_source::miette::Diagnostic;
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
pub enum CompilerError {
    #[error("invalid floating point value")]
    #[diagnostic(code(lox::invalid_float))]
    InvalidFloat(#[from] std::num::ParseFloatError),

    #[error("unexpected token {0}, expected {1}")]
    #[diagnostic(code(lox::expected_token))]
    ExpectedToken(String, String),
}

type InnerCompilerResult = Result<(), CompilerError>;

#[derive(Debug, PartialEq, Eq, PartialOrd)]
enum Precedence {
    None = 0,
    Assignment = 1,
    Or = 2,
    And = 3,
    Equality = 4,
    Comparison = 5,
    Term = 6,
    Factor = 7,
    Unary = 8,
    Call = 9,
    Primary = 10,
}

impl Precedence {
    fn next(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Self::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None,
        }
    }
}

pub struct Compiler<'c> {
    lexer: Peekable<Lexer<'c>>,
    context: Context,
}

impl<'c> Compiler<'c> {
    pub fn new(source: &'c str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
            context: Context::new(),
        }
    }

    pub fn compile(mut self) -> Result<Context, CompilerError> {
        while self.lexer.peek().is_some() {
            self.emit_bytecode()?;
        }

        Ok(self.context)
    }

    fn emit_bytecode(&mut self) -> InnerCompilerResult {
        match self.lexer.peek() {
            Some(token) => match token.kind {
                TokenType::Return => self.ret(),
                TokenType::Number => self.number(),
                TokenType::LeftParen => self.grouping(),
                _ => todo!(),
            },
            None => todo!(),
        }
    }

    fn grouping(&mut self) -> InnerCompilerResult {
        let _ = self.lexer.next();

        self.expression()?;

        let _ = self.lexer.next();

        Ok(())
    }

    fn unary(&mut self) -> InnerCompilerResult {
        let token = self.lexer.next().unwrap();

        self.parse_precedence(Precedence::Unary)?;

        match token.kind {
            TokenType::Minus => self.context.write_code(Op::Negate, token.location),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn binary(&mut self) -> InnerCompilerResult {
        let token = self.lexer.next().unwrap();

        todo!();

        let next_precedence = match token.kind {
            TokenType::Plus =>
        }
    }

    fn expression(&mut self) -> InnerCompilerResult {
        self.parse_precedence(Precedence::Assignment)?;

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> InnerCompilerResult {
        let lhs = self.lexer.next();

        match lhs {
            Some(token) => match token.kind {
                _ => todo!(),
            }
            None => todo!()
        }
    }

    fn number(&mut self) -> InnerCompilerResult {
        let Token {
            literal, location, ..
        } = self.lexer.next().unwrap();

        let number: Value = literal.parse::<f64>().map(|f| f.into())?;

        let index = self.context.add_const(number);

        self.context.write_code(Op::Constant(index), location);

        Ok(())
    }

    fn ret(&mut self) -> InnerCompilerResult {
        let token = self.lexer.next().unwrap();
        self.context.write_code(Op::Return, token.location);

        Ok(())
    }
}
