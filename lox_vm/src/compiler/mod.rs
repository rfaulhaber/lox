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

    #[error("unexpected end of input")]
    #[diagnostic(code(lox::unexpected_eof))]
    UnexpectedEndOfInput,
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

        todo!();
        self.parse_precedence(1)?;

        match token.kind {
            TokenType::Minus => self.context.write_code(Op::Negate, token.location),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn binary(&mut self) -> InnerCompilerResult {
        let token = self.lexer.next().unwrap();

        todo!();
    }

    fn expression(&mut self) -> InnerCompilerResult {
        todo!();

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: u8) -> InnerCompilerResult {
        let lhs = self.lexer.next();

        loop {
            let op = match self.lexer.next() {
                Some(token) => todo!(),
                None => return Err(CompilerError::UnexpectedEndOfInput),
            };
        }

        Ok(())
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

fn infix_binding_power(token_type: TokenType) -> (u8, u8) {
    match token_type {
        TokenType::LeftParen => todo!(),
        TokenType::RightParen => todo!(),
        TokenType::LeftBrace => todo!(),
        TokenType::RightBrace => todo!(),
        TokenType::Comma => todo!(),
        TokenType::Dot => todo!(),
        TokenType::Minus => todo!(),
        TokenType::Plus => todo!(),
        TokenType::Semicolon => todo!(),
        TokenType::Slash => todo!(),
        TokenType::Star => todo!(),
        TokenType::Bang => todo!(),
        TokenType::BangEqual => todo!(),
        TokenType::Equal => todo!(),
        TokenType::EqualEqual => todo!(),
        TokenType::Greater => todo!(),
        TokenType::GreaterEqual => todo!(),
        TokenType::Less => todo!(),
        TokenType::LessEqual => todo!(),
        TokenType::Identifier => todo!(),
        TokenType::String => todo!(),
        TokenType::Number => todo!(),
        TokenType::And => todo!(),
        TokenType::Class => todo!(),
        TokenType::Else => todo!(),
        TokenType::False => todo!(),
        TokenType::Fun => todo!(),
        TokenType::For => todo!(),
        TokenType::If => todo!(),
        TokenType::Nil => todo!(),
        TokenType::Or => todo!(),
        TokenType::Print => todo!(),
        TokenType::Return => todo!(),
        TokenType::Super => todo!(),
        TokenType::This => todo!(),
        TokenType::True => todo!(),
        TokenType::Var => todo!(),
        TokenType::While => todo!(),
    }
}
