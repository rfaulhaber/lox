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

    #[error("unexpected token {0}, expected: {1}")]
    #[diagnostic(code(lox::expected_token))]
    ExpectedToken(String, String),

    #[error("unexpected end of input")]
    #[diagnostic(code(lox::unexpected_eof))]
    UnexpectedEndOfInput,
}

type InnerCompilerResult = Result<(), CompilerError>;

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
        println!("grouping: {:?}", self.lexer);
        let _ = self.lexer.next();

        self.expression()?;

        let _ = self.lexer.next();

        Ok(())
    }

    fn unary(&mut self) -> InnerCompilerResult {
        println!("unary: {:?}", self.lexer);
        let token = self.lexer.next().unwrap();

        self.parse_precedence(8)?;

        match token.kind {
            TokenType::Minus => self.context.write_code(Op::Negate, token.location),
            _ => unreachable!(),
        };

        Ok(())
    }

    fn binary(&mut self) -> InnerCompilerResult {
        println!("binary: {:?}", self.lexer);
        let token = self.lexer.next().unwrap();

        println!("binary token: {:?}", token);

        self.parse_precedence(4)?;

        match token.kind {
            TokenType::Plus => self.context.write_code(Op::Add, token.location),
            TokenType::Minus => self.context.write_code(Op::Subtract, token.location),
            TokenType::Star => self.context.write_code(Op::Multiply, token.location),
            TokenType::Slash => self.context.write_code(Op::Divide, token.location),
            _ => {
                return Err(CompilerError::ExpectedToken(
                    token.literal,
                    "binary operator".into(),
                ))
            }
        };

        Ok(())
    }

    fn expression(&mut self) -> InnerCompilerResult {
        println!("expression: {:?}", self.lexer);
        self.parse_precedence(1)?;

        Ok(())
    }

    fn parse_precedence(&mut self, precedence: u8) -> InnerCompilerResult {
        let lhs = self.lexer.peek();

        if lhs.is_none() {
            return Err(CompilerError::UnexpectedEndOfInput);
        }

        let lhs = lhs.unwrap();
        let kind = lhs.kind;

        if kind == TokenType::LeftParen {
            self.grouping()?;
        }

        if kind == TokenType::Number {
            self.number()?;
        }

        if let Some(((), rhp)) = prefix_precedence(kind) {
            self.unary()?;
        }

        loop {
            let token = self.lexer.peek();

            if token.is_none() {
                return Err(CompilerError::UnexpectedEndOfInput);
            }

            let token = token.unwrap();

            if let Some((lhp, rhp)) = infix_precedence(token.kind) {
                if lhp < precedence {
                    break;
                }

                self.binary()?;
                continue;
            }

            break;
        }

        Ok(())
    }

    fn number(&mut self) -> InnerCompilerResult {
        println!("number: {:?}", self.lexer);
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

fn infix_precedence(token_type: TokenType) -> Option<(u8, u8)> {
    match token_type {
        TokenType::Equal => Some((1, 2)),
        TokenType::Or => Some((2, 3)),
        TokenType::And => Some((3, 4)),
        TokenType::EqualEqual | TokenType::BangEqual => Some((4, 5)),
        TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual => {
            Some((5, 6))
        }
        TokenType::Plus | TokenType::Minus => Some((6, 7)),
        TokenType::Star | TokenType::Slash => Some((7, 8)),
        TokenType::Dot | TokenType::LeftParen => Some((9, 10)),
        _ => None,
    }
}

fn prefix_precedence(token_type: TokenType) -> Option<((), u8)> {
    match token_type {
        TokenType::Bang | TokenType::Minus => Some(((), 11)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn precedence_parsing() {
        let source = "(-1 + 2) * 3 - -4";

        let compiler = Compiler::new(&source);

        let res = compiler.compile().unwrap();

        assert_eq!(res.disassemble(), vec!["foo".to_string()]);
    }
}
