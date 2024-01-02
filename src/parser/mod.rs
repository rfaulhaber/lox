use crate::{
    ast::expr::{BinaryOperator, Expr},
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
};
use thiserror::Error;

type ParseResult = Result<Expr, ParserError>;

#[derive(Debug, Error)]
pub enum ParserError {}

pub struct Parser<'p> {
    lexer: Lexer<'p>,
}

impl<'p> Parser<'p> {
    pub fn parse(&mut self) -> ParseResult {
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> ParseResult {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> ParseResult {
        let mut left = self.parse_comparison()?;

        while let Some(token) = self.lexer.next() {
            if matches!(token.kind, TokenType::BangEqual | TokenType::EqualEqual) {
                let op = if token.kind == TokenType::BangEqual {
                    BinaryOperator::Neq
                } else {
                    BinaryOperator::Eq
                };

                let right = self.parse_comparison()?;

                left = Expr::Binary(Box::new(left), op, Box::new(right));
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> ParseResult {
        todo!();
    }
}
