use crate::{
    ast::expr::{BinaryOperator, Expr},
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    arena: bumpalo::Bump,
}

pub type ParseResult<'r> = Result<Expr<'r>, ParseError>;

#[derive(Debug)]
pub struct ParseError {}

impl<'l> From<Lexer<'l>> for Parser {
    fn from(value: Lexer) -> Self {
        Self {
            tokens: value.into_iter().collect(),
            current: 0,
            arena: bumpalo::Bump::new(),
        }
    }
}

impl<'r> Parser {
    pub fn parse(&mut self) -> ParseResult<'r> {
        let expr = self.expression()?;
        Ok(expr)
    }

    fn expression(&mut self) -> ParseResult<'r> {
        let expr = self.equality()?;
        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult<'r> {
        let expr = self.comparison()?;

        let op = match self.advance().unwrap().kind {
            TokenType::BangEqual => BinaryOperator::Neq,
            TokenType::EqualEqual => BinaryOperator::Eq,
            _ => unreachable!(),
        };

        let right = self.comparison()?;

        let res = Expr::Binary(Box::new(expr), op, Box::new(right));

        Ok(res)
    }

    fn comparison(&mut self) -> ParseResult<'r> {
        todo!();
    }

    // utility functions

    fn advance(&mut self) -> Option<Token> {
        self.current += 1;
        self.tokens.get(self.current).cloned()
    }

    fn current(&mut self) -> Option<Token> {
        self.tokens.get(self.current).cloned()
    }
}
