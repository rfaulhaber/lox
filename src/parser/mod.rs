pub mod ast;

use self::ast::{
    expr::{BinaryOperator, Expr, Literal, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
};

use crate::lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};
use std::iter::Peekable;
use thiserror::Error;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Could not parse int")]
    CouldNotParseInt(#[from] std::num::ParseIntError),
    #[error("Could not parse float")]
    CouldNotParseFloat(#[from] std::num::ParseFloatError),
    #[error("Unexpected token. Expected {0} but got {1}")]
    UnexpectedToken(TokenType, TokenType),
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,
}

#[derive(Debug)]
pub struct Parser<'p> {
    lexer: Peekable<Lexer<'p>>,
}

impl<'p> Parser<'p> {
    pub fn new(lexer: Lexer<'p>) -> Self {
        Self {
            lexer: lexer.peekable(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ParseResult<Program> {
        let stmts = {
            let mut stmts = Vec::new();

            while self.lexer.peek().is_some() {
                stmts.push(self.parse_stmt()?)
            }

            stmts
        };

        Ok(Program { stmts })
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.lexer.peek() {
            Some(t) if t.kind == TokenType::Print => self.parse_print_stmt(),
            Some(_) => self.parse_expr_stmt(),
            None => todo!(),
        }
    }

    fn parse_print_stmt(&mut self) -> ParseResult<Stmt> {
        let _ = self.lexer.next();

        let expr = self.parse_expr()?;

        self.consume_single_token(TokenType::Semicolon)?;

        Ok(Stmt::Print(expr))
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;

        self.consume_single_token(TokenType::Semicolon)?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_comparison()?;

        while self
            .lexer
            .peek()
            .is_some_and(|t| matches!(t.kind, TokenType::BangEqual | TokenType::EqualEqual))
        {
            let op_token = self.lexer.next().unwrap();

            let op = match op_token.kind {
                TokenType::BangEqual => BinaryOperator::Neq,
                TokenType::EqualEqual => BinaryOperator::Eq,
                _ => unreachable!(),
            };

            let right = self.parse_comparison()?;

            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_term()?;

        while self.lexer.peek().is_some_and(|t| {
            matches!(
                t.kind,
                TokenType::Greater
                    | TokenType::GreaterEqual
                    | TokenType::Less
                    | TokenType::LessEqual
            )
        }) {
            let op_token = self.lexer.next().unwrap();

            let op = match op_token.kind {
                TokenType::GreaterEqual => BinaryOperator::Gte,
                TokenType::Greater => BinaryOperator::Gt,
                TokenType::Less => BinaryOperator::Lt,
                TokenType::LessEqual => BinaryOperator::Lte,
                _ => unreachable!(),
            };

            let right = self.parse_term()?;

            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_factor()?;

        while self
            .lexer
            .peek()
            .is_some_and(|t| matches!(t.kind, TokenType::Plus | TokenType::Minus))
        {
            let op_token = self.lexer.next().unwrap();

            let op = match op_token.kind {
                TokenType::Plus => BinaryOperator::Add,
                TokenType::Minus => BinaryOperator::Sub,
                _ => unreachable!(),
            };

            let right = self.parse_factor()?;

            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_unary()?;

        while self
            .lexer
            .peek()
            .is_some_and(|t| matches!(t.kind, TokenType::Slash | TokenType::Star))
        {
            let op_token = self.lexer.next().unwrap();

            let op = match op_token.kind {
                TokenType::Slash => BinaryOperator::Div,
                TokenType::Star => BinaryOperator::Mul,
                _ => unreachable!(),
            };

            let right = self.parse_unary()?;

            left = Expr::Binary(Box::new(left), op, Box::new(right));
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        if self
            .lexer
            .peek()
            .is_some_and(|t| matches!(t.kind, TokenType::Bang | TokenType::Minus))
        {
            let op_token = self.lexer.next().unwrap();

            let op = match op_token.kind {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Neg,
                _ => unreachable!(),
            };

            let right = self.parse_unary()?;

            return Ok(Expr::Unary(op, Box::new(right)));
        }

        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        match self.lexer.next() {
            Some(t) => match t.kind {
                TokenType::False => Ok(Expr::Literal(Literal::Bool(false))),
                TokenType::True => Ok(Expr::Literal(Literal::Bool(true))),
                TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
                TokenType::Number => try_parse_number(t),
                TokenType::String => Ok(Expr::Literal(Literal::String(t.literal))),
                TokenType::LeftParen => {
                    let expr = self.parse_expr()?;

                    self.consume_single_token(TokenType::RightParen)?;

                    Ok(Expr::Grouping(Box::new(expr)))
                }
                _ => unreachable!(),
            },
            None => todo!("throw error"),
        }
    }

    fn consume_single_token(&mut self, token_type: TokenType) -> Result<(), ParseError> {
        let next = self.lexer.next();

        match next {
            Some(t) if t.kind == token_type => Ok(()),
            Some(t) => Err(ParseError::UnexpectedToken(token_type, t.kind)),
            None => Err(ParseError::UnexpectedEndOfInput),
        }
    }
}

fn try_parse_number(t: Token) -> ParseResult<Expr> {
    if t.literal.contains(".") {
        t.literal
            .parse::<f64>()
            .map(|n| Expr::Literal(Literal::Number(Number::Float(n))))
            .map_err(|e| ParseError::CouldNotParseFloat(e))
    } else {
        t.literal
            .parse::<i64>()
            .map(|n| Expr::Literal(Literal::Number(Number::Int(n))))
            .map_err(|e| ParseError::CouldNotParseInt(e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primary_expr() {
        let mut result = Parser::new(Lexer::new("-1 * (2 + 3)")).parse();

        let literal_1 = Expr::Literal(Literal::Number(Number::Int(1)));
        let literal_2 = Expr::Literal(Literal::Number(Number::Int(2)));
        let literal_3 = Expr::Literal(Literal::Number(Number::Int(3)));

        let expected = Program {
            stmts: vec![Stmt::Expr(Expr::Binary(
                Box::new(Expr::Unary(UnaryOperator::Neg, Box::new(literal_1))),
                BinaryOperator::Mul,
                Box::new(Expr::Grouping(Box::new(Expr::Binary(
                    Box::new(literal_2),
                    BinaryOperator::Add,
                    Box::new(literal_3),
                )))),
            ))],
        };

        assert_eq!(result.unwrap(), expected);
    }
}
