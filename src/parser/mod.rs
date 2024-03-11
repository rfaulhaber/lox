pub mod ast;

use self::ast::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
};

use crate::{
    lexer::{
        lexer::Lexer,
        token::{Token, TokenType},
    },
    parser,
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
    #[error("Invalid assignment target")]
    InvalidAssignmnetTarget,
    #[error("Missing '}}' in block")]
    MissingBlockClose,
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

    pub fn from_source(source: &'p str) -> Self {
        Self {
            lexer: Lexer::new(source).peekable(),
        }
    }

    pub fn parse(&mut self) -> ParseResult<Program> {
        self.parse_program()
    }

    fn parse_program(&mut self) -> ParseResult<Program> {
        let declarations = {
            let mut declarations = Vec::new();

            // TODO error handling
            while self.lexer.peek().is_some() {
                declarations.push(self.parse_decl()?)
            }

            declarations
        };

        Ok(Program { declarations })
    }

    fn parse_decl(&mut self) -> ParseResult<Decl> {
        match self.lexer.peek() {
            Some(t) if t.kind == TokenType::Var => self.parse_decl_stmt(),
            Some(_) => Ok(Decl::Stmt(self.parse_stmt()?)),
            None => todo!(),
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        match self.lexer.peek() {
            Some(t) => match t.kind {
                TokenType::Print => self.parse_print_stmt(),
                TokenType::LeftBrace => self.parse_block(),
                TokenType::If => self.parse_if_stmt(),
                TokenType::While => self.parse_while_stmt(),
                TokenType::For => self.parse_for_stmt(),
                _ => self.parse_expr_stmt(),
            },
            None => todo!(),
        }
    }

    fn parse_print_stmt(&mut self) -> ParseResult<Stmt> {
        let _ = self.lexer.next();

        let expr = self.parse_expr()?;

        self.consume_single_token(TokenType::Semicolon)?;

        Ok(Stmt::Print(expr))
    }

    fn parse_block(&mut self) -> ParseResult<Stmt> {
        let _ = self.lexer.next(); // consume '{'

        let mut stmts = Vec::new();

        let mut right_brace_found = false;

        while let Some(token) = self.lexer.peek() {
            if token.kind == TokenType::RightBrace {
                right_brace_found = true;
                let _ = self.lexer.next();
                break;
            }

            stmts.push(self.parse_decl()?);
        }

        if self.lexer.peek().is_none() && !right_brace_found {
            return Err(ParseError::MissingBlockClose);
        }

        Ok(Stmt::Block(stmts))
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        let _ = self.lexer.next(); // consume 'if'

        self.consume_single_token(TokenType::LeftParen)?;

        let cond = self.parse_expr()?;

        self.consume_single_token(TokenType::RightParen)?;

        let stmt = self.parse_stmt()?;

        match self.lexer.peek() {
            Some(t) if t.kind == TokenType::Else => {
                let _ = self.lexer.next(); // consume "else"
                let else_stmt = self.parse_stmt()?;

                Ok(Stmt::If(cond, Box::new(stmt), Some(Box::new(else_stmt))))
            }
            _ => Ok(Stmt::If(cond, Box::new(stmt), None)),
        }
    }

    fn parse_while_stmt(&mut self) -> ParseResult<Stmt> {
        let _ = self.lexer.next(); // consume 'while'

        self.consume_single_token(TokenType::LeftParen)?;

        let cond = self.parse_expr()?;

        self.consume_single_token(TokenType::RightParen)?;

        let stmt = self.parse_stmt()?;

        Ok(Stmt::While(cond, Box::new(stmt)))
    }

    fn parse_for_stmt(&mut self) -> ParseResult<Stmt> {
        enum Initializer {
            Var(Decl),
            Expr(Stmt),
        }

        let _ = self.lexer.next(); // consume "for"

        self.consume_single_token(TokenType::LeftParen)?;

        let initializer = match self.lexer.peek() {
            Some(t) if t.kind == TokenType::Semicolon => None,
            Some(t) if t.kind == TokenType::Var => Some(Initializer::Var(self.parse_decl_stmt()?)),
            Some(_) => Some(Initializer::Expr(self.parse_expr_stmt()?)),
            None => return Err(ParseError::UnexpectedEndOfInput),
        };

        self.lexer.next_if(|t| t.kind == TokenType::Semicolon);

        let cond = match self.lexer.peek() {
            Some(t) if t.kind == TokenType::Semicolon => None,
            Some(_) => Some(self.parse_expr()?),
            None => return Err(ParseError::UnexpectedEndOfInput),
        };

        self.consume_single_token(TokenType::Semicolon)?;

        let inc = match self.lexer.peek() {
            Some(t) if t.kind == TokenType::RightParen => None,
            Some(_) => Some(self.parse_expr()?),
            None => return Err(ParseError::UnexpectedEndOfInput),
        };

        self.consume_single_token(TokenType::RightParen)?;

        let body = self.parse_stmt()?;

        let body_with_modified_inc = inc.map_or(body.clone(), |inc_expr| {
            Stmt::Block(vec![Decl::Stmt(body), Decl::Stmt(Stmt::Expr(inc_expr))])
        });

        let cond_or_true = cond.unwrap_or(Expr::Literal(Literal::Bool(true)));

        let normalized_initialier = initializer.map(|init| match init {
            Initializer::Var(var) => var,
            Initializer::Expr(stmt) => Decl::Stmt(stmt),
        });

        match normalized_initialier {
            Some(init) => Ok(Stmt::Block(vec![
                init,
                Decl::Stmt(Stmt::While(cond_or_true, Box::new(body_with_modified_inc))),
            ])),
            None => Ok(Stmt::While(cond_or_true, Box::new(body_with_modified_inc))),
        }
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<Stmt> {
        let expr = self.parse_expr()?;

        self.consume_single_token(TokenType::Semicolon)?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_decl_stmt(&mut self) -> ParseResult<Decl> {
        let _ = self.lexer.next(); // discard "var"

        let id = match self.lexer.next() {
            Some(t) if t.kind == TokenType::Identifier => t.literal,
            Some(t) => todo!("unexpected token: {:?}", t),
            None => todo!("unexpected end of input"),
        };

        let identifier = Identifier { name: id };

        match self.lexer.next() {
            Some(t) if t.kind == TokenType::Equal => {
                let expr = self.parse_expr()?;

                self.consume_single_token(TokenType::Semicolon)?;

                Ok(Decl::Var(identifier, Some(expr)))
            }
            Some(t) if t.kind == TokenType::Semicolon => Ok(Decl::Var(identifier, None)),
            Some(_) => {
                todo!("Unexpected token")
            }
            None => todo!(),
        }
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_or()?;

        if self
            .lexer
            .peek()
            .is_some_and(|t| t.kind == TokenType::Equal)
        {
            let _ = self.lexer.next();
            let value = self.parse_assignment()?;

            match expr {
                Expr::Var(id) => return Ok(Expr::Assignment(id, Box::new(value))),
                _ => return Err(ParseError::InvalidAssignmnetTarget),
            }
        }

        Ok(expr)
    }

    fn parse_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_and()?;

        while self.lexer.peek().is_some_and(|t| t.kind == TokenType::Or) {
            self.lexer.next();
            let right = self.parse_and()?;

            expr = Expr::Logical(Box::new(expr), LogicalOperator::Or, Box::new(right));
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality()?;

        while self.lexer.peek().is_some_and(|t| t.kind == TokenType::And) {
            self.lexer.next();
            let right = self.parse_equality()?;

            expr = Expr::Logical(Box::new(expr), LogicalOperator::And, Box::new(right));
        }

        Ok(expr)
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

        self.parse_call()
    }

    fn parse_call(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_primary()?;

        if self
            .lexer
            .peek()
            .is_some_and(|t| t.kind == TokenType::LeftParen)
        {
            let _ = self.lexer.next();

            let mut arguments = Vec::new();

            match self.lexer.peek() {
                Some(t) => match t.kind {
                    TokenType::RightParen => {
                        let _ = self.lexer.next();

                        return Ok(Expr::Call(Box::new(expr), arguments));
                    }
                    _ => {
                        arguments.push(self.parse_expr()?);

                        while self
                            .lexer
                            .peek()
                            .is_some_and(|t| t.kind == TokenType::Comma)
                        {
                            let _ = self.lexer.next();

                            arguments.push(self.parse_expr()?);
                        }

                        self.consume_single_token(TokenType::RightParen)?;

                        return Ok(Expr::Call(Box::new(expr), arguments));
                    }
                },
                None => todo!("unexpected end of input"),
            }
        }

        Ok(expr)
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
                TokenType::Identifier => {
                    let id = Identifier { name: t.literal };
                    Ok(Expr::Var(id))
                }
                _ => unreachable!(
                    "encountered unexpected token: {:?}, lex: {:?}",
                    t.kind, t.location
                ),
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
        let result = Parser::new(Lexer::new("-1 * (2 + 3);")).parse();

        let literal_1 = Expr::Literal(Literal::Number(Number::Int(1)));
        let literal_2 = Expr::Literal(Literal::Number(Number::Int(2)));
        let literal_3 = Expr::Literal(Literal::Number(Number::Int(3)));

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::Expr(Expr::Binary(
                Box::new(Expr::Unary(UnaryOperator::Neg, Box::new(literal_1))),
                BinaryOperator::Mul,
                Box::new(Expr::Grouping(Box::new(Expr::Binary(
                    Box::new(literal_2),
                    BinaryOperator::Add,
                    Box::new(literal_3),
                )))),
            )))],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_assignment() {
        let input = b"var a = 1;\nprint a = 2;";
        let result = Parser::new(Lexer::new(std::str::from_utf8(input).unwrap())).parse();

        let expected = Program {
            declarations: vec![
                Decl::Var(
                    Identifier { name: "a".into() },
                    Some(Expr::Literal(Literal::Number(Number::Int(1)))),
                ),
                Decl::Stmt(Stmt::Print(Expr::Assignment(
                    Identifier { name: "a".into() },
                    Box::new(Expr::Literal(Literal::Number(Number::Int(2)))),
                ))),
            ],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_empty_block() {
        let input = "{}";

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::Block(vec![]))],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_nested_empty_block() {
        let input = "{{}}";

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::Block(vec![Decl::Stmt(Stmt::Block(
                vec![],
            ))]))],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_blocks() {
        let input = r#"var a = 1;
{
    var b = 2;
    {
        var c = 3;
    }
}"#;

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![
                Decl::Var(
                    Identifier { name: "a".into() },
                    Some(Expr::Literal(Literal::Number(Number::Int(1)))),
                ),
                Decl::Stmt(Stmt::Block(vec![
                    Decl::Var(
                        Identifier { name: "b".into() },
                        Some(Expr::Literal(Literal::Number(Number::Int(2)))),
                    ),
                    Decl::Stmt(Stmt::Block(vec![Decl::Var(
                        Identifier { name: "c".into() },
                        Some(Expr::Literal(Literal::Number(Number::Int(3)))),
                    )])),
                ])),
            ],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_if() {
        let input = "if (first) if (second) whenTrue;";

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::If(
                Expr::Var(Identifier {
                    name: "first".into(),
                }),
                Box::new(Stmt::If(
                    Expr::Var(Identifier {
                        name: "second".into(),
                    }),
                    Box::new(Stmt::Expr(Expr::Var(Identifier {
                        name: "whenTrue".into(),
                    }))),
                    None,
                )),
                None,
            ))],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_if_else() {
        let input = "if (first) whenTrue; else whenFalse;";

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::If(
                Expr::Var(Identifier {
                    name: "first".into(),
                }),
                Box::new(Stmt::Expr(Expr::Var(Identifier {
                    name: "whenTrue".into(),
                }))),
                Some(Box::new(Stmt::Expr(Expr::Var(Identifier {
                    name: "whenFalse".into(),
                })))),
            ))],
        };

        assert_eq!(result.unwrap(), expected);
    }

    #[test]
    fn parse_function_call() {
        let input = "average(1, 2);";

        let result = Parser::new(Lexer::new(input)).parse();

        let expected = Program {
            declarations: vec![Decl::Stmt(Stmt::Expr(Expr::Call(
                Box::new(Expr::Var(Identifier {
                    name: String::from("average"),
                })),
                vec![
                    Expr::Literal(Literal::Number(Number::Int(1))),
                    Expr::Literal(Literal::Number(Number::Int(2))),
                ],
            )))],
        };

        assert_eq!(result.unwrap(), expected);
    }
}
