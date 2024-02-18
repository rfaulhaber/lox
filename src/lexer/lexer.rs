use crate::source::Span;

use super::{
    scanner::Scanner,
    token::{Token, TokenType},
};

#[derive(Debug)]
pub struct Lexer<'l> {
    scanner: Scanner<'l>,
}

impl<'l> Lexer<'l> {
    pub fn new(source: &'l str) -> Lexer<'l> {
        Lexer {
            scanner: Scanner::new(source),
        }
    }
}

impl<'l> Iterator for Lexer<'l> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.scanner.next() {
            Some((pos, s)) => {
                let token_type = get_token_type(&s);

                let len = s.len();

                Some(Token {
                    kind: token_type,
                    literal: s,
                    location: Span {
                        offset: pos,
                        length: len,
                    },
                })
            }
            None => None,
        }
    }
}

fn get_token_type<'t>(value: &'t str) -> TokenType {
    match value {
        "{" => TokenType::LeftBrace,
        "}" => TokenType::RightBrace,
        "(" => TokenType::LeftParen,
        ")" => TokenType::RightParen,
        "," => TokenType::Comma,
        "." => TokenType::Dot,
        "-" => TokenType::Minus,
        "+" => TokenType::Plus,
        ";" => TokenType::Semicolon,
        "/" => TokenType::Slash,
        "*" => TokenType::Star,
        "!" => TokenType::Bang,
        "!=" => TokenType::BangEqual,
        "=" => TokenType::Equal,
        "==" => TokenType::EqualEqual,
        "<" => TokenType::Greater,
        "<=" => TokenType::GreaterEqual,
        ">" => TokenType::Less,
        ">=" => TokenType::LessEqual,
        "and" => TokenType::And,
        "or" => TokenType::Or,
        "class" => TokenType::Class,
        "else" => TokenType::Else,
        "fun" => TokenType::Fun,
        "for" => TokenType::For,
        "if" => TokenType::If,
        "nil" => TokenType::Nil,
        "print" => TokenType::Print,
        "return" => TokenType::Return,
        "super" => TokenType::Super,
        "this" => TokenType::This,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "var" => TokenType::Var,
        "while" => TokenType::While,
        s if is_string_literal(s) => TokenType::String,
        s if is_number_literal(s) => TokenType::Number,
        _ => TokenType::Identifier,
    }
}

fn is_string_literal<'s>(value: &'s str) -> bool {
    let start_char = value.chars().nth(0);
    let last_char = value.chars().last();

    if let (Some(start), Some(last)) = (start_char, last_char) {
        start == '"' && last == '"'
    } else {
        false
    }
}

fn is_number_literal<'s>(value: &'s str) -> bool {
    let is_int = value.parse::<i64>();
    let is_float = value.parse::<f64>();

    is_int.is_ok() || is_float.is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn lexer_tokenizes_simple_code() {
        let input = r#"var language = "lox";"#;

        let expected = vec![
            Token {
                kind: TokenType::Var,
                literal: "var".into(),
                location: Span {
                    offset: 0,
                    length: 3,
                },
            },
            Token {
                kind: TokenType::Identifier,
                literal: "language".into(),
                location: Span {
                    offset: 4,
                    length: 8,
                },
            },
            Token {
                kind: TokenType::Equal,
                literal: "=".into(),
                location: Span {
                    offset: 13,
                    length: 1,
                },
            },
            Token {
                kind: TokenType::String,
                literal: "\"lox\"".into(),
                location: Span {
                    offset: 15,
                    length: 5,
                },
            },
            Token {
                kind: TokenType::Semicolon,
                literal: ";".into(),
                location: Span {
                    offset: 20,
                    length: 1,
                },
            },
        ];

        let result: Vec<Token> = Lexer::new(input).collect();

        assert_eq!(result, expected);
    }

    #[test]
    fn lexer_tokenizes_multiple_lines() {
        let input = b"var language = \"lox\";\nprint language;";

        let expected = vec![
            Token {
                kind: TokenType::Var,
                literal: "var".into(),
                location: Span {
                    offset: 0,
                    length: 3,
                },
            },
            Token {
                kind: TokenType::Identifier,
                literal: "language".into(),
                location: Span {
                    offset: 4,
                    length: 8,
                },
            },
            Token {
                kind: TokenType::Equal,
                literal: "=".into(),
                location: Span {
                    offset: 13,
                    length: 1,
                },
            },
            Token {
                kind: TokenType::String,
                literal: "\"lox\"".into(),
                location: Span {
                    offset: 15,
                    length: 5,
                },
            },
            Token {
                kind: TokenType::Semicolon,
                literal: ";".into(),
                location: Span {
                    offset: 20,
                    length: 1,
                },
            },
            Token {
                kind: TokenType::Print,
                literal: "print".into(),
                location: Span {
                    offset: 22,
                    length: 5,
                },
            },
            Token {
                kind: TokenType::Identifier,
                literal: "language".into(),
                location: Span {
                    offset: 28,
                    length: 8,
                },
            },
            Token {
                kind: TokenType::Semicolon,
                literal: ";".into(),
                location: Span {
                    offset: 36,
                    length: 1,
                },
            },
        ];

        let result: Vec<Token> = Lexer::new(std::str::from_utf8(input).unwrap()).collect();

        assert_eq!(result, expected);
    }
}
