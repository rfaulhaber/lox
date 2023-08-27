use super::{scanner::Scanner, token::Token};

#[derive(Debug)]
pub(crate) struct Lexer<'l> {
    source: &'l str,
    scanner: Scanner<'l>,
}

impl<'l> Iterator for Lexer<'l> {
    type Item = Token<'l>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!();
    }
}
