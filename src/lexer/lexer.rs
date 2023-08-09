#[derive(Debug)]
pub(super) struct Lexer<'l> {
    source: &'l str,
}

impl<'l> Iterator for Lexer<'l> {}
