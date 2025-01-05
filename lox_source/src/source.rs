use miette::SourceSpan;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        let (offset, length) = value;
        Self { offset, length }
    }
}

impl Into<SourceSpan> for Span {
    fn into(self) -> SourceSpan {
        (self.offset, self.length).into()
    }
}

impl Span {
    pub fn new(offset: usize, length: usize) -> Self {
        Self { offset, length }
    }
}
