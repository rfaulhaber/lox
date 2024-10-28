use super::decl::Decl;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decl>,
}
