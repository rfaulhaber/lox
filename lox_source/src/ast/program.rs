use super::decl::Decl;

// TODO implement memory arena

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Decl>,
}
