use super::stmt::Stmt;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}
