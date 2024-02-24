use super::{
    expr::{Expr, Identifier},
    stmt::Stmt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Var(Identifier, Option<Expr>),
    Stmt(Stmt),
}
