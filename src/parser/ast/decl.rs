use super::{
    expr::{Expr, Identifier},
    stmt::Stmt,
};

#[derive(Debug, PartialEq)]
pub enum Decl {
    Var(Identifier, Option<Expr>),
    Stmt(Stmt),
}
