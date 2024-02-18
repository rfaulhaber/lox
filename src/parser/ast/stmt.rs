use super::{decl::Decl, expr::Expr};

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(Vec<Decl>),
    Expr(Expr),
    Print(Expr),
}
