use super::{
    expr::{Expr, Identifier},
    stmt::Stmt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    // class name, superclass, body
    Class(Identifier, Option<Identifier>, Vec<Decl>),
    // function name, parameters, body
    Func(Identifier, Vec<Identifier>, Stmt),
    Var(Identifier, Option<Expr>),
    Stmt(Stmt),
}
