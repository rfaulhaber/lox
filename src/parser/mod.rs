use crate::lexer::lexer::Lexer;
use thiserror::Error;

pub struct NodeId(pub usize);

pub enum NodeType {
    Float,
    Int,
    String,
    True,
    False,
    Nil,

    // unary operators
    Neg,
    Not,

    // binary operators
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Add,
    Sub,
    Mul,
    Div,

    Unary {
        op: NodeId,
        expr: NodeId,
    },

    Binary {
        left: NodeId,
        op: NodeId,
        right: NodeId,
    },

    Grouping {
        expr: NodeId,
    },
}

#[derive(Debug, Error)]
pub enum ParserError {}

pub struct Parser<'p> {
    lexer: Lexer<'p>,
    errors: Vec<ParserError>,
}
