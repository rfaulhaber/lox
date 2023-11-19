pub mod expr;
pub mod visitor;

pub struct NodeId(pub usize);

pub struct SyntaxTree {
    nodes: Vec<Node>,
}
