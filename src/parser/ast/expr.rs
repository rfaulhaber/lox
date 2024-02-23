#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Logical(Box<Expr>, LogicalOperator, Box<Expr>),
    Grouping(Box<Expr>),
    Var(Identifier),
    Assignment(Identifier, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Number(Number),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Bool {
    True,
    False,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
}
