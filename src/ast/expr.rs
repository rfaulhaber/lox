#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
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
