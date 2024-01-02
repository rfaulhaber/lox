#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Unary(UnaryOperator, Box<Expr>),
    Binary(Box<Expr>, BinaryOperator, Box<Expr>),
    Grouping(Box<Expr>),
}

#[derive(Debug)]
pub enum Literal {
    Number(Number),
    String(String),
    Bool(Bool),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
}

#[derive(Debug, Clone)]
pub enum Bool {
    True,
    False,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone)]
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
