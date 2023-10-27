#[derive(Debug)]
pub enum Expr<'e> {
    Literal(Literal<'e>),
    Unary(UnaryOperator, Box<Expr<'e>>),
    Binary(Box<Expr<'e>>, BinaryOperator, Box<Expr<'e>>),
    Grouping(Box<Expr<'e>>),
}

#[derive(Debug)]
pub enum Literal<'l> {
    Number(Number),
    String(&'l str),
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
