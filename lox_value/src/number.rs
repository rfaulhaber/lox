#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Number {
    Int(i64),
    Float(f64),
}

impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Number::Int(i) => i.to_string(),
                Number::Float(f) => f.to_string(),
            }
        )
    }
}

impl std::ops::Add for Number {
    type Output = Number;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l.add(r)),
            (Number::Int(l), Number::Float(r)) => Number::Float((l as f64).add(r)),
            (Number::Float(l), Number::Int(r)) => Number::Float(l.add(r as f64)),
            (Number::Float(l), Number::Float(r)) => Number::Float(l.add(r)),
        }
    }
}

impl std::ops::Sub for Number {
    type Output = Number;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l.sub(r)),
            (Number::Int(l), Number::Float(r)) => Number::Float((l as f64).sub(r)),
            (Number::Float(l), Number::Int(r)) => Number::Float(l.sub(r as f64)),
            (Number::Float(l), Number::Float(r)) => Number::Float(l.sub(r)),
        }
    }
}

impl std::ops::Mul for Number {
    type Output = Number;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l.mul(r)),
            (Number::Int(l), Number::Float(r)) => Number::Float((l as f64).mul(r)),
            (Number::Float(l), Number::Int(r)) => Number::Float(l.mul(r as f64)),
            (Number::Float(l), Number::Float(r)) => Number::Float(l.mul(r)),
        }
    }
}

impl std::ops::Div for Number {
    type Output = Number;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Number::Int(l), Number::Int(r)) => Number::Int(l.div(r)),
            (Number::Int(l), Number::Float(r)) => Number::Float((l as f64).div(r)),
            (Number::Float(l), Number::Int(r)) => Number::Float(l.div(r as f64)),
            (Number::Float(l), Number::Float(r)) => Number::Float(l.div(r)),
        }
    }
}

impl std::ops::Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Int(i) => Number::Int(i.neg()),
            Number::Float(f) => Number::Float(f.neg()),
        }
    }
}
