use super::expr::{BinaryOperator, Expr, Literal, UnaryOperator};

pub trait Visitor: Sized {
    fn visit_expr(&mut self, expr: Expr) {
        walk_expr(self, expr);
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) {
        walk_unary_expr(self, op, expr);
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) {
        walk_binary_expr(self, left, op, right);
    }

    fn visit_literal(&mut self, literal: Literal);

    fn visit_grouping_expr(&mut self, expr: Expr) {
        walk_expr(self, expr);
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: Expr) {
    match expr {
        Expr::Literal(l) => visitor.visit_literal(l),
        Expr::Unary(op, expr) => visitor.visit_unary_expr(op.clone(), *expr),
        Expr::Binary(left, op, right) => visitor.visit_binary_expr(*left, op.clone(), *right),
        Expr::Grouping(expr) => visitor.visit_grouping_expr(*expr),
    }
}

pub fn walk_literal<V: Visitor>(visitor: &mut V, literal: Literal) {
    match literal {
        Literal::Number(_) => todo!(),
        Literal::String(_) => todo!(),
        Literal::Bool(_) => todo!(),
        Literal::Nil => todo!(),
    }
}

pub fn walk_unary_expr<V: Visitor>(visitor: &mut V, op: UnaryOperator, expr: Expr) {
    visitor.visit_expr(expr);
}

pub fn walk_binary_expr<V: Visitor>(
    visitor: &mut V,
    left_expr: Expr,
    op: BinaryOperator,
    right_expr: Expr,
) {
    visitor.visit_expr(left_expr);
    visitor.visit_expr(right_expr);
}

// sanity check to ensure visitor is implemented correctly

#[cfg(test)]
mod tests {
    use crate::ast::expr::Number;

    use super::*;

    struct TestVisitor {
        pub out: Vec<String>,
    }

    impl Visitor for TestVisitor {
        fn visit_expr(&mut self, expr: Expr) {
            walk_expr(self, expr);
        }

        fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) {
            self.out.push("(*".into());
            walk_binary_expr(self, left, op, right);
            self.out.push(")".into());
        }

        fn visit_literal(&mut self, literal: Literal) {
            match literal {
                Literal::Number(n) => match n {
                    Number::Float(f) => self.out.push(f.to_string()),
                    Number::Int(i) => self.out.push(i.to_string()),
                },
                _ => unimplemented!(),
            }
        }

        fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) {
            self.out.push("(- ".into());
            walk_unary_expr(self, op, expr);
            self.out.push(")".into());
        }

        fn visit_grouping_expr(&mut self, expr: Expr) {
            self.out.push("(group ".into());
            walk_expr(self, expr);
            self.out.push(")".into());
        }
    }

    #[test]
    fn visitor_visits_entire_tree() {
        let literal_int = Literal::Number(Number::Int(123));
        let literal_float = Literal::Number(Number::Float(45.67));

        let literal_float_expr = Expr::Literal(literal_float);
        let literal_int_expr = Expr::Literal(literal_int);

        let group_expr = Expr::Grouping(Box::new(literal_float_expr));
        let neg_expr = Expr::Unary(UnaryOperator::Neg, Box::new(literal_int_expr));

        let root = Expr::Binary(
            Box::new(neg_expr),
            BinaryOperator::Mul,
            Box::new(group_expr),
        );

        let mut v = TestVisitor { out: Vec::new() };

        v.visit_expr(root);

        assert_eq!(v.out.join(""), "(*(- 123)(group 45.67))");
    }
}
