use super::expr::{BinaryOperator, Expr, Literal, UnaryOperator};

pub(crate) trait Visitor<'ast>: Sized {
    fn visit_expr(&mut self, expr: &'ast Expr<'ast>) {
        walk_expr(self, expr);
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: &'ast Expr<'ast>) {
        walk_unary_expr(self, op, expr);
    }

    fn visit_binary_expr(
        &mut self,
        left: &'ast Expr<'ast>,
        op: BinaryOperator,
        right: &'ast Expr<'ast>,
    ) {
        walk_binary_expr(self, left, op, right);
    }

    fn visit_literal(&mut self, literal: &'ast Literal<'ast>);
}

fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, expr: &'ast Expr<'ast>) {
    match expr {
        Expr::Literal(l) => visitor.visit_literal(l),
        Expr::Unary(op, expr) => visitor.visit_unary_expr(op.clone(), expr),
        Expr::Binary(left, op, right) => visitor.visit_binary_expr(left, op.clone(), right),
        Expr::Grouping(expr) => visitor.visit_expr(expr),
    }
}

fn walk_literal<'ast, V: Visitor<'ast>>(visitor: &mut V, literal: &'ast Literal<'ast>) {
    match literal {
        Literal::Number(_) => todo!(),
        Literal::String(_) => todo!(),
        Literal::Bool(_) => todo!(),
        Literal::Nil => todo!(),
    }
}

fn walk_unary_expr<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    op: UnaryOperator,
    expr: &'ast Expr<'ast>,
) {
    visitor.visit_expr(expr);
}

fn walk_binary_expr<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    left_expr: &'ast Expr<'ast>,
    op: BinaryOperator,
    right_expr: &'ast Expr<'ast>,
) {
    visitor.visit_expr(left_expr);
    visitor.visit_expr(right_expr);
}

#[cfg(test)]
mod tests {
    use crate::ast::expr::Number;

    use super::*;

    struct TestVisitor {
        pub out: Vec<String>,
    }

    impl<'v> Visitor<'v> for TestVisitor {
        fn visit_expr(&mut self, expr: &'v Expr<'v>) {
            self.out.push("(".into());
            walk_expr(self, expr);
            self.out.push(")".into());
        }

        fn visit_binary_expr(
            &mut self,
            left: &'v Expr<'v>,
            op: BinaryOperator,
            right: &'v Expr<'v>,
        ) {
            self.out.push("*".into());
            walk_binary_expr(self, left, op, right);
        }

        fn visit_literal(&mut self, literal: &'v Literal<'v>) {
            match literal {
                Literal::Number(n) => match n {
                    Number::Float(f) => self.out.push(f.to_string()),
                    Number::Int(i) => self.out.push(i.to_string()),
                },
                _ => unimplemented!(),
            }
        }

        fn visit_unary_expr(&mut self, op: UnaryOperator, expr: &'v Expr<'v>) {
            self.out.push("-".into());
            walk_unary_expr(self, op, expr);
        }
    }

    #[test]
    fn visitor_visits_entire_tree() {
        let arena = bumpalo::Bump::new();

        let literal_int = Literal::Number(Number::Int(123));
        let literal_float = Literal::Number(Number::Float(45.67));

        let literal_float_expr = arena.alloc(Expr::Literal(literal_float));
        let literal_int_expr = arena.alloc(Expr::Literal(literal_int));

        let group_expr = arena.alloc(Expr::Grouping(literal_float_expr));
        let neg_expr = arena.alloc(Expr::Unary(UnaryOperator::Neg, literal_int_expr));

        let root = arena.alloc(Expr::Binary(neg_expr, BinaryOperator::Mul, group_expr));

        let mut v = TestVisitor { out: Vec::new() };

        v.visit_expr(root);

        assert_eq!(v.out.join(" "), "( * ( - 123 ) ( 45.67 ) )");
    }
}
