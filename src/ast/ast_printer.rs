use super::{
    expr::{BinaryOperator, Expr, Literal, Number, UnaryOperator},
    visitor::Visitor,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn print(&mut self, ast: Expr) -> String {
        self.visit_expr(ast)
    }
}

impl Visitor for AstPrinter {
    type Value = String;

    fn visit_expr(&mut self, expr: super::expr::Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_expr(*expr),
        }
    }

    fn visit_unary_expr(
        &mut self,
        op: super::expr::UnaryOperator,
        expr: super::expr::Expr,
    ) -> Self::Value {
        let op_str = match op {
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "!",
        };

        let expr = self.visit_expr(expr);

        let mut output = String::from(op_str);

        output.push_str(&expr);

        output
    }

    fn visit_binary_expr(
        &mut self,
        left: super::expr::Expr,
        op: super::expr::BinaryOperator,
        right: super::expr::Expr,
    ) -> Self::Value {
        let left_str = self.visit_expr(left);

        let op_str = match op {
            BinaryOperator::Eq => "",
            BinaryOperator::Neq => "!=",
            BinaryOperator::Lt => "<",
            BinaryOperator::Lte => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::Gte => ">=",
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
        };

        let right_str = self.visit_expr(right);

        let mut output = String::from(op_str);

        output.push(' ');
        output.push_str(&left_str);
        output.push(' ');
        output.push_str(&right_str);

        output
    }

    fn visit_literal(&mut self, literal: super::expr::Literal) -> Self::Value {
        match literal {
            Literal::Number(n) => match n {
                Number::Int(i) => i.to_string(),
                Number::Float(f) => f.to_string(),
            },
            Literal::String(s) => s,
            Literal::Bool(b) => match b {
                true => String::from("true"),
                false => String::from("false"),
            },
            Literal::Nil => String::from("nil"),
        }
    }

    fn visit_grouping_expr(&mut self, expr: super::expr::Expr) -> Self::Value {
        let mut output = String::from("group");

        let expr = self.visit_expr(expr);

        output.push_str(&expr);

        output
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn prints_simple_ast() {
        let ast = Parser::new(Lexer::new("-123 * (45.67)")).parse().unwrap();

        let expected = "(* (- 123) (group 45.67))";

        let mut printer = AstPrinter::new();

        let result = printer.print(ast);

        assert_eq!(result, expected);
    }
}
