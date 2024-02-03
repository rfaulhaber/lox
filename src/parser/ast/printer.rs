use std::io::{BufWriter, Write};

use super::{
    expr::{BinaryOperator, Expr, Literal, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
    visitor::{ExprVisitor, StmtVisitor},
};

pub struct AstPrinter<W: std::io::Write> {
    writer: W,
}

impl<W: std::io::Write> AstPrinter<W> {
    pub fn new(writer: W) -> AstPrinter<W> {
        Self { writer }
    }

    pub fn get_writer(&mut self) -> &W {
        &self.writer
    }

    pub fn print(&mut self, ast: Program) {
        self.visit_program(ast);
    }
}

impl<W: std::io::Write> StmtVisitor for AstPrinter<W> {
    fn visit_program(&mut self, program: Program) {
        program
            .stmts
            .into_iter()
            .for_each(|stmt| self.visit_stmt(stmt))
    }

    fn visit_stmt(&mut self, stmt: Stmt) {
        let stmt = match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Print(expr) => self.visit_expr(expr),
        };

        let res = write!(self.writer, "{}", stmt);

        if res.is_err() {
            panic!("could not parse: {:?}", res);
        }
    }
}

impl<W: std::io::Write> ExprVisitor for AstPrinter<W> {
    type Value = String;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
        }
    }

    fn visit_unary_expr(&mut self, op: super::expr::UnaryOperator, expr: Expr) -> Self::Value {
        let op_str = match op {
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "!",
        };

        let expr = self.visit_expr(expr);

        format!("({} {})", op_str, &expr)
    }

    fn visit_binary_expr(
        &mut self,
        left: Expr,
        op: super::expr::BinaryOperator,
        right: Expr,
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

        format!("({} {} {})", op_str, left_str, right_str)
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

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        let expr = self.visit_expr(expr);

        format!("(group {})", expr)
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn prints_simple_ast() {
        let ast = Parser::new(Lexer::new("-123 * (45.67);")).parse().unwrap();

        let expected = String::from("(* (- 123) (group 45.67))");

        let mut printer = AstPrinter::new(Vec::new());

        printer.print(ast);

        let result = String::from_utf8(printer.get_writer().clone()).unwrap();

        assert_eq!(result.trim(), expected);
    }
}
