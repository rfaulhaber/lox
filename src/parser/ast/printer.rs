use std::fmt::Write;

use super::{
    decl::Decl,
    expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, Number, UnaryOperator},
    program::Program,
    stmt::Stmt,
    visitor::{ExprVisitor, StmtVisitor},
};

pub struct AstPrinter {
    writer: String,
}

impl AstPrinter {
    pub fn new() -> AstPrinter {
        Self {
            writer: String::new(),
        }
    }

    pub fn print(&mut self, ast: Program) -> String {
        self.visit_program(ast);
        self.writer.to_owned()
    }
}

impl StmtVisitor for AstPrinter {
    fn visit_program(&mut self, program: Program) {
        program
            .declarations
            .into_iter()
            .for_each(|decl| self.visit_declaration(decl))
    }

    fn visit_stmt(&mut self, stmt: Stmt) {
        let stmt = match stmt {
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::Print(expr) => format!("(print {})", self.visit_expr(expr)).into(),
            Stmt::Block(stmts) => {
                self.visit_block(stmts);
                "".into() // stupid
            }
            Stmt::If(cond, stmt, else_stmt) => {
                self.visit_if_stmt(cond, *stmt, else_stmt.map(|f| *f));
                "".into()
            }
            Stmt::While(cond, body) => {
                self.visit_while_stmt(cond, *body);
                "".into()
            }
        };

        let res = write!(self.writer, "{}", stmt);

        if res.is_err() {
            panic!("could not parse: {:?}", res);
        }
    }

    fn visit_declaration(&mut self, decl: Decl) {
        match decl {
            Decl::Var(identifier, expr) => match expr {
                Some(expr) => {
                    let expr = self.visit_expr(expr);
                    write!(self.writer, "(var {} {})", identifier.name, expr).unwrap()
                }
                None => write!(self.writer, "(var {})", identifier.name).unwrap(),
            },
            Decl::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) {
        block.into_iter().for_each(|b| self.visit_declaration(b))
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) {
        let expr = self.visit_expr(cond);

        write!(self.writer, "(if {} ", expr);

        self.visit_stmt(stmt);

        write!(self.writer, " ");

        if else_stmt.is_some() {
            self.visit_stmt(else_stmt.unwrap());
        }

        write!(self.writer, ")");
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) {
        let expr = self.visit_expr(cond);

        write!(self.writer, "(while {}", expr);

        self.visit_stmt(body);

        write!(self.writer, " )");
    }
}

impl ExprVisitor for AstPrinter {
    type Value = String;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, rhs) => self.visit_unary_expr(op, *rhs),
            Expr::Binary(lhs, op, rhs) => self.visit_binary_expr(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.visit_grouping_expr(*expr),
            Expr::Var(var) => var.name,
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
            Expr::Call(callee, arguments) => self.visit_call_expr(*callee, arguments),
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

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        let expr = self.visit_expr(expr);

        format!("(assign {} {})", id.name, expr)
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        let left = self.visit_expr(left);
        let right = self.visit_expr(right);

        let op = match op {
            LogicalOperator::And => "and",
            LogicalOperator::Or => "or",
        };

        format!("({} {} {})", op, left, right)
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        let expr = self.visit_expr(callee);
        let arguments: Vec<String> = arguments
            .into_iter()
            .map(|expr| self.visit_expr(expr))
            .collect();

        format!("(call {} {:?})", expr, arguments)
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

        let result = AstPrinter::new().print(ast);

        assert_eq!(result, expected);
    }
}
