use lox_bytecode::Chunk;
use lox_source::{
    ast::{
        expr::{Expr, Literal, Number},
        program::Program,
        visitor::Visitor,
    },
    parser::{ParseError, Parser},
};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum CompilerError {
    #[error("Encountered parser error {0}")]
    ParserError(#[from] ParseError),
}

pub type CompilerResult = Result<Chunk, CompilerError>;

pub struct Compiler {
    ast: Program,
    chunk: Chunk,
}

impl<'c> Compiler {
    pub fn new(source: &'c str) -> Result<Self, CompilerError> {
        let program = Parser::from_source(source).parse()?;

        Ok(Self {
            ast: program,
            chunk: Chunk::new(),
        })
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let _ = self.visit_program(self.ast.clone())?;

        Ok(())
    }

    pub fn bytecode(self) -> Chunk {
        self.chunk
    }
}

impl Visitor for Compiler {
    type Value = Result<(), CompilerError>;

    fn visit_expr(&mut self, expr: Expr) -> Self::Value {
        match expr {
            Expr::Literal(l) => self.visit_literal(l),
            Expr::Unary(op, expr) => self.visit_unary_expr(op, *expr),
            Expr::Call(callee, arguments) => self.visit_call_expr(*callee, arguments),
            Expr::Binary(left, op, right) => self.visit_binary_expr(*left, op, *right),
            Expr::Logical(left, op, right) => self.visit_logical_expr(*left, op, *right),
            Expr::Grouping(expr) => self.visit_expr(*expr),
            Expr::Get(_, _) => todo!(),
            Expr::Set(_, _, _) => todo!(),
            Expr::Var(id) => todo!(),
            Expr::Assignment(id, expr) => todo!(),
        }
    }

    fn visit_unary_expr(
        &mut self,
        op: lox_source::ast::expr::UnaryOperator,
        expr: Expr,
    ) -> Self::Value {
        todo!()
    }

    fn visit_binary_expr(
        &mut self,
        left: Expr,
        op: lox_source::ast::expr::BinaryOperator,
        right: Expr,
    ) -> Self::Value {
        todo!()
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        match literal {
            Literal::Number(Number::Float(f)) => todo!(),
            Literal::Number(Number::Int(i)) => todo!(),
            Literal::String(_) => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Nil => todo!(),
        }
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        todo!()
    }

    fn visit_assignment_expr(
        &mut self,
        id: lox_source::ast::expr::Identifier,
        expr: Expr,
    ) -> Self::Value {
        todo!()
    }

    fn visit_logical_expr(
        &mut self,
        left: Expr,
        op: lox_source::ast::expr::LogicalOperator,
        right: Expr,
    ) -> Self::Value {
        todo!()
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        todo!()
    }

    fn visit_program(&mut self, program: lox_source::ast::program::Program) -> Self::Value {
        todo!()
    }

    fn visit_declaration(&mut self, decl: lox_source::ast::decl::Decl) -> Self::Value {
        todo!()
    }

    fn visit_class_delcaration(
        &mut self,
        id: lox_source::ast::expr::Identifier,
        superclass: Option<lox_source::ast::expr::Identifier>,
        funcs: Vec<lox_source::ast::decl::Decl>,
    ) -> Self::Value {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: lox_source::ast::stmt::Stmt) -> Self::Value {
        todo!()
    }

    fn visit_block(&mut self, block: Vec<lox_source::ast::decl::Decl>) -> Self::Value {
        todo!()
    }

    fn visit_if_stmt(
        &mut self,
        cond: Expr,
        stmt: lox_source::ast::stmt::Stmt,
        else_stmt: Option<lox_source::ast::stmt::Stmt>,
    ) -> Self::Value {
        todo!()
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: lox_source::ast::stmt::Stmt) -> Self::Value {
        todo!()
    }

    fn visit_func_declaration(
        &mut self,
        name: lox_source::ast::expr::Identifier,
        parameters: Vec<lox_source::ast::expr::Identifier>,
        body: lox_source::ast::stmt::Stmt,
    ) -> Self::Value {
        todo!()
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        todo!()
    }
}
