use lox_bytecode::{Chunk, Op};
use lox_source::{
    ast::{
        decl::Decl,
        expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, Number, UnaryOperator},
        program::Program,
        stmt::Stmt,
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
    pub fn new(source: Program) -> Self {
        Self {
            ast: source,
            chunk: Chunk::new(),
        }
    }

    pub fn new_from_source(source: &'c str) -> Result<Self, CompilerError> {
        let program = Parser::from_source(source).parse()?;

        Ok(Compiler::new(program))
    }

    pub fn compile(&mut self) -> Result<(), CompilerError> {
        let _ = self.visit_program(self.ast.clone())?;

        Ok(())
    }

    pub fn bytecode(self) -> Chunk {
        self.chunk
    }

    fn write_int(&mut self, int: i64) {
        let idx = self.chunk.add_int(int);
        self.chunk.add_op(Op::Integer(idx));
    }

    fn write_float(&mut self, float: f64) {
        let idx = self.chunk.add_float(float);
        self.chunk.add_op(Op::Float(idx));
    }

    fn write_string(&mut self, string: String) {
        let idx = self.chunk.add_string(string);
        self.chunk.add_op(Op::String(idx))
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
            Expr::Var(id) => {
                let idx = self.chunk.add_string(id.name);
                self.chunk.add_op(Op::GetGlobal(idx));
                Ok(())
            }
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
        }
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        let _ = self.visit_expr(expr)?;

        let opcode = match op {
            UnaryOperator::Neg => Op::Negate,
            UnaryOperator::Not => Op::Not,
        };

        self.chunk.add_op(opcode);

        Ok(())
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value {
        let _ = self.visit_expr(left)?;
        let _ = self.visit_expr(right)?;

        let opcode: &[Op] = match op {
            BinaryOperator::Eq => &[Op::Equal],
            BinaryOperator::Neq => &[Op::Equal, Op::Not],
            BinaryOperator::Lt => &[Op::Less],
            BinaryOperator::Lte => &[Op::Less, Op::Not],
            BinaryOperator::Gt => &[Op::Greater],
            BinaryOperator::Gte => &[Op::Greater, Op::Not],
            BinaryOperator::Add => &[Op::Add],
            BinaryOperator::Sub => &[Op::Subtract],
            BinaryOperator::Mul => &[Op::Multiply],
            BinaryOperator::Div => &[Op::Divide],
        };

        for op in opcode {
            self.chunk.add_op(op.clone());
        }

        Ok(())
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        match literal {
            Literal::Number(Number::Float(f)) => {
                self.write_float(f);
            }
            Literal::Number(Number::Int(i)) => {
                self.write_int(i);
            }
            Literal::String(s) => self.write_string(s),
            Literal::Bool(true) => self.chunk.add_op(Op::True),
            Literal::Bool(false) => self.chunk.add_op(Op::False),
            Literal::Nil => self.chunk.add_op(Op::Nil),
        };

        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        self.visit_expr(expr)
    }

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        let _ = self.visit_expr(expr)?;

        let idx = self.chunk.add_string(id.name);
        self.chunk.add_op(Op::SetGlobal(idx));

        Ok(())
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        todo!()
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        todo!()
    }

    fn visit_program(&mut self, program: Program) -> Self::Value {
        let result = program
            .declarations
            .iter()
            .map(|d| self.visit_declaration(d.clone()))
            .last();

        result.unwrap_or(Ok(()))
    }

    fn visit_declaration(&mut self, decl: Decl) -> Self::Value {
        match decl {
            Decl::Class(id, superclass, funcs) => {
                self.visit_class_delcaration(id, superclass, funcs)
            }
            Decl::Func(name, parameters, body) => {
                self.visit_func_declaration(name, parameters, body)
            }
            Decl::Var(id, expr) => {
                let _ = match expr {
                    Some(expr) => self.visit_expr(expr)?,
                    None => {
                        self.chunk.add_op(Op::Nil);
                    }
                };

                self.chunk.add_string(id.name);

                Ok(())
            }
            Decl::Stmt(stmt) => self.visit_stmt(stmt),
        }
    }

    fn visit_class_delcaration(
        &mut self,
        id: Identifier,
        superclass: Option<Identifier>,
        funcs: Vec<Decl>,
    ) -> Self::Value {
        todo!()
    }

    fn visit_stmt(&mut self, stmt: Stmt) -> Self::Value {
        match stmt {
            Stmt::Block(_) => todo!(),
            Stmt::Expr(expr) => {
                let _ = self.visit_expr(expr)?;
                self.chunk.add_op(Op::Pop);

                Ok(())
            }
            Stmt::Print(expr) => {
                let _ = self.visit_expr(expr)?;
                self.chunk.add_op(Op::Print);

                Ok(())
            }
            Stmt::Return(_) => todo!(),
            Stmt::If(_, _, _) => todo!(),
            Stmt::While(_, _) => todo!(),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        todo!()
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        todo!()
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        todo!()
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        todo!()
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        todo!()
    }
}

#[cfg(test)]
mod test {
    use lox_bytecode::Op;

    use super::*;

    #[test]
    fn number_literals() {
        let input = "123;";
        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn grouping() {
        let input = "(123);";
        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn unary_negation() {
        let input = "-123;";
        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_op(Op::Negate);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_add() {
        let input = "123 + 456;";

        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_int(456);
        expected.add_op(Op::Integer(1));
        expected.add_op(Op::Add);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_sub() {
        let input = "123 - 456;";

        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_int(456);
        expected.add_op(Op::Integer(1));
        expected.add_op(Op::Subtract);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_mul() {
        let input = "123 * 456;";

        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_int(456);
        expected.add_op(Op::Integer(1));
        expected.add_op(Op::Multiply);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_div() {
        let input = "123 / 456;";

        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_int(456);
        expected.add_op(Op::Integer(1));
        expected.add_op(Op::Divide);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn booleans() {
        let input = "!true;";

        let mut expected = Chunk::new();
        expected.add_op(Op::True);
        expected.add_op(Op::Not);
        expected.add_op(Op::Pop);

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn snapshots() {
        let input = r#"var beverage = "cafe au lait";
var breakfast = "beignets with " + beverage;
print breakfast;"#;

        let mut compiler = Compiler::new_from_source(input).unwrap();
        let _ = compiler.compile().unwrap();
        let result = compiler.bytecode().disassemble();

        insta::assert_yaml_snapshot!(result);
    }
}
