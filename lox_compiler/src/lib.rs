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
    #[error("Too many local variables declared")]
    LocalVariableLimit,
    #[error("Duplicate variable {0} in scope")]
    DuplicateVariableInScope(String),
}

pub type CompilerResult = Result<Chunk, CompilerError>;

pub const LOCALS_COUNT: u8 = u8::MAX;

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: usize,
    initialized: bool,
}

pub struct Compiler {
    ast: Program,
    chunk: Chunk,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl<'c> Compiler {
    pub fn new(source: Program) -> Self {
        Self {
            ast: source,
            chunk: Chunk::new(),
            locals: Vec::with_capacity(LOCALS_COUNT.into()),
            scope_depth: 0,
        }
    }

    pub fn new_from_source(source: &'c str) -> Result<Self, CompilerError> {
        let program = Parser::from_source(source).parse()?;

        Ok(Compiler::new(program))
    }

    pub fn compile(mut self) -> Result<Chunk, CompilerError> {
        let _ = self.visit_program(self.ast.clone())?;

        Ok(self.chunk)
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

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.locals
            .iter()
            .filter(|l| l.depth == self.scope_depth)
            .for_each(|_| self.chunk.add_op(Op::Pop));

        self.scope_depth -= 1;
    }

    fn add_local(&mut self, name: String) -> usize {
        let idx = self.locals.len();
        self.locals.insert(
            idx,
            Local {
                name,
                depth: self.scope_depth,
                initialized: false,
            },
        );

        idx
    }

    fn find_local(&mut self, name: &String) -> Option<&Local> {
        if self.scope_depth == 0 {
            return None;
        }

        self.locals
            .iter()
            .find(|local| local.name == *name && local.depth == self.scope_depth)
    }

    fn lookup_local(&mut self, name: &String) -> Option<(usize, &Local)> {
        if self.scope_depth == 0 {
            return None;
        }

        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == *name)
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
                let name = id.name;

                let existing_local = self.lookup_local(&name);

                if let Some((idx, _)) = existing_local {
                    self.chunk.add_op(Op::GetLocal(idx));
                } else {
                    let idx = self.chunk.add_string(name);
                    self.chunk.add_op(Op::GetGlobal(idx));
                }

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

        let name = id.name;

        let existing_local = self.lookup_local(&name);
        if let Some((idx, _)) = existing_local {
            self.locals[idx].initialized = true;
            self.chunk.add_op(Op::SetLocal(idx));
        } else {
            let idx = self.chunk.add_string(name);
            self.chunk.add_op(Op::SetGlobal(idx));
        }

        Ok(())
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        let _ = self.visit_expr(left)?;

        match op {
            LogicalOperator::And => {
                todo!();
            }
            LogicalOperator::Or => {
                todo!();
            }
        };
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        todo!()
    }

    fn visit_program(&mut self, program: Program) -> Self::Value {
        program
            .declarations
            .iter()
            .map(|d| self.visit_declaration(d.clone()))
            .collect()
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

                let name = id.name;

                if self.scope_depth == 0 {
                    let idx = self.chunk.add_string(name);
                    self.chunk.add_op(Op::DefineGlobal(idx));
                } else {
                    if self.locals.len() == LOCALS_COUNT.into() {
                        return Err(CompilerError::LocalVariableLimit);
                    }

                    let existing_local = self.find_local(&name);

                    if existing_local.is_some() {
                        return Err(CompilerError::DuplicateVariableInScope(name));
                    }

                    let idx = self.add_local(name);
                    self.chunk.add_op(Op::SetLocal(idx));
                }

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
            Stmt::Block(decls) => self.visit_block(decls),
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
            Stmt::If(cond, stmt, else_stmt) => {
                self.visit_if_stmt(cond, *stmt, else_stmt.map(|v| *v))
            }
            Stmt::While(_, _) => todo!(),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        self.begin_scope();

        for decl in block {
            self.visit_declaration(decl)?;
        }

        self.end_scope();

        Ok(())
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        let _ = self.visit_expr(cond)?;

        // TODO check this jump math it doesn't look right
        // TODO clean up

        let then_jump = self.chunk.code_len();

        self.visit_stmt(stmt)?;

        let jump = self.chunk.code_len() - then_jump;

        if let Some(stmt) = else_stmt {
            self.chunk.insert_op(then_jump, Op::JumpIfFalse(jump + 2));
            self.chunk.insert_op(then_jump + 1, Op::Pop);
            let else_jump = self.chunk.code_len();
            self.visit_stmt(stmt)?;
            let else_jump_pos = self.chunk.code_len() - else_jump;
            self.chunk.insert_op(else_jump, Op::Jump(else_jump_pos + 1));
            self.chunk.insert_op(else_jump + 1, Op::Pop);
        } else {
            self.chunk.insert_op(then_jump, Op::JumpIfFalse(jump + 1));
            self.chunk.insert_op(then_jump + 1, Op::Pop);
        }

        Ok(())
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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn grouping() {
        let input = "(123);";
        let mut expected = Chunk::new();
        expected.add_int(123);
        expected.add_op(Op::Integer(0));
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

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

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn booleans() {
        let input = "!true;";

        let mut expected = Chunk::new();
        expected.add_op(Op::True);
        expected.add_op(Op::Not);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.disassemble(), expected.disassemble());
    }

    #[test]
    fn global_variable_snapshot() {
        let input = r#"var breakfast = "beignets";
var beverage = "cafe au lait";
breakfast = "beignets with " + beverage;

print breakfast;"#;

        let result = Compiler::new_from_source(input)
            .unwrap()
            .compile()
            .unwrap()
            .disassemble();

        insta::assert_yaml_snapshot!(result);
    }

    #[test]
    fn local_variable_snapshot() {
        let input = r#"var test = "global";
{
    var test = "inner";
    {
        var test = "inner2";
        var innermost = "innermost";
    }
}
 "#;

        let result = Compiler::new_from_source(input)
            .unwrap()
            .compile()
            .unwrap()
            .disassemble();

        insta::assert_yaml_snapshot!(result);
    }

    #[test]
    fn conditional_if() {
        let input = r#"var x = false;
if (x) {
    print "x";
}
"#;

        let result = Compiler::new_from_source(input)
            .unwrap()
            .compile()
            .unwrap()
            .disassemble();

        insta::assert_yaml_snapshot!(result);
    }

    #[test]
    fn conditional_else() {
        let input = r#"var x = false;
if (x) {
    print "fizz";
} else {
    print "buzz";
}
"#;

        let result = Compiler::new_from_source(input)
            .unwrap()
            .compile()
            .unwrap()
            .disassemble();

        insta::assert_yaml_snapshot!(result);
    }
}
