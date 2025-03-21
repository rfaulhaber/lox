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
use lox_vm::bytecode::{Chunk, Op};
use lox_vm::value::Function;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum CompilerError {
    #[error("Encountered parser error {0}")]
    ParserError(#[from] ParseError),
    #[error("Too many local variables declared")]
    LocalVariableLimit,
    #[error("Duplicate variable {0} in scope")]
    DuplicateVariableInScope(String),
    #[error("Insufficient context depth")]
    NoContextFound,
}

pub type CompilerResult = Result<Chunk, CompilerError>;

pub const LOCALS_COUNT: u8 = u8::MAX;

#[derive(Debug)]
pub struct Local {
    name: String,
    depth: usize,
    initialized: bool,
}

#[derive(Debug)]
pub struct Context {
    locals: Vec<Local>,
    scope_depth: usize,
}

impl Context {
    pub fn new() -> Self {
        Self {
            locals: Vec::with_capacity(LOCALS_COUNT.into()),
            scope_depth: 0,
        }
    }
}

pub struct Compiler {
    ast: Program,
    chunk: Chunk,
    context: Vec<Context>,
}

enum JumpType {
    JumpIfFalse,
    JumpIfFalseWithExtraOffset,
    Jump,
}

impl<'c> Compiler {
    pub fn new(source: Program) -> Self {
        Self {
            ast: source,
            chunk: Chunk::new(),
            context: {
                let mut context = Vec::with_capacity(256);
                context.push(Context::new());
                context
            },
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

    fn begin_scope(&mut self) -> Result<(), CompilerError> {
        self.context
            .last_mut()
            .map(|ctx| ctx.scope_depth += 1)
            .ok_or(CompilerError::NoContextFound)
    }

    fn end_scope(&mut self) -> Result<(), CompilerError> {
        let count = self
            .context
            .last_mut()
            .map(|ctx| {
                ctx.locals
                    .iter()
                    .filter(|l| l.depth == ctx.scope_depth)
                    .count()
            })
            .ok_or(CompilerError::NoContextFound)?;

        for _ in 0..count {
            self.chunk.add_op(Op::Pop);
        }

        self.context
            .last_mut()
            .map(|ctx| {
                ctx.scope_depth -= 1;
                ctx.locals.retain(|local| local.depth <= ctx.scope_depth);
            })
            .ok_or(CompilerError::NoContextFound)
    }

    fn begin_context(&mut self) {
        self.context.push(Context::new());
    }

    fn end_context(&mut self) {
        let _ = self.context.pop();
    }

    fn scope_depth(&mut self) -> usize {
        self.context
            .last()
            .map(|ctx| ctx.scope_depth)
            .unwrap_or_default() // maybe I should not do this lol
    }

    fn locals_count(&mut self) -> usize {
        self.ref_context(|ctx| ctx.locals.len()).unwrap_or_default()
    }

    fn add_local(&mut self, name: String) -> Result<usize, CompilerError> {
        self.context
            .last_mut()
            .map(|ctx| {
                let idx = ctx.locals.len();
                ctx.locals.push(Local {
                    name,
                    depth: ctx.scope_depth,
                    initialized: false,
                });
                idx
            })
            .ok_or(CompilerError::NoContextFound)
    }

    fn find_local(&mut self, name: &String) -> Option<&Local> {
        if self.scope_depth() == 0 {
            return None;
        }

        self.context.last().map(|ctx| {
            ctx.locals
                .iter()
                .find(|local| local.name == *name && local.depth == ctx.scope_depth)
        })?
    }

    fn lookup_local(&mut self, name: &String) -> Option<(usize, &Local)> {
        if self.scope_depth() == 0 {
            return None;
        }

        self.context.last().map(|ctx| {
            ctx.locals
                .iter()
                .enumerate()
                .rev()
                .find(|(_, local)| local.name == *name)
        })?
    }

    fn initialize_local(&mut self, idx: usize) -> Result<(), CompilerError> {
        self.use_context(|ctx| ctx.locals[idx].initialized = true)
    }

    fn ref_context<F, R>(&mut self, func: F) -> Result<R, CompilerError>
    where
        F: FnOnce(&Context) -> R,
    {
        self.context
            .last()
            .map(func)
            .ok_or(CompilerError::NoContextFound)
    }

    fn use_context<F, R>(&mut self, func: F) -> Result<R, CompilerError>
    where
        F: FnOnce(&mut Context) -> R,
    {
        self.context
            .last_mut()
            .map(func)
            .ok_or(CompilerError::NoContextFound)
    }

    fn jump_around<F>(&mut self, jump_type: JumpType, func: F) -> Result<(), CompilerError>
    where
        F: FnOnce(&mut Compiler) -> Result<(), CompilerError>,
    {
        let start_pos = self.chunk.code_len();

        let _ = func(self)?;

        let dist = self.chunk.code_len() - start_pos;

        let op = match jump_type {
            JumpType::JumpIfFalse => Op::JumpIfFalse(dist + 1),
            JumpType::JumpIfFalseWithExtraOffset => Op::JumpIfFalse(dist + 2),
            JumpType::Jump => Op::Jump(dist + 1),
        };

        self.chunk.insert_op(start_pos, op);
        self.chunk.insert_op(start_pos + 1, Op::Pop);

        Ok(())
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
            BinaryOperator::Lte => &[Op::Greater, Op::Not],
            BinaryOperator::Gt => &[Op::Greater],
            BinaryOperator::Gte => &[Op::Less, Op::Not],
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
            self.initialize_local(idx)?;
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
                self.jump_around(JumpType::JumpIfFalse, |compiler| compiler.visit_expr(right))?;
            }
            LogicalOperator::Or => {
                self.chunk.add_op(Op::JumpIfFalse(1));
                let jump_pos = self.chunk.code_len();

                self.chunk.add_op(Op::Pop);

                let _ = self.visit_expr(right)?;

                let dist = self.chunk.code_len() - jump_pos;

                self.chunk.insert_op(jump_pos, Op::Jump(dist));
            }
        };

        Ok(())
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        let _ = self.visit_expr(callee)?;

        let arg_count = arguments.len();

        for expr in arguments {
            let _ = self.visit_expr(expr)?;
        }

        self.chunk.add_op(Op::Call(arg_count));

        Ok(())
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

                if self.scope_depth() == 0 {
                    let idx = self.chunk.add_string(name);
                    self.chunk.add_op(Op::DefineGlobal(idx));
                } else {
                    if self.locals_count() == LOCALS_COUNT.into() {
                        return Err(CompilerError::LocalVariableLimit);
                    }

                    let existing_local = self.find_local(&name);

                    if existing_local.is_some() {
                        return Err(CompilerError::DuplicateVariableInScope(name));
                    }

                    let idx = self.add_local(name)?;
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
            Stmt::Return(expr) => self.visit_return_stmt(expr),
            Stmt::If(cond, stmt, else_stmt) => {
                self.visit_if_stmt(cond, *stmt, else_stmt.map(|v| *v))
            }
            Stmt::While(cond, body) => self.visit_while_stmt(cond, *body),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        self.begin_scope()?;

        for decl in block {
            self.visit_declaration(decl)?;
        }

        self.end_scope()?;

        Ok(())
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        let _ = self.visit_expr(cond)?;

        if let Some(else_stmt) = else_stmt {
            self.jump_around(JumpType::JumpIfFalseWithExtraOffset, |compiler| {
                compiler.visit_stmt(stmt)
            })?;

            self.jump_around(JumpType::Jump, |compiler| compiler.visit_stmt(else_stmt))?;
        } else {
            self.jump_around(JumpType::JumpIfFalse, |compiler| compiler.visit_stmt(stmt))?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        let loop_start = self.chunk.code_len();

        let _ = self.visit_expr(cond)?;

        self.jump_around(JumpType::JumpIfFalseWithExtraOffset, |compiler| {
            compiler.visit_stmt(body)
        })?;

        let offset = self.chunk.code_len() - loop_start + 1;
        self.chunk.add_op(Op::Loop(offset));
        self.chunk.add_op(Op::Pop);

        Ok(())
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        let arity = parameters.len();

        self.begin_context();

        let errors = parameters
            .iter()
            .map(|var| {
                self.context
                    .last_mut()
                    .map(|ctx| {
                        ctx.locals.push(Local {
                            name: var.name.clone(),
                            depth: ctx.scope_depth + 1,
                            initialized: false,
                        })
                    })
                    .ok_or(CompilerError::NoContextFound)
            })
            .filter(|r| r.is_err())
            .collect::<Vec<Result<(), CompilerError>>>();

        if !errors.is_empty() {
            return errors.first().cloned().unwrap();
        }

        let mut fn_chunk = Chunk::new();

        std::mem::swap(&mut self.chunk, &mut fn_chunk);

        let _ = self.visit_stmt(body)?;

        std::mem::swap(&mut self.chunk, &mut fn_chunk);

        self.end_context();

        self.chunk
            .push_fn(Function::new_named(name.name, fn_chunk, arity));

        Ok(())
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        if let Some(expr) = expr {
            let _ = self.visit_expr(expr)?;
        } else {
            self.chunk.add_op(Op::Nil);
        }

        self.chunk.add_op(Op::Return);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use lox_vm::bytecode::Op;

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
}
