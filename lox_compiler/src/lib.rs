// lox_compiler/src/lib.rs

use lox_source::{
    ast::{
        decl::Decl,
        expr::{BinaryOperator, Expr, Identifier, Literal, LogicalOperator, UnaryOperator},
        program::Program,
        stmt::Stmt,
        visitor::Visitor,
    },
    parser::{ParseError, Parser},
};
use lox_vm::value::{Function, Value};
use lox_vm::{
    bytecode::{Chunk, Op},
    value::Upvalue,
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
    #[error("Insufficient context depth")]
    NoContextFound,
}

pub type CompilerResult = Result<Chunk, CompilerError>;

pub const LOCALS_COUNT: u8 = u8::MAX;

#[derive(Debug, Clone)]
pub struct Local {
    name: String,
    depth: usize,
    initialized: bool,
}

#[derive(Debug)]
pub struct Context {
    function: Function,
    locals: Vec<Local>,
    scope_depth: usize,
}

impl<'c> Context {
    pub fn new(function_name: Option<String>, arity: usize) -> Self {
        let mut locals = Vec::with_capacity(LOCALS_COUNT as usize);
        // Reserve slot 0 for the function itself (or 'this' in methods)
        locals.push(Local {
            // Name slot 0 appropriately depending on context (e.g., "" for script/function, "this" for method)
            name: "".to_string(),
            depth: 0,
            initialized: true, // Slot 0 is implicitly initialized
        });
        Self {
            function: match function_name {
                Some(name) => Function::new_named(name, Chunk::new(), arity),
                None => Function::new_anonymous(Chunk::new(), arity),
            },
            locals,
            scope_depth: 0,
        }
    }

    pub fn new_top_level() -> Self {
        Self::new(Some("top level".into()), 0)
    }

    /// Returns the index of the local and the local where `name` == local.name, if it exists.
    /// Unlike `find_local`, this is not limited to the current scope, however it does search by the most recent locals added.
    pub fn lookup_local(&self, name: &String) -> Option<(usize, &Local)> {
        if self.scope_depth == 0 {
            return None;
        }

        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == *name)
    }

    /// Returns a reference to a local named `name` declared *strictly within* the current innermost scope.
    pub fn find_local_in_current_scope(&self, name: &str) -> Option<(usize, &Local)> {
        // Search backwards only through locals at the current depth
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name == name && local.depth == self.scope_depth)
    }
}

pub struct Compiler {
    ast: Program,
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
            context: {
                let mut context = Vec::with_capacity(256);
                context.push(Context::new_top_level());
                context
            },
        }
    }

    pub fn new_from_source(source: &'c str) -> Result<Self, CompilerError> {
        let program = Parser::from_source(source).parse()?;

        Ok(Compiler::new(program))
    }

    pub fn compile(mut self) -> Result<Function, CompilerError> {
        let _ = self.visit_program(self.ast.clone())?;

        Ok(self
            .context
            .pop()
            .expect("Compiler context stack empty")
            .function)
    }

    fn begin_scope(&mut self) {
        self.current_context_mut().scope_depth += 1;
    }

    fn end_scope(&mut self) -> Result<(), CompilerError> {
        let ctx = self.current_context_mut();
        ctx.scope_depth -= 1;

        // Pop locals defined in the scope that just ended
        let mut pop_count = 0;
        while let Some(local) = ctx.locals.last() {
            if local.depth > ctx.scope_depth {
                ctx.locals.pop();
                pop_count += 1;
            } else {
                break;
            }
        }

        // Emit Pop instructions for the removed locals
        for _ in 0..pop_count {
            self.emit_op(Op::Pop);
        }
        Ok(())
    }

    fn begin_function_context(&mut self, name: Option<String>, arity: usize) {
        self.context.push(Context::new(name, arity));
    }

    fn end_function_context(&mut self) -> Result<Function, CompilerError> {
        let last_is_return = self
            .current_chunk()
            .code()
            .last()
            .map(|op| matches!(op, Op::Return))
            .unwrap();

        if !last_is_return {
            self.current_chunk_mut().add_op(Op::Nil);
            self.current_chunk_mut().add_op(Op::Return);
        }

        // TODO error handling
        let finished_function = self.context.pop().unwrap().function;

        Ok(finished_function)
    }

    #[inline]
    fn current_context(&self) -> &Context {
        self.context.last().expect("Context stack empty")
    }

    #[inline]
    fn current_context_mut(&mut self) -> &mut Context {
        self.context.last_mut().expect("Context stack empty")
    }

    #[inline]
    fn current_chunk(&self) -> &Chunk {
        self.current_context().function.chunk()
    }

    #[inline]
    fn current_chunk_mut(&mut self) -> &mut Chunk {
        self.current_context_mut().function.chunk_mut()
    }

    fn declare_variable(&mut self, name: &String) -> Result<(), CompilerError> {
        // Global scope
        if self.current_context().scope_depth == 0 {
            // For globals, definition happens when the initializer is evaluated (or Nil is pushed).
            // Nothing to do at declaration time for globals.
            return Ok(());
        }

        // Local scope
        // Check for redeclaration in the *same* scope
        if let Some(_) = self.current_context().find_local_in_current_scope(name) {
            return Err(CompilerError::DuplicateVariableInScope(name.clone()));
        }

        // TODO Check limit before adding
        // if self.current_context().locals.len() as u8 >= LOCALS_COUNT {
        //     return Err(CompilerError::LocalVariableLimit(LOCALS_COUNT));
        // }

        // Add the local as uninitialized
        let local = Local {
            name: name.clone(),
            depth: self.current_context().scope_depth,
            initialized: false, // Mark as uninitialized initially
        };

        self.current_context_mut().locals.push(local);

        Ok(())
    }

    /// Defines a variable. For locals, marks as initialized. For globals, emits DefineGlobal.
    fn define_variable(&mut self, name_const_index: Option<usize>) -> Result<(), CompilerError> {
        // If local, mark the last declared local as initialized.
        if self.current_context().scope_depth > 0 {
            self.mark_last_local_initialized();
            // No code emitted here; initialization value is already on the stack.
            return Ok(());
        }

        // If global, emit DefineGlobal using the constant index of the name.
        if let Some(index) = name_const_index {
            self.emit_op(Op::DefineGlobal(index));
        } else {
            // This should not happen if declare_variable/parse_variable handled globals correctly
            panic!("Missing constant index for global variable definition");
        }
        Ok(())
    }

    fn parse_variable(&mut self, name: &Identifier) -> Result<(Op, Op), CompilerError> {
        // Returns (get_op, set_op) templates
        let id_name = &name.name;

        // Try resolving as local first
        if let Some((index, local)) = self.current_context().lookup_local(id_name) {
            // Check if accessing uninitialized local in its own initializer (semantic check)
            if !local.initialized {
                println!("local not initialized {:?}", local);
                // This check should ideally happen *during* initializer compilation
                // return Err(CompilerError::VariableUsedInInitializer(id_name.clone()));
                // For now, we allow it but the VM might read garbage if set isn't emitted first.
            }
            // TODO: Handle index > 255 if needed (Op::Get/SetLocalLong)
            Ok((Op::GetLocal(index), Op::SetLocal(index)))
        } else {
            // Assume global
            // TODO: Add upvalue resolution here later
            let index = self.emit_const(id_name.clone())?;
            // TODO: Handle index > 255 if needed (Op::Get/SetGlobalLong)
            Ok((Op::GetGlobal(index), Op::SetGlobal(index)))
        }
    }

    /// Marks the most recently added local variable as initialized.
    fn mark_last_local_initialized(&mut self) {
        if self.current_context().scope_depth == 0 {
            return;
        }

        self.current_context_mut()
            .locals
            .last_mut()
            .map(|l| l.initialized = true);
    }

    #[inline]
    fn emit_op(&mut self, op: Op) {
        // TODO: Add line number information from AST node
        self.current_chunk_mut().add_op(op);
    }

    #[inline]
    fn emit_ops(&mut self, ops: &[Op]) {
        for op in ops {
            self.emit_op(op.clone());
        }
    }

    /// Emits a constant and returns its index.
    fn emit_const<V: Into<Value>>(&mut self, value: V) -> Result<usize, CompilerError> {
        let index = self.current_chunk_mut().add_const(value.into());
        // Optional: Check if index exceeds max constants (e.g., u8::MAX or u16::MAX)
        Ok(index)
    }

    /// Emits Op::Const for a given value.
    fn emit_const_op<V: Into<Value>>(&mut self, value: V) -> Result<(), CompilerError> {
        let index = self.emit_const(value)?;
        // TODO: Handle potential need for Op::ConstLong if index > 255
        self.emit_op(Op::Const(index));
        Ok(())
    }

    /// Emits a jump instruction (like JumpIfFalse or Jump) with a placeholder offset.
    /// Returns the index of the placeholder instruction for later patching.
    fn emit_jump(&mut self, jump_op_template: Op) -> usize {
        self.emit_op(jump_op_template); // Emit with placeholder offset (usually 0 or max)
        self.current_chunk_mut().code_len() - 1 // Return index of the jump op
    }

    /// Patches a previously emitted jump instruction at `jump_index`.
    /// Calculates the offset from the instruction *after* the jump to the current end of the chunk.
    fn patch_jump(&mut self, jump_index: usize) -> Result<(), CompilerError> {
        // Offset = (current code end) - (index after jump instruction)
        let offset = self.current_chunk_mut().code_len() - (jump_index + 1);

        // TODO: Check if offset fits in the jump instruction's operand (e.g., u16)
        // if offset > u16::MAX as usize {
        //     // Assuming jumps use u16 offsets eventually
        //     return Err(CompilerError::JumpOffsetTooLarge);
        // }

        // Update the jump instruction's offset
        match self.current_chunk_mut().code_mut()[jump_index] {
            Op::JumpIfFalse(ref mut placeholder) => *placeholder = offset,
            Op::Jump(ref mut placeholder) => *placeholder = offset,
            // Add other jump types if needed
            // TODO do not panic?
            _ => panic!(
                "Attempted to patch non-jump instruction at index {}",
                jump_index
            ),
        }
        Ok(())
    }

    /// Emits a loop instruction (jumps backward).
    fn emit_loop(&mut self, loop_start_index: usize) -> Result<(), CompilerError> {
        // Offset = (instruction after loop op) - loop_start_index
        let offset = self.current_chunk_mut().code_len() + 1 - loop_start_index; // +1 for the loop op itself

        // TODO: Check offset size
        // if offset > u16::MAX as usize {
        //     return Err(CompilerError::JumpOffsetTooLarge);
        // }

        self.emit_op(Op::Loop(offset));
        Ok(())
    }

    /// Emits Nil and Return. Should be called at the end of function compilation.
    fn emit_return(&mut self) -> Result<(), CompilerError> {
        self.emit_op(Op::Nil);
        self.emit_op(Op::Return);
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
            Expr::Get(_, _) => todo!("get expressions not implemented"),
            Expr::Set(_, _, _) => todo!("set expressions not implemented"),
            Expr::Var(id) => {
                let (get_op, _) = self.parse_variable(&id)?;
                self.emit_op(get_op);
                Ok(())
            }
            Expr::Assignment(id, expr) => self.visit_assignment_expr(id, *expr),
        }
    }

    fn visit_unary_expr(&mut self, op: UnaryOperator, expr: Expr) -> Self::Value {
        self.visit_expr(expr)?; // Compile operand first

        match op {
            UnaryOperator::Neg => self.emit_op(Op::Negate),
            UnaryOperator::Not => self.emit_op(Op::Not),
        }

        Ok(())
    }

    fn visit_binary_expr(&mut self, left: Expr, op: BinaryOperator, right: Expr) -> Self::Value {
        self.visit_expr(left)?;
        self.visit_expr(right)?;

        match op {
            BinaryOperator::Eq => self.emit_op(Op::Equal),
            BinaryOperator::Neq => self.emit_ops(&[Op::Equal, Op::Not]),
            BinaryOperator::Lt => self.emit_op(Op::Less),
            BinaryOperator::Lte => self.emit_ops(&[Op::Greater, Op::Not]),
            BinaryOperator::Gt => self.emit_op(Op::Greater),
            BinaryOperator::Gte => self.emit_ops(&[Op::Less, Op::Not]),
            BinaryOperator::Add => self.emit_op(Op::Add),
            BinaryOperator::Sub => self.emit_op(Op::Subtract),
            BinaryOperator::Mul => self.emit_op(Op::Multiply),
            BinaryOperator::Div => self.emit_op(Op::Divide),
        }
        Ok(())
    }

    fn visit_literal(&mut self, literal: Literal) -> Self::Value {
        match literal {
            Literal::Number(n) => self.emit_const_op(n)?,
            Literal::String(s) => self.emit_const_op(s)?,
            Literal::Bool(true) => self.emit_op(Op::True),
            Literal::Bool(false) => self.emit_op(Op::False),
            Literal::Nil => self.emit_op(Op::Nil),
        };

        Ok(())
    }

    fn visit_grouping_expr(&mut self, expr: Expr) -> Self::Value {
        self.visit_expr(expr)
    }

    fn visit_assignment_expr(&mut self, id: Identifier, expr: Expr) -> Self::Value {
        self.visit_expr(expr)?; // Evaluate the RHS value first
        let (_, set_op) = self.parse_variable(&id)?; // Get the appropriate Set instruction
        self.emit_op(set_op);
        // Assignment is an expression, leave the assigned value on the stack
        Ok(())
    }

    fn visit_logical_expr(&mut self, left: Expr, op: LogicalOperator, right: Expr) -> Self::Value {
        match op {
            LogicalOperator::And => {
                // Compile LHS
                self.visit_expr(left)?;
                // If LHS is false, jump over RHS and the Pop after it
                let end_jump = self.emit_jump(Op::JumpIfFalse(0)); // Placeholder offset
                // If LHS was true, it's still on the stack. Pop it.
                self.emit_op(Op::Pop);
                // Compile RHS (only executed if LHS was true)
                self.visit_expr(right)?;
                // Patch the jump to land here
                self.patch_jump(end_jump)?;
            }
            LogicalOperator::Or => {
                // Compile LHS
                self.visit_expr(left)?;
                // If LHS is falsey, jump to RHS evaluation
                let else_jump = self.emit_jump(Op::JumpIfFalse(0)); // Placeholder
                // If LHS is true, jump *over* RHS evaluation
                let end_jump = self.emit_jump(Op::Jump(0)); // Placeholder

                // Patch else_jump: If LHS was false, land here.
                self.patch_jump(else_jump)?;
                // Pop the falsey LHS value before evaluating RHS
                self.emit_op(Op::Pop);
                // Compile RHS
                self.visit_expr(right)?;

                // Patch end_jump: If LHS was true, land here, skipping RHS and Pop.
                self.patch_jump(end_jump)?;
            }
        }
        Ok(())
    }

    fn visit_call_expr(&mut self, callee: Expr, arguments: Vec<Expr>) -> Self::Value {
        let arg_count = arguments.len();
        // TODO: Check arg_count limit (e.g., 255)
        for arg in arguments {
            self.visit_expr(arg)?; // Push arguments onto stack
        }
        self.visit_expr(callee)?; // Push callee onto stack
        self.emit_op(Op::Call(arg_count));
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
                todo!("Class declaration compilation not implemented")
            }
            Decl::Func(name, parameters, body) => {
                self.visit_func_declaration(name, parameters, body)
            }
            Decl::Var(id, initializer) => {
                let global_name_index: Option<usize>;

                // Declare variable (adds local or checks global)
                self.declare_variable(&id.name)?;

                // Compile initializer or push Nil
                if let Some(expr) = initializer.clone() {
                    self.visit_expr(expr)?;
                    // --- Semantic Check ---
                    // If local, check if initializer references the var being declared
                    // This requires resolving identifiers within the expression, which
                    // might need a separate analysis pass or checks within visit_expr(Expr::Var).
                    // For now, we skip this detailed check.
                    // if self.current_context().scope_depth > 0 {
                    //     if contains_reference(&expr, &id.name) { ... }
                    // }
                    // --- End Semantic Check ---
                } else {
                    self.emit_op(Op::Nil); // Default value if no initializer
                }

                // Get constant index for global name *after* potential initializer compilation
                if self.current_context().scope_depth == 0 {
                    global_name_index = Some(self.emit_const(id.name.clone())?);
                } else {
                    global_name_index = None;
                }

                // Define variable (marks local initialized or emits DefineGlobal)
                self.define_variable(global_name_index)?;
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
                self.visit_expr(expr)?;
                self.emit_op(Op::Pop); // Pop result of expression statement
                Ok(())
            }
            Stmt::Print(expr) => {
                self.visit_expr(expr)?;
                self.emit_op(Op::Print);
                Ok(())
            }
            Stmt::Return(expr) => self.visit_return_stmt(expr),
            Stmt::If(cond, then_branch, else_branch) => {
                self.visit_if_stmt(cond, *then_branch, else_branch.map(|b| *b))
            }
            Stmt::While(cond, body) => self.visit_while_stmt(cond, *body),
        }
    }

    fn visit_block(&mut self, block: Vec<Decl>) -> Self::Value {
        self.begin_scope();

        for decl in block {
            self.visit_declaration(decl)?;
        }

        self.end_scope()?;

        Ok(())
    }

    fn visit_if_stmt(&mut self, cond: Expr, stmt: Stmt, else_stmt: Option<Stmt>) -> Self::Value {
        // Compile condition
        self.visit_expr(cond)?;

        // Emit jump placeholder to skip 'then' if condition is false
        let then_jump = self.emit_jump(Op::JumpIfFalse(0)); // Jump to 'else' or after 'if'

        // Pop condition value if it was true (before executing 'then')
        self.emit_op(Op::Pop);
        // Compile 'then' branch
        self.visit_stmt(stmt)?;

        if let Some(else_stmt) = else_stmt {
            // Emit unconditional jump to skip 'else' after 'then' executes
            let else_jump = self.emit_jump(Op::Jump(0)); // Jump over 'else'

            // Patch the initial JumpIfFalse to land here (start of 'else')
            self.patch_jump(then_jump)?;

            // Pop condition value if it was false (before executing 'else')
            self.emit_op(Op::Pop);
            // Compile 'else' branch
            self.visit_stmt(else_stmt)?;

            // Patch the unconditional jump to land here (after 'else')
            self.patch_jump(else_jump)?;
        } else {
            // No 'else' branch
            // Patch the initial JumpIfFalse to land here (after 'then')
            self.patch_jump(then_jump)?;
            // Pop condition value if it was false (since there's no 'else' to execute)
            // Note: If the condition was true, the Pop *after* JumpIfFalse already handled it.
            // This Pop handles the case where the condition was false and we jumped.
            self.emit_op(Op::Pop);
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: Expr, body: Stmt) -> Self::Value {
        let loop_start = self.current_chunk_mut().code_len(); // Mark start of loop (condition)

        // Compile condition
        self.visit_expr(cond)?;

        // Jump out of loop if condition is false
        let exit_jump = self.emit_jump(Op::JumpIfFalse(0)); // Placeholder

        // Pop condition value if true (before executing body)
        self.emit_op(Op::Pop);
        // Compile loop body
        self.visit_stmt(body)?;

        // Emit loop instruction to jump back to condition
        self.emit_loop(loop_start)?;

        // Patch exit jump to land here (after the loop)
        self.patch_jump(exit_jump)?;
        // Pop condition value if it was false (when exiting loop)
        self.emit_op(Op::Pop);

        Ok(())
    }

    fn visit_func_declaration(
        &mut self,
        name: Identifier,
        parameters: Vec<Identifier>,
        body: Stmt,
    ) -> Self::Value {
        let arity = parameters.len();
        // TODO: Check arity limit (e.g., 255)

        // Start a new compilation context for the function
        self.begin_function_context(Some(name.name.clone()), arity);

        self.begin_scope(); // Function body starts a new scope implicitly

        for param in parameters {
            self.declare_variable(&param.name)?;
            self.mark_last_local_initialized();
        }

        // Compile the function body
        self.visit_stmt(body)?;

        let compiled_function = self.end_function_context()?;

        // --- Back in the outer context ---

        // Add the compiled function object as a constant in the outer chunk
        let const_index = self.emit_const(compiled_function)?;

        // Emit Op::Closure to create the runtime closure object
        self.emit_op(Op::Closure(const_index));

        // Define the variable (global or local) holding the closure
        // Need the name as a constant if global
        let global_name_index = if self.current_context().scope_depth == 0 {
            Some(self.emit_const(name.name.clone())?)
        } else {
            // If declared local, define_variable will mark it initialized
            None
        };
        // Define the variable (global or local) that holds the closure
        self.declare_variable(&name.name)?; // Declare in outer scope
        self.define_variable(global_name_index)?; // Define (marks local init or emits DefineGlobal)

        Ok(())
    }

    fn visit_return_stmt(&mut self, expr: Option<Expr>) -> Self::Value {
        match expr {
            Some(e) => self.visit_expr(e)?,
            None => self.emit_op(Op::Nil), // Implicit nil return value
        };

        self.emit_op(Op::Return);

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
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn grouping() {
        let input = "(123);";
        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn unary_negation() {
        let input = "-123;";
        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_op(Op::Negate);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_add() {
        let input = "123 + 456;";

        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_const(456);
        expected.add_op(Op::Const(1));
        expected.add_op(Op::Add);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_sub() {
        let input = "123 - 456;";

        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_const(456);
        expected.add_op(Op::Const(1));
        expected.add_op(Op::Subtract);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_mul() {
        let input = "123 * 456;";

        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_const(456);
        expected.add_op(Op::Const(1));
        expected.add_op(Op::Multiply);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn basic_div() {
        let input = "123 / 456;";

        let mut expected = Chunk::new();
        expected.add_const(123);
        expected.add_op(Op::Const(0));
        expected.add_const(456);
        expected.add_op(Op::Const(1));
        expected.add_op(Op::Divide);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }

    #[test]
    fn booleans() {
        let input = "!true;";

        let mut expected = Chunk::new();
        expected.add_op(Op::True);
        expected.add_op(Op::Not);
        expected.add_op(Op::Pop);

        let result = Compiler::new_from_source(input).unwrap().compile().unwrap();

        assert_eq!(result.chunk().disassemble(), expected.disassemble());
    }
}
