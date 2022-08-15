use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::thread::current;

use crate::ast;
use crate::ast::AssignExpr;
use crate::ast::CallExpr;
use crate::ast::ExprVisitor;
use crate::ast::ExpressionStmt;
use crate::ast::FunctionStmt;
use crate::ast::GroupingExpr;
use crate::ast::LiteralExpr;
use crate::ast::LogicalExpr;
use crate::ast::Stmt;
use crate::ast::StmtVisitor;
use crate::class::CONSTRUCTOR_NAME;
use crate::interpreter::Interpreter;
use crate::error::InterpreterErrorCode;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::scanner::token::Token;

enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}
enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct Resolver<'a> {
    interpreter: &'a Interpreter,
    scopes: RefCell<Vec<RefCell<HashMap<String, bool>>>>,
    current_function: RefCell<FunctionType>,
    current_class: RefCell<ClassType>,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a Interpreter) -> Self {
        Self {
            interpreter,
            scopes: RefCell::new(Vec::new()),
            current_function: RefCell::new(FunctionType::None),
            current_class: RefCell::new(ClassType::None),
        }
    }
    pub fn resolve(&self, stmt: &Stmt) -> LoxResult<()> {
        stmt.accept(self)
    }
    fn resolve_all(&self, stmts: Vec<Rc<Stmt>>) -> LoxResult<()> {
        stmts.iter().try_for_each(|s| s.accept(self))?;
        Ok(())
    }
    fn begin_scope(&self) {
        self.scopes.borrow_mut().push(RefCell::new(HashMap::new()));
    }
    fn end_scope(&self) {
        self.scopes.borrow_mut().pop();
    }
    fn declare(&self, token: &Token) -> LoxResult<()> {
        if let Some(scope) = self.scopes.borrow().last() {
            if scope.borrow().contains_key(&token.lexeme) {
                return Err(self
                    .interpreter
                    .error(token.span, InterpreterErrorCode::RedefiningLocalVar));
            }
            scope.borrow_mut().insert(token.lexeme.clone(), false);
        }
        Ok(())
    }
    fn define(&self, token: &Token) {
        self.define_str(token.lexeme.clone().as_str())
    }
    fn define_str(&self, name: &str) {
        if let Some(scope) = self.scopes.borrow().last() {
            scope.borrow_mut().insert(name.to_string(), true);
        }
    }
    fn resolve_local(&self, expr_id: usize, name: &Token) {
        self.resolve_local_str(expr_id, &name.lexeme.clone())
    }
    fn resolve_local_str(&self, expr_id: usize, name: &str) {
        let scopes = self.scopes.borrow();
        let count = scopes.len();
        for (i, scope) in scopes.iter().enumerate().rev() {
            if scope.borrow().contains_key(name) {
                self.interpreter
                    .locals
                    .borrow_mut()
                    .insert(expr_id, count - 1 - i);
                return;
            }
        }
    }
    fn resolve_function(&self, stmt: &FunctionStmt, t: FunctionType) -> LoxResult<()> {
        let enclosing_function = self.current_function.replace(t);
        self.begin_scope();
        for token in stmt.params.iter() {
            self.declare(token)?;
            self.define(token);
        }
        self.resolve_all(stmt.body.to_vec())?;
        self.end_scope();
        self.current_function.replace(enclosing_function);
        Ok(())
    }
}

impl<'a> ExprVisitor<()> for Resolver<'a> {
    fn visit_assign_expr(&self, expr: &AssignExpr) -> Result<(), LoxError> {
        expr.value.accept(self)?;
        self.resolve_local(expr.id, &expr.name);
        Ok(())
    }

    fn visit_binary_expr(&self, expr: &ast::BinaryExpr) -> Result<(), LoxError> {
        expr.left.accept(self)?;
        expr.right.accept(self)
    }

    fn visit_call_expr(&self, expr: &CallExpr) -> Result<(), LoxError> {
        expr.callee.accept(self)?;
        for arg in expr.arguments.iter() {
            arg.accept(self)?;
        }
        Ok(())
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<(), LoxError> {
        expr.expression.accept(self)
    }

    fn visit_literal_expr(&self, _expr: &LiteralExpr) -> Result<(), LoxError> {
        Ok(())
    }

    fn visit_logical_expr(&self, expr: &LogicalExpr) -> Result<(), LoxError> {
        expr.left.accept(self)?;
        expr.right.accept(self)
    }

    fn visit_unary_expr(&self, expr: &ast::UnaryExpr) -> Result<(), LoxError> {
        expr.right.accept(self)
    }

    fn visit_var_expr(&self, expr: &ast::VarExpr) -> Result<(), LoxError> {
        if let Some(last) = self.scopes.borrow().last() {
            if !last.borrow().get(&expr.name.lexeme).unwrap_or(&true) {
                return Err(self
                    .interpreter
                    .error(expr.span, InterpreterErrorCode::InitVarWithUnassignedVar));
            }
        }
        self.resolve_local(expr.id, &expr.name);
        Ok(())
    }

    fn visit_get_expr(&self, expr: &ast::GetExpr) -> Result<(), LoxError> {
        expr.object.accept(self)
    }

    fn visit_set_expr(&self, expr: &ast::SetExpr) -> Result<(), LoxError> {
        expr.value.accept(self)?;
        expr.object.accept(self)
    }

    fn visit_this_expr(&self, expr: &ast::ThisExpr) -> Result<(), LoxError> {
        self.resolve_local_str(expr.id, "this");
        Ok(())
    }

    fn visit_super_expr(&self, expr: &ast::SuperExpr) -> Result<(), LoxError> {
        if matches!(*self.current_class.borrow(), ClassType::None) {
            return Err(self.interpreter.error(expr.span, InterpreterErrorCode::SuperOutsideClass));
        } else if matches!(*self.current_class.borrow(), ClassType::Class) {
            return Err(self.interpreter.error(expr.span, InterpreterErrorCode::SuperOutsideSuperclass));
        }
        self.resolve_local_str(expr.id, "super");
        Ok(())
    }
}

impl<'a> StmtVisitor<()> for Resolver<'a> {
    fn visit_block_stmt(&self, stmt: &ast::BlockStmt) -> Result<(), LoxError> {
        self.begin_scope();
        self.resolve_all(stmt.statements.to_vec())?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression_stmt(&self, stmt: &ExpressionStmt) -> Result<(), LoxError> {
        stmt.expression.accept(self)
    }

    fn visit_function_stmt(&self, stmt: &ast::FunctionStmt) -> Result<(), LoxError> {
        self.declare(&stmt.name)?;
        self.define(&stmt.name);
        self.resolve_function(stmt, FunctionType::Function)?;
        Ok(())
    }

    fn visit_if_stmt(&self, stmt: &ast::IfStmt) -> Result<(), LoxError> {
        stmt.predicate.accept(self)?;
        stmt.then_branch.accept(self)?;
        if let Some(else_branch) = &stmt.else_branch {
            else_branch.accept(self)?;
        }
        Ok(())
    }

    fn visit_print_stmt(&self, stmt: &ast::PrintStmt) -> Result<(), LoxError> {
        stmt.expression.accept(self)
    }

    fn visit_return_stmt(&self, stmt: &ast::ReturnStmt) -> Result<(), LoxError> {
        let current_func = self.current_function.borrow();
        if matches!(*current_func, FunctionType::None) {
            return Err(self
                .interpreter
                .error(stmt.span, InterpreterErrorCode::ReturnOutsideFunction));
        }
        if let Some(expr) = &stmt.expression {
            if matches!(*current_func, FunctionType::Initializer) {
                return Err(self
                    .interpreter
                    .error(stmt.span, InterpreterErrorCode::ReturnInsideConstructor));
            }
            expr.accept(self)?;
        }
        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &ast::VarStmt) -> Result<(), LoxError> {
        self.declare(&stmt.name)?;
        if let Some(init) = &stmt.initializer {
            init.accept(self)?;
        }
        self.define(&stmt.name);
        Ok(())
    }

    fn visit_while_stmt(&self, stmt: &ast::WhileStmt) -> Result<(), LoxError> {
        stmt.predicate.accept(self)?;
        stmt.body.accept(self)
    }

    fn visit_class_stmt(&self, stmt: &ast::ClassStmt) -> Result<(), LoxError> {
        let enclosing_class_type = self.current_class.replace(ClassType::Class);
        self.declare(&stmt.name)?;
        self.define(&stmt.name);
        let mut has_superclass = false;
        if let Some(superclass) = &stmt.superclass {
            if stmt.name.lexeme == superclass.name.lexeme {
                return Err(self.interpreter.error(
                    superclass.name.span,
                    InterpreterErrorCode::ParentClassIsChildClass,
                ));
            }
            self.current_class.replace(ClassType::Subclass);
            has_superclass = true;
            self.begin_scope();
            self.define_str("super");

            self.visit_var_expr(&superclass)?;
        }
        self.begin_scope();
        self.define_str("this");
        for f in stmt.methods.as_ref() {
            self.resolve_function(
                &f,
                if f.name.lexeme == CONSTRUCTOR_NAME {
                    FunctionType::Initializer
                } else {
                    FunctionType::Method
                },
            )?;
        }
        self.end_scope();
        
        if has_superclass {
            self.end_scope();
        }
        self.current_class.replace(enclosing_class_type);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::Expr;
    use crate::ast::Stmt;
    use crate::interpreter::Interpreter;
    use crate::error::InterpreterError;
    use crate::error::InterpreterErrorCode;
    use crate::error::LoxError;
    use crate::scanner::span::Span;
    use crate::scanner::token::Token;
    use crate::scanner::token_type::TokenType;
    use crate::scanner::value::Value;

    use super::Resolver;

    fn span() -> Span {
        Span::new(0, 0, 0, 0)
    }

    fn name(name: &str) -> Token {
        Token::new(TokenType::Identifier, name.to_string(), span(), None)
    }

    fn number(n: f64) -> Value {
        Value::Number(n)
    }

    #[test]
    fn scope() {
        let interpreter = Interpreter::new();
        let resolver = Resolver::new(&interpreter);

        let outer_init = Some(Rc::new(Expr::new_literal(number(4.0), span())));
        let a_decl = Rc::new(Stmt::new_var(name("a"), outer_init, span()));

        let a_read = Rc::new(Expr::new_var(name("a"), span()));

        let a_print = Rc::new(Stmt::new_print(a_read, span()));

        let a_read_2 = Rc::new(Expr::new_var(name("a"), span()));

        let a_print_2 = Rc::new(Stmt::new_print(a_read_2, span()));

        let block_2 = Rc::new(Stmt::new_block(Rc::new(vec![a_print_2]), span()));

        let ast = Stmt::new_block(Rc::new(vec![a_decl, a_print, block_2]), span());

        let result = ast.accept(&resolver);

        assert!(result.is_ok());
    }
    #[test]
    fn read_self_before_define() {
        let interpreter = Interpreter::new();
        let resolver = Resolver::new(&interpreter);

        let a_read = Rc::new(Expr::new_var(name("a"), span()));

        let block = Rc::new(Stmt::new_block(
            Rc::new(vec![Rc::new(Stmt::new_var(
                name("a"),
                Some(a_read),
                span(),
            ))]),
            span(),
        ));

        let ast = Stmt::new_block(
            Rc::new(vec![Rc::new(Stmt::new_var(name("a"), None, span())), block]),
            span(),
        );

        let result = ast.accept(&resolver);

        assert!(result.is_err());
        assert!(matches!(
            result.expect_err(""),
            LoxError::Interpreter(InterpreterError {
                code: InterpreterErrorCode::InitVarWithUnassignedVar,
                span: _
            })
        ))
    }
}
