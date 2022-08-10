use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
use crate::lox_error::LoxError;
use crate::lox_error::LoxResult;
use crate::lox_error::ParserError;
use crate::lox_error::ParserErrorCode;
use crate::scanner::token::Token;

pub struct Resolver {
    scopes: RefCell<Vec<RefCell<HashMap<String, bool>>>>,
    locals: RefCell<HashMap<usize, usize>>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            scopes: RefCell::new(Vec::new()),
            locals: RefCell::new(HashMap::new()),
        }
    }
    pub fn get_depth(&self, id: usize) -> Option<usize> {
        let b = self.locals.borrow();
        b.get(&id).map(|d| d.clone())
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
    fn declare(&self, token: &Token) {
        if let Some(scope) = self.scopes.borrow().last() {
            scope.borrow_mut().insert(token.lexeme.clone(), false);
        }
    }
    fn define(&self, token: &Token) {
        if let Some(mut scope) = self.scopes.borrow().last() {
            scope.borrow_mut().insert(token.lexeme.clone(), true);
        }
    }
    fn resolve_local(&self, expr_id: usize, name: &Token) {
        let scopes = self.scopes.borrow();
        let count = scopes.len();
        let name = name.lexeme.clone();
        for (i, scope) in scopes.iter().enumerate().rev() {
            if scope.borrow().contains_key(&name) {
                self.locals.borrow_mut().insert(expr_id, count - 1 - i);
                return;
            }
        }
    }
    fn resolve_function(&self, stmt: &FunctionStmt) {
        self.begin_scope();
        for token in stmt.params.iter() {
            self.declare(&token);
            self.define(&token);
        }
        self.end_scope();
    }
    fn error(&self, code: ParserErrorCode, token: &Token) -> LoxError {
        LoxError::Parser(ParserError {
            code,
            token: token.clone(),
            next_token: None,
        })
    }
}

impl ExprVisitor<()> for Resolver {
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

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<(), LoxError> {
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
                return Err(self.error(ParserErrorCode::InitVarWithUnassignedVar, &expr.name));
            }
        }
        self.resolve_local(expr.id, &expr.name);
        Ok(())
    }
}

impl StmtVisitor<()> for Resolver {
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
        self.declare(&stmt.name);
        self.define(&stmt.name);
        self.resolve_function(stmt);
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
        if let Some(expr) = &stmt.expression {
            expr.accept(self)?;
        }
        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &ast::VarStmt) -> Result<(), LoxError> {
        self.declare(&stmt.name);
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
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::Expr;
    use crate::ast::Stmt;
    use crate::lox_error::LoxError;
    use crate::lox_error::ParserError;
    use crate::lox_error::ParserErrorCode;
    use crate::scanner::token::Span;
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
        let resolver = Resolver::new();

        let outer_init = Some(Rc::new(Expr::new_literal(number(4.0), span())));
        let a_decl = Rc::new(Stmt::new_var(name("a"), outer_init, span()));

        let a_read = Rc::new(Expr::new_var(name("a"), span()));

        let a_read_id = a_read.id();

        let a_print = Rc::new(Stmt::new_print(a_read, span()));

        let a_read_2 = Rc::new(Expr::new_var(name("a"), span()));

        let a_read_2_id = a_read_2.id();

        let a_print_2 = Rc::new(Stmt::new_print(a_read_2, span()));

        let block_2 = Rc::new(Stmt::new_block(Rc::new(vec![a_print_2]), span()));

        let ast = Stmt::new_block(Rc::new(vec![a_decl, a_print, block_2]), span());

        let result = ast.accept(&resolver);

        assert!(result.is_ok());

        println!("{:?}", resolver.get_depth(a_read_id));
        println!("{:?}", resolver.get_depth(a_read_2_id));
    }
    #[test]
    fn read_self_before_define() {
        let resolver = Resolver::new();

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
        assert!(matches!(result.expect_err(""), LoxError::Parser(ParserError { code: ParserErrorCode::InitVarWithUnassignedVar, token: _, next_token: _ })))
    }
}
