use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::Expr;
use crate::ast::ExprVisitor;
use crate::ast::Stmt;
use crate::ast::StmtVisitor;
use crate::lox_error::LoxResult;

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    fn resolve_all(&self, stmts: Vec<Rc<Stmt>>) -> LoxResult<()> {
        stmts.iter().try_for_each(|s| self.resolve(s))?;
        Ok(())
    }
    fn resolve(&self, stmt: &Stmt) -> LoxResult<()> {
        stmt.accept(self)
    }
    fn resolve_expr(&self, expr: &Expr) -> LoxResult<()> {
        expr.accept(self)
    }
    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}

impl ExprVisitor<()> for Resolver {
    fn visit_assign_expr(&self,expr: &crate::ast::AssignExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_binary_expr(&self,expr: &crate::ast::BinaryExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_call_expr(&self,expr: &crate::ast::CallExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_grouping_expr(&self,expr: &crate::ast::GroupingExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_literal_expr(&self,expr: &crate::ast::LiteralExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_logical_expr(&self,expr: &crate::ast::LogicalExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_unary_expr(&self,expr: &crate::ast::UnaryExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_var_expr(&self,expr: &crate::ast::VarExpr) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }
}

impl StmtVisitor<()> for Resolver {
    fn visit_block_stmt(&self,stmt: &crate::ast::BlockStmt) -> Result<(),crate::lox_error::LoxError>  {
        self.begin_scope();
        self.resolve_all(stmt.statements.to_vec())?;
        self.end_scope();
        Ok(())
    }

    fn visit_expression_stmt(&self,stmt: &crate::ast::ExpressionStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_function_stmt(&self,stmt: &crate::ast::FunctionStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_if_stmt(&self,stmt: &crate::ast::IfStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_print_stmt(&self,stmt: &crate::ast::PrintStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_return_stmt(&self,stmt: &crate::ast::ReturnStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_var_stmt(&self,stmt: &crate::ast::VarStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }

    fn visit_while_stmt(&self,stmt: &crate::ast::WhileStmt) -> Result<(),crate::lox_error::LoxError>  {
        todo!()
    }
}
