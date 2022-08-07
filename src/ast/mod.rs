use r_ast::define_ast;

use crate::lox_error::LoxError;
use crate::scanner::token::Token;
use crate::scanner::value::Value;

define_ast!(Expr(
    Binary: { left: Expr, operator: Token, right: Expr },
    Grouping: { expression: Expr },
    Literal: { value: Value },
    Unary: { operator: Token, right: Expr },
    Var: { name: Token },
));

define_ast!(Stmt(
    Block: { statements: Vec<Stmt> },
    Expression: { expression: Expr },
    Print: { expression: Expr },
    Var: { name: Token, initializer: Expr },
));

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&self, expr: &Stmt) -> Result<String, LoxError> {
        expr.accept(self)
    }
    fn parenthesize(&self, name: &String, exprs: &[&Expr]) -> Result<String, LoxError> {
        let mut builder = format!("({name}");

        for expr in exprs {
            builder = format!("{builder} {}", expr.accept(self)?);
        }
        builder = format!("{builder})");

        Ok(builder)
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<String, LoxError> {
        self.parenthesize(&expr.operator.lexeme, &[&expr.left, &expr.right])
    }
    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<String, LoxError> {
        self.parenthesize(&"group".to_string(), &[&expr.expression])
    }

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<String, LoxError> {
        match &expr.value {
            Value::Number(n) => Ok(format!("{}", n)),
            Value::String(s) => Ok(s.to_string()),
            Value::Bool(b) => Ok(format!("{}", b)),
            Value::Nil => Ok("nil".to_string()),
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<String, LoxError> {
        self.parenthesize(&expr.operator.lexeme, &[&expr.right])
    }

    fn visit_var_expr(&self, expr: &VarExpr) -> Result<String, LoxError> {
        Ok(format!("{}", expr.name))
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_expression_stmt(&self, stmt: &ExpressionStmt) -> Result<String, LoxError> {
        stmt.expression.accept(self)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<String, LoxError> {
        self.parenthesize(&"print".to_string(), &[&stmt.expression])
    }

    fn visit_var_stmt(&self, stmt: &VarStmt) -> Result<String, LoxError> {
        self.parenthesize(&"var".to_string(), &[&stmt.initializer])
    }

    fn visit_block_stmt(&self, stmt: &BlockStmt) -> Result<String, LoxError> {
        stmt.statements
            .iter()
            .try_fold(String::new(), |acc, stmt| Ok(acc + &stmt.accept(self)?))
    }
}
