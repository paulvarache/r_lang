use r_ast::define_ast;

use crate::scanner::token::{Token};
use crate::LoxError;
use crate::scanner::value::Value;

define_ast!(Expr(
    Binary: { left: Expr, operator: Token, right: Expr },
    Grouping: { expression: Expr },
    Literal: { value: Value },
    Unary: { operator: Token, right: Expr },
));

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&self, expr: &Expr) -> Result<String, LoxError> {
        expr.accept(self)
    }
    fn parenthesize(&self, name: &String, exprs: &[&Box<Expr>]) -> Result<String, LoxError> {
        let mut builder = format!("({name}");

        for expr in exprs {
            builder = format!("{builder} {}", expr.accept(self)?);
        }
        builder = format!("{builder})");

        Ok(builder)
    }

}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary_expr(&self,expr: &BinaryExpr) -> Result<String,LoxError>  {
        self.parenthesize(&expr.operator.lexeme, &[&expr.left, &expr.right])
    }
    fn visit_grouping_expr(&self,expr: &GroupingExpr) -> Result<String,LoxError>  {
        self.parenthesize(&"group".to_string(), &[&expr.expression])
    }

    fn visit_literal_expr(&self,expr: &LiteralExpr) -> Result<String,LoxError>  {
        match &expr.value {
            Value::Number(n) => Ok(format!("{}", n)),
            Value::String(s) => Ok(s.to_string()),
            Value::Bool(b) => Ok(format!("{}", b)),
            Value::Nil => Ok("nil".to_string()),
        }
    }

    fn visit_unary_expr(&self,expr: &UnaryExpr) -> Result<String,LoxError>  {
        self.parenthesize(&expr.operator.lexeme, &[&expr.right])
    }
}