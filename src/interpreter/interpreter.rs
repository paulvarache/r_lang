use std::cell::RefCell;

use crate::{
    ast::{BinaryExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, UnaryExpr, Stmt, StmtVisitor, ExpressionStmt, PrintStmt},
    lox_error::LoxError,
    scanner::{token_type::TokenType, value::Value},
};

use super::environment::Environment;

impl LoxError {
    pub fn interpreter(expr: Expr, message: String) -> Self {
        let err = LoxError::Interpreter { expr, message };
        err.report();
        err
    }
}

pub struct Interpreter {
    environment: RefCell<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self { environment: RefCell::new(Environment::new()) }
    }
    pub fn evaluate(&self, stmt: &Stmt) -> Result<(), LoxError> {
        stmt.accept(self)
    }
    pub fn evaluate_expression(&self, expr: &Expr) -> Result<Value, LoxError> {
        expr.accept(self)
    }
    fn is_thruthy(val: Value) -> bool {
        match val {
            Value::Bool(false) | Value::Nil => false,
            _ => true,
        }
    }
    fn solve_number_binary(&self, solver: fn(f64, f64) -> Value, left: Value, right: Value, expr: &BinaryExpr) -> Result<Value, LoxError> {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Ok(solver(l, r)),
            _ => Err(LoxError::interpreter(
                Expr::Binary(expr.clone()),
                format!("Operands must be numbers"),
            )),
        }
    }
}

impl StmtVisitor<()> for Interpreter {
    fn visit_expression_stmt(&self,stmt: &ExpressionStmt) -> Result<(),LoxError>  {
        self.evaluate_expression(&stmt.expression).and(Ok(()))
    }

    fn visit_print_stmt(&self,stmt: &PrintStmt) -> Result<(),LoxError>  {
        let value = self.evaluate_expression(&stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&self,stmt: &crate::ast::VarStmt) -> Result<(),LoxError>  {
        let value = self.evaluate_expression(&stmt.initializer)?;
        self.environment.borrow_mut().define(stmt.name.lexeme.clone(), value);
        Ok(())
    }
}

impl ExprVisitor<Value> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<Value, LoxError> {
        let left = self.evaluate_expression(&expr.left)?;
        let right = self.evaluate_expression(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => self.solve_number_binary(|l, r| Value::Number(l - r), left, right, expr),
            TokenType::Slash => self.solve_number_binary(|l, r| Value::Number(l / r), left, right, expr),
            TokenType::Star => self.solve_number_binary(|l, r| Value::Number(l * r), left, right, expr),
            TokenType::Plus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => Err(LoxError::interpreter(
                    Expr::Binary(expr.clone()),
                    format!("Operands must be numbers or strings"),
                )),
            }
            TokenType::Less => self.solve_number_binary(|l, r| Value::Bool(l < r), left, right, expr),
            TokenType::LessEqual => self.solve_number_binary(|l, r| Value::Bool(l <= r), left, right, expr),
            TokenType::Greater => self.solve_number_binary(|l, r| Value::Bool(l > r), left, right, expr),
            TokenType::GreaterEqual => self.solve_number_binary(|l, r| Value::Bool(l >= r), left, right, expr),
            TokenType::EqualEqual => self.solve_number_binary(|l, r| Value::Bool(l == r), left, right, expr),
            _ => Err(LoxError::interpreter(
                Expr::Binary(expr.clone()),
                format!("invalid binary operator {}", expr.operator.ttype),
            )),
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<Value, LoxError> {
        self.evaluate_expression(&expr.expression)
    }

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<Value, LoxError> {
        Ok(expr.value.clone())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<Value, LoxError> {
        let right = self.evaluate_expression(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => match right {
                Value::Number(_) => Ok(-right),
                _ => Err(LoxError::interpreter(
                    Expr::Unary(expr.clone()),
                    format!(
                        "cannot use operator {} on type {}",
                        expr.operator.ttype, right
                    ),
                )),
            },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::is_thruthy(right))),
            _ => Err(LoxError::interpreter(
                Expr::Unary(expr.clone()),
                format!("invalid unary operator {}", expr.operator.ttype),
            )),
        }
    }

    fn visit_var_expr(&self,expr: &crate::ast::VarExpr) -> Result<Value,LoxError>  {
        self.environment.borrow_mut().get(expr.name.lexeme.clone())
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::token::Token;

    use super::*;

    #[test]
    fn test_unary_minus() {
        let terp = Interpreter::new();
        let unary_expr = UnaryExpr {
            operator: Token::new(TokenType::Minus, "-".to_string(), 0, None),
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Value::Number(2.0),
            })),
        };
        let result = terp.visit_unary_expr(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Number(-2.0)));
    }
    #[test]
    fn test_unary_not() {
        let terp = Interpreter::new();
        let unary_expr = UnaryExpr {
            operator: Token::new(TokenType::Bang, "-".to_string(), 0, None),
            right: Box::new(Expr::Literal(LiteralExpr {
                value: Value::Bool(false),
            })),
        };
        let result = terp.visit_unary_expr(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Bool(true)));
    }
}
