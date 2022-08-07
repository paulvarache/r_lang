use std::{cell::RefCell, rc::Rc};

use crate::environment::*;
use crate::lox_error::*;
use crate::ast::*;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::value::Value;

impl LoxError {
    pub fn interpreter(token: &Token, message: String) -> Self {
        let err = LoxError::Interpreter {
            token: token.clone(),
            message,
        };
        err.report();
        err
    }
}

pub struct Interpreter {
    environment: RefCell<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: RefCell::new(Rc::new(RefCell::new(Environment::new()))),
        }
    }
    pub fn execute(&self, stmt: &Stmt) -> Result<(), LoxError> {
        stmt.accept(self)
    }
    pub fn evaluate(&self, expr: &Expr) -> Result<Value, LoxError> {
        expr.accept(self)
    }
    fn execute_block(&self, statements: &[Stmt], e: Environment) -> LoxResult<()> {
        let previous = self.environment.replace(Rc::new(RefCell::new(e)));

        let result = statements
            .iter()
            .try_for_each(|statement| self.execute(statement));

        self.environment.replace(previous);

        result
    }
    fn is_thruthy(val: Value) -> bool {
        !matches!(val, Value::Bool(false) | Value::Nil)
    }
    fn solve_number_binary(
        &self,
        solver: fn(f64, f64) -> Value,
        left: Value,
        right: Value,
        expr: &BinaryExpr,
    ) -> Result<Value, LoxError> {
        match (left, right) {
            (Value::Number(l), Value::Number(r)) => Ok(solver(l, r)),
            _ => Err(LoxError::interpreter(
                &expr.operator,
                "Operands must be numbers".to_string(),
            )),
        }
    }
}

impl StmtVisitor<()> for Interpreter {
    fn visit_expression_stmt(&self, stmt: &ExpressionStmt) -> Result<(), LoxError> {
        self.evaluate(&stmt.expression).and(Ok(()))
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<(), LoxError> {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &crate::ast::VarStmt) -> Result<(), LoxError> {
        let value = self.evaluate(&stmt.initializer)?;
        self.environment
            .borrow()
            .borrow_mut()
            .define(&stmt.name.as_string(), value);
        Ok(())
    }

    fn visit_block_stmt(&self, stmt: &crate::ast::BlockStmt) -> Result<(), LoxError> {
        let e = Environment::new_with_enclosing(self.environment.borrow().clone());
        self.execute_block(&stmt.statements, e)
    }
}

impl ExprVisitor<Value> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<Value, LoxError> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => {
                self.solve_number_binary(|l, r| Value::Number(l - r), left, right, expr)
            }
            TokenType::Slash => {
                self.solve_number_binary(|l, r| Value::Number(l / r), left, right, expr)
            }
            TokenType::Star => {
                self.solve_number_binary(|l, r| Value::Number(l * r), left, right, expr)
            }
            TokenType::Plus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                _ => Err(LoxError::interpreter(
                    &expr.operator,
                    "Operands must be numbers or strings".to_string(),
                )),
            },
            TokenType::Less => {
                self.solve_number_binary(|l, r| Value::Bool(l < r), left, right, expr)
            }
            TokenType::LessEqual => {
                self.solve_number_binary(|l, r| Value::Bool(l <= r), left, right, expr)
            }
            TokenType::Greater => {
                self.solve_number_binary(|l, r| Value::Bool(l > r), left, right, expr)
            }
            TokenType::GreaterEqual => {
                self.solve_number_binary(|l, r| Value::Bool(l >= r), left, right, expr)
            }
            TokenType::EqualEqual => {
                self.solve_number_binary(|l, r| Value::Bool(l == r), left, right, expr)
            }
            _ => Err(LoxError::interpreter(
                &expr.operator,
                format!("invalid binary operator {}", expr.operator.ttype),
            )),
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<Value, LoxError> {
        self.evaluate(&expr.expression)
    }

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<Value, LoxError> {
        Ok(expr.value.clone())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<Value, LoxError> {
        let right = self.evaluate(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => match right {
                Value::Number(_) => Ok(-right),
                _ => Err(LoxError::interpreter(
                    &expr.operator,
                    format!(
                        "cannot use operator {} on type {}",
                        expr.operator.ttype, right
                    ),
                )),
            },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::is_thruthy(right))),
            _ => Err(LoxError::interpreter(
                &expr.operator,
                format!("invalid unary operator {}", expr.operator.ttype),
            )),
        }
    }

    fn visit_var_expr(&self, expr: &crate::ast::VarExpr) -> LoxResult<Value> {
        self.environment.borrow().borrow().get(&expr.name)
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
