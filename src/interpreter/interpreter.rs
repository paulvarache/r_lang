use crate::{
    ast::{BinaryExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, UnaryExpr},
    scanner::{value::Value, token_type::TokenType},
    LoxError,
};

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(&self, expr: &Expr) -> Result<Value, LoxError> {
        expr.accept(self)
    }
    fn is_thruthy(val: Value) -> bool {
        match val {
            Value::Bool(false) | Value::Nil => false,
            _ => true,
        }
    }
}

impl ExprVisitor<Value> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<Value, LoxError> {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => Ok(left - right),
            TokenType::Slash => Ok(left / right),
            TokenType::Star => Ok(left * right),
            TokenType::Plus => Ok(left + right),
            TokenType::Less => Ok(Value::Bool(left < right)),
            TokenType::LessEqual => Ok(Value::Bool(left <= right)),
            TokenType::Greater => Ok(Value::Bool(left > right)),
            TokenType::GreaterEqual => Ok(Value::Bool(left >= right)),
            TokenType::EqualEqual => Ok(Value::Bool(left == right)),
            _ => Err(LoxError::new(
                expr.operator.line,
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
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(LoxError::new(
                    expr.operator.line,
                    format!(
                        "cannot use operator {} on type {}",
                        expr.operator.ttype, right
                    ),
                )),
            },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::is_thruthy(right))),
            _ => Err(LoxError::new(
                expr.operator.line,
                format!("invalid unary operator {}", expr.operator.ttype),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::token::Token;

    use super::*;

    #[test]
    fn test_unary_minus() {
        let terp = Interpreter {};
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
        let terp = Interpreter {};
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
