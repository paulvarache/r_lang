use std::ops::Deref;
use std::{cell::RefCell, rc::Rc};

use crate::ast::*;
use crate::callable::LoxCallable;
use crate::environment::*;
use crate::function::LoxFunction;
use crate::lox_error::*;
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
    globals: Rc<RefCell<Environment>>,
    environment: RefCell<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: Rc::new(RefCell::new(Environment::new())),
            environment: RefCell::new(Rc::new(RefCell::new(Environment::new()))),
        }
    }
    pub fn execute(&self, stmt: &Stmt) -> Result<(), LoxError> {
        stmt.accept(self)
    }
    pub fn evaluate(&self, expr: &Expr) -> Result<Value, LoxError> {
        expr.accept(self)
    }
    fn execute_block(&self, statements: &Rc<Vec<Rc<Stmt>>>, e: Environment) -> LoxResult<()> {
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
    fn visit_block_stmt(&self, stmt: &crate::ast::BlockStmt) -> Result<(), LoxError> {
        let e = Environment::new_with_enclosing(self.environment.borrow().clone());
        self.execute_block(&stmt.statements, e)
    }

    fn visit_expression_stmt(&self, stmt: &ExpressionStmt) -> Result<(), LoxError> {
        self.evaluate(&stmt.expression).and(Ok(()))
    }

    fn visit_if_stmt(&self, stmt: &IfStmt) -> Result<(), LoxError> {
        let pred = Interpreter::is_thruthy(stmt.predicate.accept(self)?);
        match pred {
            true => self.execute(&stmt.then_branch),
            false => {
                if let Some(else_branch) = &stmt.else_branch {
                    return self.execute(else_branch);
                }
                Ok(())
            }
        }
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<(), LoxError> {
        let value = self.evaluate(&stmt.expression)?;
        let formatted = match value {
            Value::Nil => "nil".to_string(),
            Value::Bool(b) => format!("{}", b),
            Value::String(s) => format!("\"{}\"", s),
            Value::Number(n) => format!("{}", n),
            Value::Func(_) => "func".to_string(),
        };
        println!("{}", formatted);
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

    fn visit_while_stmt(&self, stmt: &WhileStmt) -> Result<(), LoxError> {
        while Interpreter::is_thruthy(self.evaluate(&stmt.predicate)?) {
            self.execute(&stmt.body)?;
        }
        Ok(())
    }

    fn visit_function_stmt(&self,stmt: &FunctionStmt) -> Result<(),LoxError>  {
        let f = LoxFunction::new(stmt, self.environment.borrow().deref());

        self.environment.borrow().borrow_mut().define(&stmt.name.as_string(), Value::Func(Rc::new(f)));
        Ok(())
    }
}

impl ExprVisitor<Value> for Interpreter {
    fn visit_assign_expr(&self, expr: &AssignExpr) -> Result<Value, LoxError> {
        let value = self.evaluate(&expr.value)?;
        self.environment
            .borrow()
            .borrow_mut()
            .assign(&expr.name, value)?;
        self.environment.borrow().borrow().get(&expr.name)
    }

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

    fn visit_logical_expr(&self, expr: &LogicalExpr) -> Result<Value, LoxError> {
        let left = self.evaluate(&expr.left)?;

        match (&expr.operator.ttype, Interpreter::is_thruthy(left.clone())) {
            (TokenType::And, true) | (TokenType::Or, false) => {
                let right = self.evaluate(&expr.right)?;
                Ok(right)
            }
            (TokenType::And, false) | (TokenType::Or, true) => Ok(left),
            _ => Err(LoxError::interpreter(
                &expr.operator,
                "expected AND or OR".to_string(),
            )),
        }
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

    fn visit_call_expr(&self, expr: &CallExpr) -> Result<Value, LoxError> {
        let callee = self.evaluate(&expr.callee)?;

        let mut args = Vec::new();

        for a in expr.arguments.iter() {
            args.push(self.evaluate(a)?);
        }

        match callee {
            Value::Func(f) => f.call(self, args),
            _ => Ok(Value::Nil)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::token::{Span, Token};

    use super::*;

    fn number(n: f64) -> Value {
        Value::Number(n)
    }
    fn string(s: &str) -> Value {
        Value::String(s.to_string())
    }
    fn bool(b: bool) -> Value {
        Value::Bool(b)
    }

    fn token(ttype: TokenType) -> Token {
        Token::new(ttype, "".to_string(), 0, Span::new(0, 0, 0, 0), None)
    }

    fn token_literal(ttype: TokenType, literal: Value) -> Token {
        Token::new(
            ttype,
            "".to_string(),
            0,
            Span::new(0, 0, 0, 0),
            Some(literal),
        )
    }

    fn literal(value: Value) -> Expr {
        Expr::Literal(LiteralExpr { value })
    }

    #[test]
    fn test_unary_minus() {
        let terp = Interpreter::new();

        let unary_expr = UnaryExpr {
            operator: token(TokenType::Minus),
            right: Rc::new(literal(number(2.0))),
        };
        let result = terp.visit_unary_expr(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Number(-2.0)));
    }
    #[test]
    fn test_unary_not() {
        let terp = Interpreter::new();
        let unary_expr = UnaryExpr {
            operator: token(TokenType::Bang),
            right: Rc::new(literal(bool(false))),
        };
        let result = terp.visit_unary_expr(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Bool(true)));
    }

    #[test]
    fn if_then() {
        let terp = Interpreter::new();
        let var_stmt = VarStmt {
            name: token_literal(TokenType::Identifier, string("hi")),
            initializer: Rc::new(literal(string("hi"))),
        };
        let expr = AssignExpr {
            name: token_literal(TokenType::Identifier, string("hi")),
            value: Rc::new(literal(string("hello"))),
        };
        let stmt = IfStmt {
            predicate: Rc::new(literal(string("smth"))),
            then_branch: Rc::new(Stmt::Expression(ExpressionStmt {
                expression: Rc::new(Expr::Assign(expr)),
            })),
            else_branch: None,
        };
        terp.visit_var_stmt(&var_stmt).expect("Failed to set var");
        let result = terp.visit_if_stmt(&stmt);
        assert!(result.is_ok());
        // Read variable
        let result = terp
            .environment
            .borrow()
            .borrow()
            .get(&token_literal(TokenType::Identifier, string("hi")));
        assert_eq!(result.expect("hi"), Value::String("hello".to_string()))
    }

    #[test]
    fn assign() {
        let terp = Interpreter::new();
        let stmt = VarStmt {
            name: token_literal(TokenType::Identifier, string("hi")),
            initializer: Rc::new(literal(string("hi"))),
        };
        let expr = AssignExpr {
            name: token_literal(TokenType::Identifier, string("hi")),
            value: Rc::new(literal(string("hello"))),
        };
        let result = terp.visit_var_stmt(&stmt);
        assert!(result.is_ok());
        let result = terp.visit_assign_expr(&expr);
        assert!(result.is_ok());
        assert_eq!(result.expect("hi"), Value::String("hello".to_string()))
    }

    fn set_var(name: &str, init: Value, value: Value) -> (Stmt, Expr) {
        let stmt = Stmt::Var(VarStmt {
            name: token_literal(TokenType::Identifier, string("hi")),
            initializer: Rc::new(literal(init)),
        });
        let expr = Expr::Assign(AssignExpr {
            name: token_literal(TokenType::Identifier, string(name)),
            value: Rc::new(literal(value)),
        });

        (stmt, expr)
    }

    fn read_var(inter: Interpreter, name: &str) -> Value {
        inter
            .environment
            .borrow()
            .borrow()
            .get(&token_literal(TokenType::Identifier, string(name)))
            .expect("Could not read checker value")
    }

    #[test]
    fn logical_and_true() {
        let terp = Interpreter::new();
        let (stmt, expr) = set_var("executed", bool(false), bool(true));

        terp.execute(&stmt)
            .expect("Could not initialize checker var");

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(true))),
            operator: token(TokenType::And),
            right: Rc::new(expr),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::Bool(true)));

        // Should have evaluated the right arm of the AND operator
        let val = read_var(terp, "executed");

        assert!(matches!(val, Value::Bool(true)));
    }
    #[test]
    fn logical_and_false() {
        let terp = Interpreter::new();
        let (stmt, expr) = set_var("executed", bool(false), bool(true));

        terp.execute(&stmt)
            .expect("Could not initialize checker var");

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(false))),
            operator: token(TokenType::And),
            right: Rc::new(expr),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::Bool(false)));

        // Should have NOT evaluated the right arm of the AND operator
        let val = read_var(terp, "executed");

        assert!(matches!(val, Value::Bool(false)));
    }
    #[test]
    fn logical_or_true() {
        let terp = Interpreter::new();
        let (stmt, expr) = set_var("executed", bool(false), bool(true));

        terp.execute(&stmt)
            .expect("Could not initialize checker var");

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(true))),
            operator: token(TokenType::Or),
            right: Rc::new(expr),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::Bool(true)));

        // Should have NOT evaluated the right arm of the OR operator
        let val = read_var(terp, "executed");

        assert!(matches!(val, Value::Bool(false)));
    }
    #[test]
    fn logical_or_false() {
        let terp = Interpreter::new();
        let (stmt, expr) = set_var("executed", bool(false), bool(true));

        terp.execute(&stmt)
            .expect("Could not initialize checker var");

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(false))),
            operator: token(TokenType::Or),
            right: Rc::new(expr),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        // true is the result of the right arm of the OR
        assert!(matches!(result.expect(""), Value::Bool(true)));

        // Should have evaluated the right arm of the AND operator
        let val = read_var(terp, "executed");

        assert!(matches!(val, Value::Bool(true)));
    }

    #[test]
    fn logical_and_left_type() {
        let terp = Interpreter::new();

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(Value::Nil)),
            operator: token(TokenType::And),
            right: Rc::new(literal(string("hi"))),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::Nil));
    }
    #[test]
    fn logical_and_right_type() {
        let terp = Interpreter::new();

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(true))),
            operator: token(TokenType::And),
            right: Rc::new(literal(string("hi"))),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }
    #[test]
    fn logical_or_left_type() {
        let terp = Interpreter::new();

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(string("hi"))),
            operator: token(TokenType::Or),
            right: Rc::new(literal(bool(false))),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }
    #[test]
    fn logical_or_right_type() {
        let terp = Interpreter::new();

        let ex = Expr::Logical(LogicalExpr {
            left: Rc::new(literal(bool(false))),
            operator: token(TokenType::Or),
            right: Rc::new(literal(string("hi"))),
        });

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }

    #[test]
    fn while_loop() {
        let terp = Interpreter::new();

        let var_name = token_literal(TokenType::Identifier, string("count"));

        let init_var = Stmt::Var(VarStmt {
            name: var_name.clone(),
            initializer: Rc::new(literal(number(0.0))),
        });

        let stmt = Stmt::While(WhileStmt {
            // count < 5
            predicate: Rc::new(Expr::Binary(BinaryExpr {
                left: Rc::new(Expr::Var(VarExpr {
                    name: var_name.clone(),
                })),
                operator: token(TokenType::Less),
                right: Rc::new(literal(number(5.0))),
            })),
            body: Rc::new(Stmt::Expression(ExpressionStmt {
                // count = count + 1
                expression: Rc::new(Expr::Assign(AssignExpr {
                    name: var_name.clone(),
                    value: Rc::new(Expr::Binary(BinaryExpr {
                        left: Rc::new(Expr::Var(VarExpr {
                            name: var_name.clone(),
                        })),
                        operator: token(TokenType::Plus),
                        right: Rc::new(literal(number(1.0))),
                    })),
                })),
            })),
        });

        terp.execute(&init_var)
            .expect("failed to initalize counter");

        let result = terp.execute(&stmt);

        assert!(result.is_ok());

        let value = read_var(terp, "count");

        assert!(matches!(value, Value::Number(n) if n == 5.0));
    }
}
