use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::*;
use crate::callable::Callable;
use crate::class::LoxClass;
use crate::environment::*;
use crate::function::LoxFunction;
use crate::error::*;
use crate::native::DateNative;
use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::value::Value;

impl Demistify for InterpreterError {
    fn demistify(&self) -> String {
        match self.code {
            InterpreterErrorCode::NumberBinaryExprOperandsIncorrectType => {
                "NumberBinaryExprOperandsIncorrectType".to_string()
            }
            InterpreterErrorCode::PlusExprOperandsIncorrectType => {
                "PlusExprOperandsIncorrectType".to_string()
            }
            InterpreterErrorCode::UnknownBinaryOperator => "UnknownBinaryOperator".to_string(),
            InterpreterErrorCode::UnknownLogicalOperator => "UnknownLogicalOperator".to_string(),
            InterpreterErrorCode::UnaryMinusInvalidType => "UnaryMinusInvalidType".to_string(),
            InterpreterErrorCode::UnaryUnknownOperator => "UnaryUnknownOperator".to_string(),
            InterpreterErrorCode::AssignToUndefinedVar => "AssignToUndefinedVar".to_string(),
            InterpreterErrorCode::ReadUndefinedVar => "ReadUndefinedVar".to_string(),
            InterpreterErrorCode::NotAFunction => "NotAFunction".to_string(),
            InterpreterErrorCode::FunctionArityMismatch => "FunctionArityMismatch".to_string(),
            InterpreterErrorCode::RedefiningLocalVar => {
                "a variable with this name already exists in this scope".to_string()
            }
            InterpreterErrorCode::InitVarWithUnassignedVar => {
                "can't read local variable in its own initializer".to_string()
            }
            InterpreterErrorCode::ReturnOutsideFunction => {
                "cannot return outside a function".to_string()
            }
            InterpreterErrorCode::AccessingNonInstanceProperty => {
                "properties can only be accessed on class instances".to_string()
            }
            InterpreterErrorCode::InstancePropertyUndefined => "property is undefined".to_string(),
            InterpreterErrorCode::ReturnInsideConstructor => {
                "cannot return in initializer".to_string()
            }
            InterpreterErrorCode::ParentClassIsChildClass => {
                "parent class cannot be the child class itself".to_string()
            }
            InterpreterErrorCode::ParentClassIsNotClass => {
                "parent class is not a class".to_string()
            }
            InterpreterErrorCode::SuperOutsideClass => "cannot use `super` outside a class method".to_string(),
            InterpreterErrorCode::SuperOutsideSuperclass => "cannot use `super` in a class without a parent class".to_string(),
        }
    }
}

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    pub locals: RefCell<HashMap<usize, usize>>,
    environment: RefCell<Rc<RefCell<Environment>>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        globals.borrow_mut().define(
            "date",
            Value::Func(Rc::new(Callable {
                func: Rc::new(DateNative {}),
            })),
        );
        Self {
            globals: Rc::clone(&globals),
            environment: RefCell::new(Rc::clone(&globals)),
            locals: RefCell::new(HashMap::new()),
        }
    }
    pub fn execute(&self, stmt: &Stmt) -> Result<(), LoxError> {
        stmt.accept(self)
    }
    pub fn evaluate(&self, expr: &Expr) -> Result<Value, LoxError> {
        expr.accept(self)
    }
    pub fn execute_block(&self, statements: &Rc<Vec<Rc<Stmt>>>, e: Environment) -> LoxResult<()> {
        let previous = self.environment.replace(Rc::new(RefCell::new(e)));

        let result = statements.iter().try_for_each(|s| self.execute(&s.clone()));

        self.environment.replace(previous);

        result
    }
    fn lookup_variable(&self, name: &Token, expr_id: usize, expr_span: Span) -> LoxResult<Value> {
        let bor = self.locals.borrow();
        let depth = bor.get(&expr_id);
        if let Some(depth) = depth {
            self.environment
                .borrow()
                .as_ref()
                .borrow()
                .get_at(&name.lexeme, *depth)
        } else {
            self.globals
                .as_ref()
                .borrow()
                .get(name)
                .ok_or_else(|| self.error(expr_span, InterpreterErrorCode::ReadUndefinedVar))
        }
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
            _ => Err(self.error(
                expr.span,
                InterpreterErrorCode::NumberBinaryExprOperandsIncorrectType,
            )),
        }
    }
    pub fn error(&self, span: Span, code: InterpreterErrorCode) -> LoxError {
        LoxError::Interpreter(InterpreterError { span, code })
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
            Value::Class(_) => "class".to_string(),
            Value::Instance(i) => format!("instance({})", i.name.lexeme),
        };
        println!("{}", formatted);
        Ok(())
    }

    fn visit_var_stmt(&self, stmt: &crate::ast::VarStmt) -> Result<(), LoxError> {
        let value = stmt
            .initializer
            .as_ref()
            .map_or(Ok(Value::Nil), |init| self.evaluate(init))?;

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

    fn visit_function_stmt(&self, stmt: &FunctionStmt) -> Result<(), LoxError> {
        let f = LoxFunction::new(stmt, self.environment.borrow().deref(), false);
        let c = Callable { func: Rc::new(f) };

        self.environment
            .borrow()
            .borrow_mut()
            .define(&stmt.name.as_string(), Value::Func(Rc::new(c)));
        Ok(())
    }

    fn visit_return_stmt(&self, stmt: &ReturnStmt) -> Result<(), LoxError> {
        let value = stmt
            .expression
            .as_ref()
            .map_or(Ok(Value::Nil), |expr| self.evaluate(expr))?;
        Err(LoxError::Return(value))
    }

    fn visit_class_stmt(&self, stmt: &ClassStmt) -> Result<(), LoxError> {
        let mut superclass = None;
        if let Some(superclass_expr) = &stmt.superclass {
            let superclass_value = self.visit_var_expr(&superclass_expr)?;

            if let Value::Class(c) = superclass_value {
                superclass = Some(c);
            } else {
                return Err(self.error(
                    superclass_expr.span,
                    InterpreterErrorCode::ParentClassIsNotClass,
                ));
            }
        }
        self.environment
            .borrow()
            .as_ref()
            .borrow_mut()
            .define(&stmt.name.lexeme, Value::Nil);

        let mut superclass_previous_env = None;

        if let Some(superclass) = &superclass {
            let env = Environment::new_with_enclosing(self.environment.borrow().clone());
            superclass_previous_env = Some(self.environment.replace(Rc::new(RefCell::new(env))));
            self.environment
                .borrow()
                .borrow_mut()
                .define("super", Value::Class(Rc::clone(&superclass)));
        }

        let class = Value::Class(Rc::new(LoxClass::new(
            &stmt.name,
            stmt.methods.as_ref(),
            superclass,
            self.environment.borrow().deref(),
        )));

        if let Some(previous) = superclass_previous_env {
            self.environment.replace(previous);
        }

        self.environment
            .borrow()
            .as_ref()
            .borrow_mut()
            .assign(&stmt.name, class);

        Ok(())
    }
}

impl ExprVisitor<Value> for Interpreter {
    fn visit_assign_expr(&self, expr: &AssignExpr) -> Result<Value, LoxError> {
        let value = self.evaluate(&expr.value)?;
        let bor = self.locals.borrow();
        let depth = bor.get(&expr.id);
        if let Some(depth) = depth {
            self.environment.borrow().borrow_mut().assign_at(
                &expr.name.lexeme,
                value.clone(),
                *depth,
            );
        } else if !self
            .globals
            .as_ref()
            .borrow_mut()
            .assign(&expr.name, value.clone())
        {
            return Err(self.error(expr.span, InterpreterErrorCode::AssignToUndefinedVar));
        }
        Ok(value)
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
                _ => Err(self.error(
                    expr.span,
                    InterpreterErrorCode::PlusExprOperandsIncorrectType,
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
            _ => Err(self.error(expr.span, InterpreterErrorCode::UnknownBinaryOperator)),
        }
    }

    fn visit_call_expr(&self, expr: &CallExpr) -> Result<Value, LoxError> {
        let callee = self.evaluate(&expr.callee)?;

        let (callable, class) = match callee {
            Value::Func(c) => Ok((c, None)),
            Value::Class(c) => {
                let class = Rc::clone(&c);
                Ok((Rc::new(Callable { func: class }), Some(Rc::clone(&c))))
            }
            _ => Err(self.error(expr.callee.span(), InterpreterErrorCode::NotAFunction)),
        }?;

        let args = expr
            .arguments
            .iter()
            .map(|a| self.evaluate(a))
            .collect::<LoxResult<Vec<Value>>>()?;

        if args.len() != callable.func.arity() {
            return Err(self.error(expr.span, InterpreterErrorCode::FunctionArityMismatch));
        }
        callable.func.call(self, args, class)
    }

    fn visit_get_expr(&self, expr: &GetExpr) -> Result<Value, LoxError> {
        let value = self.evaluate(&expr.object)?;

        if let Value::Instance(inst) = value {
            inst.get(&expr.name, &inst).ok_or_else(|| {
                self.error(
                    expr.name.span,
                    InterpreterErrorCode::InstancePropertyUndefined,
                )
            })
        } else {
            Err(self.error(
                expr.span,
                InterpreterErrorCode::AccessingNonInstanceProperty,
            ))
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
            _ => Err(self.error(expr.span, InterpreterErrorCode::UnknownLogicalOperator)),
        }
    }

    fn visit_set_expr(&self, expr: &SetExpr) -> Result<Value, LoxError> {
        let obj = self.evaluate(&expr.object)?;

        if let Value::Instance(inst) = obj {
            let value = self.evaluate(&expr.value)?;
            inst.set(&expr.name, &value);
            Ok(value)
        } else {
            Err(self.error(
                expr.object.span(),
                InterpreterErrorCode::AccessingNonInstanceProperty,
            ))
        }
    }

    fn visit_super_expr(&self, expr: &SuperExpr) -> Result<Value, LoxError> {
        if let Some(depth) = self.locals.borrow().get(&expr.id) {
            let env = self.environment.borrow();
            let env = env.as_ref().borrow();
            let superclass = env.get_at("super", *depth)?;
            let value = env.get_at("this", *depth - 1)?;
            if let Value::Instance(inst) = &value {
                if let Value::Class(superclass) = superclass {
                    let callable = superclass
                        .find_method(&expr.name.lexeme)
                        .map(|m| superclass.bind_method(m, inst))
                        .ok_or_else(|| {
                            self.error(
                                expr.name.span,
                                InterpreterErrorCode::InstancePropertyUndefined,
                            )
                        })?;
                    return Ok(Value::Func(Rc::new(callable)));
                }
            }
        }
        Ok(Value::Nil) // Lots of else not taken care of. The elses will not happen since all generic values will have the right type from the resolver
    }

    fn visit_this_expr(&self, expr: &ThisExpr) -> Result<Value, LoxError> {
        self.lookup_variable(&expr.keyword, expr.id, expr.span)
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<Value, LoxError> {
        let right = self.evaluate(&expr.right)?;
        match expr.operator.ttype {
            TokenType::Minus => match right {
                Value::Number(_) => Ok(-right),
                _ => Err(self.error(expr.span, InterpreterErrorCode::UnaryMinusInvalidType)),
            },
            TokenType::Bang => Ok(Value::Bool(!Interpreter::is_thruthy(right))),
            _ => Err(self.error(expr.span, InterpreterErrorCode::UnaryUnknownOperator)),
        }
    }

    fn visit_var_expr(&self, expr: &VarExpr) -> LoxResult<Value> {
        self.lookup_variable(&expr.name, expr.id, expr.span)
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::token::Token;

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
        Token::new(ttype, "".to_string(), Span::new(0, 0, 0, 0), None)
    }

    fn token_literal(ttype: TokenType, literal: Value) -> Token {
        Token::new(ttype, "".to_string(), Span::new(0, 0, 0, 0), Some(literal))
    }

    fn span() -> Span {
        Span {
            start: (0, 0),
            end: (0, 0),
        }
    }

    fn literal(value: Value) -> Expr {
        Expr::new_literal(value, span())
    }

    #[test]
    fn test_unary_minus() {
        let terp = Interpreter::new();

        let unary_expr = Expr::new_unary(
            token(TokenType::Minus),
            Rc::new(literal(number(2.0))),
            span(),
        );
        let result = terp.evaluate(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Number(-2.0)));
    }
    #[test]
    fn test_unary_not() {
        let terp = Interpreter::new();
        let unary_expr = Expr::new_unary(
            token(TokenType::Bang),
            Rc::new(literal(bool(false))),
            span(),
        );
        let result = terp.evaluate(&unary_expr);
        assert!(result.is_ok());
        assert_eq!(result.ok(), Some(Value::Bool(true)));
    }

    #[test]
    fn if_then() {
        let terp = Interpreter::new();
        let var_stmt = Stmt::new_var(
            token_literal(TokenType::Identifier, string("hi")),
            Some(Rc::new(literal(string("hi")))),
            span(),
        );
        let expr = Expr::new_assign(
            token_literal(TokenType::Identifier, string("hi")),
            Rc::new(literal(string("hello"))),
            span(),
        );
        let stmt = Stmt::new_if(
            Rc::new(literal(string("smth"))),
            Rc::new(Stmt::new_expression(Rc::new(expr), span())),
            None,
            span(),
        );
        terp.execute(&var_stmt).expect("Failed to set var");
        let result = terp.execute(&stmt);
        assert!(result.is_ok());
        // Read variable
        let result = terp
            .environment
            .borrow()
            .as_ref()
            .borrow()
            .get(&token_literal(TokenType::Identifier, string("hi")));
        assert_eq!(result.expect("hi"), Value::String("hello".to_string()))
    }

    #[test]
    fn assign() {
        let terp = Interpreter::new();
        let stmt = Stmt::new_var(
            token_literal(TokenType::Identifier, string("hi")),
            Some(Rc::new(literal(string("hi")))),
            span(),
        );
        let expr = Expr::new_assign(
            token_literal(TokenType::Identifier, string("hi")),
            Rc::new(literal(string("hello"))),
            span(),
        );
        let result = terp.execute(&stmt);
        assert!(result.is_ok());
        let result = terp.evaluate(&expr);
        assert!(result.is_ok());
        assert_eq!(result.expect("hi"), Value::String("hello".to_string()))
    }

    fn set_var(name: &str, init: Value, value: Value) -> (Stmt, Expr) {
        let stmt = Stmt::new_var(
            token_literal(TokenType::Identifier, string("hi")),
            Some(Rc::new(literal(init))),
            span(),
        );
        let expr = Expr::new_assign(
            token_literal(TokenType::Identifier, string(name)),
            Rc::new(literal(value)),
            span(),
        );

        (stmt, expr)
    }

    fn read_var(inter: Interpreter, name: &str) -> Value {
        inter
            .environment
            .borrow()
            .as_ref()
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

        let ex = Expr::new_logical(
            Rc::new(literal(bool(true))),
            token(TokenType::And),
            Rc::new(expr),
            span(),
        );

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

        let ex = Expr::new_logical(
            Rc::new(literal(bool(false))),
            token(TokenType::And),
            Rc::new(expr),
            span(),
        );

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

        let ex = Expr::new_logical(
            Rc::new(literal(bool(true))),
            token(TokenType::Or),
            Rc::new(expr),
            span(),
        );

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

        let ex = Expr::new_logical(
            Rc::new(literal(bool(false))),
            token(TokenType::Or),
            Rc::new(expr),
            span(),
        );

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

        let ex = Expr::new_logical(
            Rc::new(literal(Value::Nil)),
            token(TokenType::And),
            Rc::new(literal(string("hi"))),
            span(),
        );

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::Nil));
    }
    #[test]
    fn logical_and_right_type() {
        let terp = Interpreter::new();

        let ex = Expr::new_logical(
            Rc::new(literal(bool(true))),
            token(TokenType::And),
            Rc::new(literal(string("hi"))),
            span(),
        );

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }
    #[test]
    fn logical_or_left_type() {
        let terp = Interpreter::new();

        let ex = Expr::new_logical(
            Rc::new(literal(string("hi"))),
            token(TokenType::Or),
            Rc::new(literal(bool(false))),
            span(),
        );

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }
    #[test]
    fn logical_or_right_type() {
        let terp = Interpreter::new();

        let ex = Expr::new_logical(
            Rc::new(literal(bool(false))),
            token(TokenType::Or),
            Rc::new(literal(string("hi"))),
            span(),
        );

        let result = terp.evaluate(&ex);

        assert!(result.is_ok());
        assert!(matches!(result.expect(""), Value::String(s) if s == "hi"));
    }

    #[test]
    fn while_loop() {
        let terp = Interpreter::new();

        let var_name = token_literal(TokenType::Identifier, string("count"));

        let init_var = Stmt::new_var(
            var_name.clone(),
            Some(Rc::new(literal(number(0.0)))),
            span(),
        );

        let stmt = Stmt::new_while(
            // count < 5
            Rc::new(Expr::new_binary(
                Rc::new(Expr::new_var(var_name.clone(), span())),
                token(TokenType::Less),
                Rc::new(literal(number(5.0))),
                span(),
            )),
            Rc::new(Stmt::new_expression(
                // count = count + 1
                Rc::new(Expr::new_assign(
                    var_name.clone(),
                    Rc::new(Expr::new_binary(
                        Rc::new(Expr::new_var(var_name.clone(), span())),
                        token(TokenType::Plus),
                        Rc::new(literal(number(1.0))),
                        span(),
                    )),
                    span(),
                )),
                span(),
            )),
            span(),
        );

        terp.execute(&init_var)
            .expect("failed to initalize counter");

        let result = terp.execute(&stmt);

        assert!(result.is_ok());

        let value = read_var(terp, "count");

        assert!(matches!(value, Value::Number(n) if n == 5.0));
    }
}
