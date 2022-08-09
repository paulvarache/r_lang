use r_ast::define_ast;
use std::rc::Rc;

use crate::lox_error::LoxError;
use crate::scanner::token::Token;
use crate::scanner::token::Span;
use crate::scanner::value::Value;

define_ast!(Expr(
    Assign: { name: Token, value: Expr },
    Binary: { left: Expr, operator: Token, right: Expr },
    Call: { callee: Expr, arguments: Vec<Expr> },
    Grouping: { expression: Expr },
    Literal: { value: Value },
    Logical: { left: Expr, operator: Token, right: Expr },
    Unary: { operator: Token, right: Expr },
    Var: { name: Token },
));

define_ast!(Stmt(
    Block: { statements: Vec<Stmt> },
    Expression: { expression: Expr },
    Function: {name: Token, params: Vec<Token>, body: Vec<Stmt> },
    If: { predicate: Expr, then_branch: Stmt, else_branch: Option<Stmt> },
    Print: { expression: Expr },
    Var: { name: Token, initializer: Expr },
    While: { predicate: Expr, body: Stmt },
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
    fn visit_assign_expr(&self, expr: &AssignExpr) -> Result<String, LoxError> {
        Ok(format!(
            "(set {} {})",
            expr.name.as_string(),
            expr.value.accept(self)?
        ))
    }
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
            Value::Func(_) => Ok("func".to_string()),
            Value::Nil => Ok("nil".to_string()),
        }
    }

    fn visit_logical_expr(&self, expr: &LogicalExpr) -> Result<String, LoxError> {
        self.parenthesize(&expr.operator.lexeme, &[&expr.left, &expr.right])
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<String, LoxError> {
        self.parenthesize(&expr.operator.lexeme, &[&expr.right])
    }

    fn visit_var_expr(&self, expr: &VarExpr) -> Result<String, LoxError> {
        Ok(format!("{}", expr.name))
    }

    fn visit_call_expr(&self, expr: &CallExpr) -> Result<String, LoxError> {
        let args = expr
            .arguments
            .iter()
            .try_fold(String::new(), |acc, expr| Ok(acc + &expr.accept(self)?))?;

        Ok(format!("call {} ({})", expr.callee.accept(self)?, args))
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_block_stmt(&self, stmt: &BlockStmt) -> Result<String, LoxError> {
        stmt.statements
            .iter()
            .try_fold(String::new(), |acc, stmt| Ok(acc + &stmt.accept(self)?))
    }

    fn visit_expression_stmt(&self, stmt: &ExpressionStmt) -> Result<String, LoxError> {
        stmt.expression.accept(self)
    }

    fn visit_if_stmt(&self, stmt: &IfStmt) -> Result<String, LoxError> {
        let predicate = stmt.predicate.accept(self)?;
        let then = stmt.then_branch.accept(self)?;
        let mut str = format!("(if ({}) then {}", predicate, then);
        if let Some(else_branch) = &stmt.else_branch {
            str = format!("{} else {}", str, else_branch.accept(self)?);
        }

        str = format!("{})", str);

        Ok(str)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> Result<String, LoxError> {
        self.parenthesize(&"print".to_string(), &[&stmt.expression])
    }

    fn visit_var_stmt(&self, stmt: &VarStmt) -> Result<String, LoxError> {
        self.parenthesize(&"var".to_string(), &[&stmt.initializer])
    }

    fn visit_while_stmt(&self, stmt: &WhileStmt) -> Result<String, LoxError> {
        Ok(format!(
            "(while {} {})",
            stmt.predicate.accept(self)?,
            stmt.body.accept(self)?
        ))
    }

    fn visit_function_stmt(&self,_stmt: &FunctionStmt) -> Result<String,LoxError>  {
        todo!()
    }
}
