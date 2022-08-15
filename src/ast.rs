use r_ast::define_ast;
use std::rc::Rc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crate::error::LoxError;
use crate::scanner::token::Token;
use crate::scanner::span::Span;
use crate::scanner::value::Value;

static AST_NODE_COUNTER: AtomicUsize = AtomicUsize::new(0);

define_ast!(Expr(
    Assign: { name: Token, value: Expr },
    Binary: { left: Expr, operator: Token, right: Expr },
    Call: { callee: Expr, arguments: Vec<Expr> },
    Get: { object: Expr, name: Token },
    Grouping: { expression: Expr },
    Literal: { value: Value },
    Logical: { left: Expr, operator: Token, right: Expr },
    Set: { object: Expr, name: Token, value: Expr },
    Super: { name: Token },
    This: { keyword: Token },
    Unary: { operator: Token, right: Expr },
    Var: { name: Token },
));

define_ast!(Stmt(
    Block: { statements: Vec<Stmt> },
    Class: { name: Token, methods: Vec<FunctionStmt>, superclass: Option<VarExpr> },
    Expression: { expression: Expr },
    Function: {name: Token, params: Vec<Token>, body: Vec<Stmt> },
    If: { predicate: Expr, then_branch: Stmt, else_branch: Option<Stmt> },
    Print: { expression: Expr },
    Return: { expression: Option<Expr> },
    Var: { name: Token, initializer: Option<Expr> },
    While: { predicate: Expr, body: Stmt },
));

pub struct AstPrinter {}

impl AstPrinter {
    // pub fn print(&self, expr: &Stmt) -> Result<String, LoxError> {
    //     expr.accept(self)
    // }
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
            Value::Class(_) => Ok("class".to_string()),
            Value::Instance(_) => Ok("instance".to_string()),
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

    fn visit_get_expr(&self,expr: &GetExpr) -> Result<String,LoxError>  {
        todo!()
    }

    fn visit_set_expr(&self,expr: &SetExpr) -> Result<String,LoxError>  {
        todo!()
    }

    fn visit_this_expr(&self,expr: &ThisExpr) -> Result<String,LoxError>  {
        todo!()
    }

    fn visit_super_expr(&self,expr: &SuperExpr) -> Result<String,LoxError>  {
        todo!()
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

    fn visit_function_stmt(&self,_stmt: &FunctionStmt) -> Result<String,LoxError>  {
        todo!()
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
        if let Some(init) = &stmt.initializer {
            self.parenthesize(&"var".to_string(), &[init])
        } else {
            self.parenthesize(&"var".to_string(), &[])
        }
    }

    fn visit_while_stmt(&self, stmt: &WhileStmt) -> Result<String, LoxError> {
        Ok(format!(
            "(while {} {})",
            stmt.predicate.accept(self)?,
            stmt.body.accept(self)?
        ))
    }

    fn visit_return_stmt(&self,_stmt: &ReturnStmt) -> Result<String,LoxError>  {
        todo!()
    }

    fn visit_class_stmt(&self,stmt: &ClassStmt) -> Result<String,LoxError>  {
        todo!()
    }
}
