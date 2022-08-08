use crate::ast::*;
use crate::lox_error::Demistify;
use crate::lox_error::LoxError;
use crate::lox_error::LoxErrorCode;
use crate::lox_error::LoxResult;
use crate::lox_error::ParserError;
use crate::lox_error::ParserErrorCode;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::value::Value;
use crate::scanner::Scan;

impl ParserError {
    fn demistify_next_token(&self) -> String {
        self.next_token
            .clone()
            .map(|next_token| format!(", got {}", next_token.demistify()))
            .unwrap_or("".to_string())
    }
}

impl Demistify for ParserError {
    fn demistify(&self) -> String {
        match self.code {
            LoxErrorCode::Parser(code) => match code {
                ParserErrorCode::None => "None".to_string(),
                ParserErrorCode::MissingIdentifierAfterVarKeyword => {
                    format!(
                        "expected identifier after 'var'{}",
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::MissingExpressionAfterVarEqual => {
                    format!(
                        "expected expression after '='{}",
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::MissingSemicolonOrEqualAfterVarDeclaration => {
                    if matches!(self.token.ttype, TokenType::Identifier) {
                        format!(
                            "expected ';' or '=' after variable declaration{}",
                            self.demistify_next_token()
                        )
                    } else {
                        format!(
                            "expected ';' after variable declaration{}",
                            self.demistify_next_token()
                        )
                    }
                }
                ParserErrorCode::MissingExpressionAfterPrintKeyword => {
                    format!(
                        "expected expression after 'print' keyword{}",
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::MissingSemicolonAfterPrintStatement => {
                    format!(
                        "expected ';' after {}{}",
                        self.token.demistify(),
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::MissingSemicolonAfterExpressionStatement => {
                    format!(
                        "expected ';' after {}{}",
                        self.token.demistify(),
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::MissingOpenParenAfterIfKeyword => {
                    format!(
                        "expected '(' after if{}",
                        self.demistify_next_token()
                    )
                }
                ParserErrorCode::UnterminatedIfPredicate => "UnterminatedIfPredicate".to_string(),
                ParserErrorCode::MissingClosingParenAfterIfPredicate => {
                    "MissingClosingParenAfterIfPredicate".to_string()
                }
                ParserErrorCode::MissingStatementAfterIf => "MissingStatementAfterIf".to_string(),
                ParserErrorCode::MissingStatementAfterElse => {
                    "MissingStatementAfterElse".to_string()
                }
                ParserErrorCode::MissingOpenParenAfterWhileKeyword => {
                    "MissingOpenParenAfterWhileKeyword".to_string()
                }
                ParserErrorCode::UnterminatedWhilePredicate => {
                    "UnterminatedWhilePredicate".to_string()
                }
                ParserErrorCode::MissingClosingParenAfterWhilePredicate => {
                    "MissingClosingParenAfterWhilePredicate".to_string()
                }
                ParserErrorCode::MissingStatementAfterWhile => {
                    "MissingStatementAfterWhile".to_string()
                }
                ParserErrorCode::MissingOpenParenAfterForKeyword => {
                    "MissingOpenParenAfterForKeyword".to_string()
                }
                ParserErrorCode::MissingSemicolonAfterForIteration => {
                    "MissingSemicolonAfterForIteration".to_string()
                }
                ParserErrorCode::MissingClosingParenAfterFor => {
                    "MissingClosingParenAfterFor".to_string()
                }
                ParserErrorCode::MissingForBody => "MissingForBody".to_string(),
                ParserErrorCode::UnterminatedBlock => "UnterminatedBlock".to_string(),
                ParserErrorCode::UnterminatedAssignment => "UnterminatedAssignment".to_string(),
                ParserErrorCode::InvalidAssignmentTarget => "InvalidAssignmentTarget".to_string(),
                ParserErrorCode::UnterminatedLogicalOr => "UnterminatedLogicalOr".to_string(),
                ParserErrorCode::UnterminatedLogicalAnd => "UnterminatedLogicalAnd".to_string(),
                ParserErrorCode::MissingEqualityRightHandSide => {
                    "MissingEqualityRightHandSide".to_string()
                }
                ParserErrorCode::MissingComparisonRightHandSide => {
                    "MissingComparisonRightHandSide".to_string()
                }
                ParserErrorCode::MissingTermRightHandSide => "MissingTermRightHandSide".to_string(),
                ParserErrorCode::MissingFactorRightHandSide => {
                    "MissingFactorRightHandSide".to_string()
                }
                ParserErrorCode::MissingUnaryRightHandSide => {
                    "MissingUnaryRightHandSide".to_string()
                }
                ParserErrorCode::MissingClosingParenAfterArgumentList => {
                    "MissingClosingParenAfterArgumentList".to_string()
                }
                ParserErrorCode::UnterminatedArgumentList => "UnterminatedArgumentList".to_string(),
                ParserErrorCode::UnterminatedGroup => "UnterminatedGroup".to_string(),
                ParserErrorCode::MissingClosingParenAfterGroup => {
                    "MissingClosingParenAfterGroup".to_string()
                }
                ParserErrorCode::UnexpectedTokenInExpression => {
                    "UnexpectedTokenInExpression".to_string()
                }
            },
            _ => "".to_string(),
        }
    }
}

pub struct Parser<'a> {
    last: Option<Token>,
    next: Option<Token>,
    scanner: Box<dyn Scan + 'a>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Box<dyn Scan + 'a>) -> Self {
        Self {
            scanner,
            next: None,
            last: None,
        }
    }

    // declaration -> "var" var_declaration
    //              | statement
    pub fn declaration(&mut self) -> Result<Option<Stmt>, LoxError> {
        let r = match self.is_match(&[TokenType::Var])? {
            Some(_) => self.var_declaration(),
            None => self.statement(),
        };
        // Continue if the declaration failed
        match r {
            Ok(r) => Ok(r),
            Err(err) => {
                self.synchronize()?;
                Err(err)
            }
        }
    }

    // var_delaration -> IDENTIFIER ( "=" expression )? ";"
    fn var_declaration(&mut self) -> Result<Option<Stmt>, LoxError> {
        let name = self
            .is_match(&[TokenType::Identifier])?
            .ok_or_else(|| self.error(ParserErrorCode::MissingIdentifierAfterVarKeyword))?;
        let mut initializer = Expr::Literal(LiteralExpr { value: Value::Nil });
        if self.is_match(&[TokenType::Equal])?.is_some() {
            initializer = self
                .expression()?
                .ok_or_else(|| self.error(ParserErrorCode::MissingExpressionAfterVarEqual))?;
        }

        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonOrEqualAfterVarDeclaration,
        )?;

        Ok(Some(Stmt::Var(VarStmt {
            name,
            initializer: Box::new(initializer),
        })))
    }

    // statement -> exprStmt
    //            | ifStmt
    //            | printStmt
    //            | whileStmt
    //            | forStmt
    //            | block
    pub fn statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.peek()? {
            Some(token) => match token.ttype {
                TokenType::If => {
                    self.skip()?;
                    self.if_statement()
                }
                TokenType::Print => {
                    self.skip()?;
                    self.print_statement()
                }
                TokenType::While => {
                    self.skip()?;
                    self.while_statement()
                }
                TokenType::For => {
                    self.skip()?;
                    self.for_statement()
                }
                TokenType::OpenBrace => {
                    self.skip()?;
                    self.block()
                }
                _ => self.expression_statement(),
            },
            None => Ok(None),
        }
    }

    // printStmt -> "print" expression
    fn print_statement(&mut self) -> LoxResult<Option<Stmt>> {
        let expr = self
            .expression()?
            .ok_or_else(|| self.error(ParserErrorCode::MissingExpressionAfterPrintKeyword))?;
        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonAfterPrintStatement,
        )?;
        Ok(Some(Stmt::new_print(Box::new(expr))))
    }
    fn if_statement(&mut self) -> LoxResult<Option<Stmt>> {
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterIfKeyword,
        )?;

        let predicate = self
            .expression()?
            .ok_or_else(|| self.error(ParserErrorCode::UnterminatedIfPredicate))?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingOpenParenAfterIfKeyword,
        )?;
        let then_branch = self
            .statement()?
            .ok_or_else(|| self.error(ParserErrorCode::MissingStatementAfterIf))?;
        let mut else_branch = None;
        if self.is_match(&[TokenType::Else])?.is_some() {
            else_branch = Some(
                self.statement()?
                    .ok_or_else(|| self.error(ParserErrorCode::MissingStatementAfterElse))?,
            );
        }
        Ok(Some(Stmt::new_if(
            Box::new(predicate),
            Box::new(then_branch),
            else_branch.map(Box::new),
        )))
    }
    fn while_statement(&mut self) -> LoxResult<Option<Stmt>> {
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterWhileKeyword,
        )?;

        let predicate = self
            .expression()?
            .ok_or_else(|| self.error(ParserErrorCode::UnterminatedWhilePredicate))?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterWhilePredicate,
        )?;

        let body = self
            .statement()?
            .ok_or_else(|| self.error(ParserErrorCode::MissingStatementAfterWhile))?;

        Ok(Some(Stmt::new_while(Box::new(predicate), Box::new(body))))
    }
    fn for_statement(&mut self) -> LoxResult<Option<Stmt>> {
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterForKeyword,
        )?;
        let initializer = match self.is_match(&[TokenType::Semicolon, TokenType::Var])? {
            Some(token) if matches!(token.ttype, TokenType::Semicolon) => None,
            Some(token) if matches!(token.ttype, TokenType::Var) => self.var_declaration()?,
            _ => self.expression_statement()?,
        };
        let predicate = match self.peek()? {
            Some(token) if matches!(token.ttype, TokenType::Semicolon) => None,
            _ => self.expression()?,
        }
        .unwrap_or(Expr::new_literal(Value::Bool(true)));

        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonAfterForIteration,
        )?;

        let increment = match self.peek()? {
            Some(token) if matches!(token.ttype, TokenType::CloseParen) => None,
            _ => self.expression()?,
        };
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterFor,
        )?;
        let mut body = self
            .statement()?
            .ok_or_else(|| self.error(ParserErrorCode::MissingForBody))?;

        if let Some(increment) = increment {
            body = Stmt::new_block(vec![body, Stmt::new_expression(Box::new(increment))]);
        }
        body = Stmt::new_while(Box::new(predicate), Box::new(body));
        if let Some(initializer) = initializer {
            body = Stmt::new_block(vec![initializer, body]);
        }
        Ok(Some(body))
    }
    // exprStmt -> expression ';'
    fn expression_statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.expression()? {
            Some(expr) => self
                .consume(
                    TokenType::Semicolon,
                    ParserErrorCode::MissingSemicolonAfterExpressionStatement,
                )
                .and(Ok(Some(Stmt::new_expression(Box::new(expr))))),

            None => Ok(None),
        }
    }

    fn block(&mut self) -> Result<Option<Stmt>, LoxError> {
        let mut statements = Vec::new();

        loop {
            if self.is_match(&[TokenType::CloseBrace])?.is_some() {
                break;
            }
            let decl = self
                .declaration()?
                .ok_or_else(|| self.error(ParserErrorCode::UnterminatedBlock))?;
            statements.push(decl);
        }
        Ok(Some(Stmt::new_block(statements)))
    }

    // assignment -> IDENTIFIER "=" assignment
    //             | logic_or
    fn assignment(&mut self) -> LoxResult<Option<Expr>> {
        if let Some(expr) = self.logic_or()? {
            if self.is_match(&[TokenType::Equal])?.is_some() {
                let value = self
                    .assignment()?
                    .ok_or_else(|| self.error(ParserErrorCode::UnterminatedAssignment))?;
                if let Expr::Var(v) = expr {
                    return Ok(Some(Expr::new_assign(v.name.clone(), Box::new(value))));
                }
                return Err(self.error(ParserErrorCode::InvalidAssignmentTarget));
            }
            return Ok(Some(expr));
        }
        // forward the none
        Ok(None)
    }
    // logic_or -> logic_and ( "or" logic_and )*
    fn logic_or(&mut self) -> LoxResult<Option<Expr>> {
        if let Some(mut expr) = self.logic_and()? {
            while let Some(operator) = self.is_match(&[TokenType::Or])? {
                let right = self
                    .logic_or()?
                    .ok_or_else(|| self.error(ParserErrorCode::UnterminatedLogicalOr))?;
                expr = Expr::new_logical(Box::new(expr), operator, Box::new(right));
            }
            return Ok(Some(expr));
        }
        Ok(None)
    }
    // logic_or -> equality ( "and" equality )* ;
    fn logic_and(&mut self) -> LoxResult<Option<Expr>> {
        if let Some(mut expr) = self.equality()? {
            while let Some(operator) = self.is_match(&[TokenType::And])? {
                let right = self
                    .equality()?
                    .ok_or_else(|| self.error(ParserErrorCode::UnterminatedLogicalAnd))?;
                expr = Expr::new_logical(Box::new(expr), operator, Box::new(right));
            }
            return Ok(Some(expr));
        }
        Ok(None)
    }

    // expression -> assignment
    pub fn expression(&mut self) -> LoxResult<Option<Expr>> {
        self.assignment()
    }

    // equality -> comparison ( ('==' | '!=') comparison )*
    fn equality(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.comparison()? {
            Some(mut expr) => {
                while let Some(operator) =
                    self.is_match(&[TokenType::EqualEqual, TokenType::BangEqual])?
                {
                    let right = self
                        .comparison()?
                        .ok_or_else(|| self.error(ParserErrorCode::MissingEqualityRightHandSide))?;
                    expr = Expr::new_binary(Box::new(expr), operator, Box::new(right))
                }
                Ok(Some(expr))
            }
            None => Ok(None),
        }
    }
    // comparison -> term ( ('>' | '>=' | '<' | '<=') term )*
    fn comparison(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.term()? {
            Some(mut expr) => {
                while let Some(operator) = self.is_match(&[
                    TokenType::Greater,
                    TokenType::GreaterEqual,
                    TokenType::Less,
                    TokenType::LessEqual,
                ])? {
                    let right = self.term()?.ok_or_else(|| {
                        self.error(ParserErrorCode::MissingComparisonRightHandSide)
                    })?;
                    expr = Expr::new_binary(Box::new(expr), operator, Box::new(right));
                }
                Ok(Some(expr))
            }
            None => Ok(None),
        }
    }
    // term -> factor ( ('-' | '+') factor )*
    fn term(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.factor()? {
            Some(mut expr) => {
                while let Some(operator) = self.is_match(&[TokenType::Minus, TokenType::Plus])? {
                    let right = self
                        .factor()?
                        .ok_or_else(|| self.error(ParserErrorCode::MissingTermRightHandSide))?;
                    expr = Expr::new_binary(Box::new(expr), operator, Box::new(right));
                }
                Ok(Some(expr))
            }
            None => Ok(None),
        }
    }
    // factor -> unary ( ('/' | '*') unary )*
    fn factor(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.unary()? {
            Some(mut expr) => {
                while let Some(operator) = self.is_match(&[TokenType::Slash, TokenType::Star])? {
                    let right = self
                        .unary()?
                        .ok_or_else(|| self.error(ParserErrorCode::MissingFactorRightHandSide))?;
                    expr = Expr::new_binary(Box::new(expr), operator, Box::new(right));
                }
                Ok(Some(expr))
            }
            None => Ok(None),
        }
    }
    // unary -> ('!' | '-') unary | call
    fn unary(&mut self) -> Result<Option<Expr>, LoxError> {
        if let Some(operator) = self.is_match(&[TokenType::Bang, TokenType::Minus])? {
            let right = self
                .term()?
                .ok_or_else(|| self.error(ParserErrorCode::MissingUnaryRightHandSide))?;
            return Ok(Some(Expr::new_unary(operator, Box::new(right))));
        }
        self.call()
    }
    // call -> primary ("(" arguments? ")")*
    fn call(&mut self) -> LoxResult<Option<Expr>> {
        self.primary()?
            .map(|mut expr| {
                loop {
                    if self.is_match(&[TokenType::OpenParen])?.is_none() {
                        break;
                    }
                    expr = self.finish_call(expr)?;
                }
                Ok(Some(expr))
            })
            .unwrap_or(Ok(None))
    }
    // arguments -> expression ("," expression)*
    fn finish_call(&mut self, callee: Expr) -> LoxResult<Expr> {
        let mut arguments = Vec::new();

        let next_is_close_paren = self
            .peek()?
            // token -> token type is close paren
            .and_then(|token| Some(matches!(token.ttype, TokenType::CloseParen)))
            // None -> false
            .unwrap_or(false);
        // Read arguments if there is no close paren in sight
        if !next_is_close_paren {
            loop {
                // Add the next expression, fail if missing
                let arg = self
                    .expression()?
                    .ok_or_else(|| self.error(ParserErrorCode::UnterminatedArgumentList))?;
                arguments.push(arg);
                // no ',' means end of argument list
                if self.is_match(&[TokenType::Comma])?.is_none() {
                    break;
                }
            }
        }
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterArgumentList,
        )?;
        Ok(Expr::new_call(Box::new(callee), arguments))
    }
    // primary -> NUMBER | STRING | true | false | nil | '(' expression ')' | IDENTIFIER
    fn primary(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.peek()? {
            Some(token) => match token.ttype {
                TokenType::False => {
                    self.skip()?;
                    Ok(Some(Expr::new_literal(Value::Bool(false))))
                }
                TokenType::True => {
                    self.skip()?;
                    Ok(Some(Expr::new_literal(Value::Bool(true))))
                }
                TokenType::Nil => {
                    self.skip()?;
                    Ok(Some(Expr::new_literal(Value::Nil)))
                }
                TokenType::Number => {
                    self.skip()?;
                    Ok(Some(Expr::new_literal(
                        token.literal.unwrap_or(Value::Number(0.0).clone()),
                    )))
                }
                TokenType::String => {
                    self.skip()?;
                    Ok(Some(Expr::new_literal(
                        token
                            .literal
                            .unwrap_or(Value::String("".to_string()).clone()),
                    )))
                }
                TokenType::OpenParen => {
                    self.skip()?;
                    let expr = self
                        .expression()?
                        .ok_or_else(|| self.error(ParserErrorCode::UnterminatedGroup))?;

                    self.consume(
                        TokenType::CloseParen,
                        ParserErrorCode::MissingClosingParenAfterGroup,
                    )?;
                    Ok(Some(Expr::new_grouping(Box::new(expr))))
                }
                TokenType::Identifier => {
                    self.skip()?;
                    Ok(Some(Expr::new_var(token.clone())))
                }
                TokenType::Eof => Ok(None),
                _ => Err(self.error(ParserErrorCode::UnexpectedTokenInExpression)),
            },
            _ => Ok(None),
        }
    }

    fn consume(&mut self, ttype: TokenType, code: ParserErrorCode) -> LoxResult<Token> {
        match self.peek()? {
            Some(token) if token.ttype == ttype => {
                self.skip()?;
                Ok(token)
            }
            _ => Err(self.error(code)), // error reporting will grab the correct tokens as we didn't consume anything here
        }
    }

    fn is_match(&mut self, types: &[TokenType]) -> Result<Option<Token>, LoxError> {
        for t in types {
            match self.peek()? {
                Some(token) if token.ttype == *t => {
                    return self.advance();
                }
                _ => {}
            }
        }
        Ok(None)
    }

    fn synchronize(&mut self) -> Result<(), LoxError> {
        // Scan through the next tokens
        while let Some(token) = self.peek()? {
            match token.ttype {
                // These keywords successfully synchronize,
                // don't advance in this situation
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return Ok(());
                }
                // Semi colon also synchronizes, but we can consume it
                TokenType::Semicolon => {
                    self.advance()?;
                    return Ok(());
                }
                _ => {
                    self.advance()?;
                }
            }
        }
        Ok(())
    }

    fn skip(&mut self) -> Result<(), LoxError> {
        if !matches!(self.next, None) {
            self.last = self.next.clone();
            self.next = None;
        } else {
            self.last = self.scanner.next()?;
        }
        Ok(())
    }

    fn peek(&mut self) -> Result<Option<Token>, LoxError> {
        if matches!(self.next, None) {
            self.next = self.scanner.next()?;
        }
        Ok(self.next.clone())
    }

    fn advance(&mut self) -> Result<Option<Token>, LoxError> {
        match self.next.clone() {
            Some(token) => {
                self.last = Some(token.clone());
                self.next = None;
                Ok(Some(token))
            }
            _ => {
                self.last = self.scanner.next()?;
                Ok(self.last.clone())
            }
        }
    }
    fn error(&mut self, code: ParserErrorCode) -> LoxError {
        let next = self.peek().unwrap_or(None);
        match &self.last {
            Some(token) => self.report_error(token, &next, code),
            None => LoxError::scanner(self.scanner.line(), "".to_string()), //todo code
        }
    }
    /// reports a parser error. The passed token will be considered the place of the error
    /// the parser's last token will be used as next_token
    fn error_with_token(&self, token: &Token, code: ParserErrorCode) -> LoxError {
        self.report_error(token, &self.last, code)
    }
    fn report_error(
        &self,
        token: &Token,
        next_token: &Option<Token>,
        code: ParserErrorCode,
    ) -> LoxError {
        let err = LoxError::Parser(ParserError {
            token: token.clone(),
            next_token: next_token.clone(),
            code: LoxErrorCode::Parser(code),
        });
        err.report();
        err
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{CallExpr, Expr, LiteralExpr, Stmt, VarExpr},
        lox_error::{LoxResult, ParserErrorCode},
        scanner::{token::Token, token_type::TokenType, value::Value, Scan},
    };

    use super::Parser;

    struct TestScanner {
        tokens: Vec<Token>,
    }

    impl TestScanner {
        pub fn new(tokens: Vec<Token>) -> Self {
            Self { tokens }
        }
    }

    impl Scan for TestScanner {
        fn line(&self) -> usize {
            999
        }

        fn next(&mut self) -> LoxResult<Option<Token>> {
            Ok(match self.tokens.len() {
                0 => None,
                _ => Some(self.tokens.remove(0)),
            })
        }
    }

    fn token(ttype: TokenType) -> Token {
        Token::new(ttype, format!(""), 999, None)
    }
    fn token_literal(ttype: TokenType, literal: Value) -> Token {
        Token::new(ttype, format!(""), 999, Some(literal))
    }

    fn test_parse_stmt(tokens: Vec<Token>) -> LoxResult<Option<Stmt>> {
        let s = TestScanner::new(tokens);
        let mut parser = Parser::new(Box::new(s));

        parser.declaration()
    }
    fn test_parse_expr(tokens: Vec<Token>) -> LoxResult<Option<Expr>> {
        let s = TestScanner::new(tokens);
        let mut parser = Parser::new(Box::new(s));

        parser.expression()
    }

    #[test]
    fn primary_false() {
        let result = test_parse_expr(vec![token(TokenType::False)]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        assert!(matches!(
            expr,
            Expr::Literal(LiteralExpr {
                value: Value::Bool(false)
            })
        ))
    }

    #[test]
    fn primary_true() {
        let result = test_parse_expr(vec![token(TokenType::True)]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        assert!(matches!(
            expr,
            Expr::Literal(LiteralExpr {
                value: Value::Bool(true)
            })
        ))
    }

    #[test]
    fn primary_number() {
        let result = test_parse_expr(vec![token_literal(TokenType::Number, Value::Number(9.0))]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        match expr {
            Expr::Literal(LiteralExpr {
                value: Value::Number(n),
            }) if n == 9.0 => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn primary_string() {
        let result = test_parse_expr(vec![token_literal(
            TokenType::String,
            Value::String(String::from("hi")),
        )]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        match expr {
            Expr::Literal(LiteralExpr {
                value: Value::String(s),
            }) if s == String::from("hi") => assert!(true),
            _ => assert!(false),
        }
    }

    #[test]
    fn primary_nil() {
        let result = test_parse_expr(vec![token(TokenType::Nil)]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        assert!(matches!(
            expr,
            Expr::Literal(LiteralExpr { value: Value::Nil })
        ))
    }

    #[test]
    fn primary_identifier() {
        let result = test_parse_expr(vec![token_literal(
            TokenType::Identifier,
            Value::String(String::from("ident")),
        )]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();

        match expr {
            Expr::Var(VarExpr { name: t })
                if t.literal == Some(Value::String(String::from("ident"))) =>
            {
                assert!(true)
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn primary_grouping() {
        let result = test_parse_expr(vec![
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
            token(TokenType::CloseParen),
        ]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail").unwrap();
        assert!(matches!(expr, Expr::Grouping(_)))
    }
    #[test]
    fn primary_empty_grouping() {
        let result = test_parse_expr(vec![
            token(TokenType::OpenParen),
            token(TokenType::CloseParen),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn primary_unterminated_grouping() {
        let result = test_parse_expr(vec![
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn primary_missing_closing_paren() {
        let result = test_parse_expr(vec![
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
            token(TokenType::Semicolon),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn primary_eof() {
        let result = test_parse_expr(vec![token(TokenType::Eof)]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail");
        assert!(expr.is_none())
    }

    #[test]
    fn expext_semi_colon_success() {
        let s = TestScanner::new(vec![token(TokenType::Semicolon)]);
        let mut parser = Parser::new(Box::new(s));

        let res = parser.consume(TokenType::Semicolon, ParserErrorCode::None);
        assert!(res.is_ok())
    }

    #[test]
    fn expext_semi_colon_failure() {
        let s = TestScanner::new(vec![token(TokenType::Minus)]);
        let mut parser = Parser::new(Box::new(s));

        let res = parser.consume(TokenType::Semicolon, ParserErrorCode::None);
        assert!(res.is_err())
    }

    #[test]
    fn unterminated_block() {
        let result = test_parse_stmt(vec![
            token(TokenType::OpenBrace),
            token(TokenType::Print),
            token_literal(TokenType::Number, Value::Number(1.0)),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn if_missing_open_paren() {
        let result = test_parse_stmt(vec![token(TokenType::If), token(TokenType::Print)]);

        assert!(result.is_err());
    }
    #[test]
    fn if_missing_predicate() {
        let result = test_parse_stmt(vec![
            token(TokenType::If),
            token(TokenType::OpenParen),
            token(TokenType::Print),
        ]);

        assert!(result.is_err());
    }
    #[test]
    fn if_missing_closing_paren() {
        let result = test_parse_stmt(vec![
            token(TokenType::If),
            token(TokenType::OpenParen),
            token(TokenType::True),
            token(TokenType::Semicolon),
        ]);

        assert!(result.is_err());
    }
    #[test]
    fn if_inline() {
        let result = test_parse_stmt(vec![
            token(TokenType::If),
            token(TokenType::OpenParen),
            token(TokenType::True),
            token(TokenType::CloseParen),
            token(TokenType::True),
            token(TokenType::Semicolon),
        ]);

        assert!(result.is_ok());
    }
    #[test]
    fn if_missing_else_stmt_after_else() {
        let result = test_parse_stmt(vec![
            token(TokenType::If),
            token(TokenType::OpenParen),
            token(TokenType::True),
            token(TokenType::CloseParen),
            token(TokenType::True),
            token(TokenType::Semicolon),
            token(TokenType::Else),
            token(TokenType::Semicolon),
        ]);

        assert!(result.is_err());
    }
    #[test]
    fn if_else() {
        let result = test_parse_stmt(vec![
            token(TokenType::If),
            token(TokenType::OpenParen),
            token(TokenType::True),
            token(TokenType::CloseParen),
            token(TokenType::True),
            token(TokenType::Semicolon),
            token(TokenType::Else),
            token(TokenType::True),
            token(TokenType::Semicolon),
        ]);

        assert!(result.is_ok());
    }

    #[test]
    fn logic() {
        let result = test_parse_expr(vec![
            token(TokenType::True),
            token(TokenType::And),
            token(TokenType::False),
            token(TokenType::Or),
            token(TokenType::False),
            token(TokenType::And),
            token(TokenType::Nil),
        ]);

        assert!(result.is_ok());
    }

    #[test]
    fn fn_call_no_args() {
        let result = test_parse_expr(vec![
            token_literal(TokenType::Identifier, Value::String("my_fn".to_string())),
            token(TokenType::OpenParen),
            token(TokenType::CloseParen),
        ]);

        assert!(result.is_ok());

        let expr = result.expect("").expect("");

        if let Expr::Call(CallExpr { callee, arguments }) = expr {
            if let Expr::Var(VarExpr { name }) = *callee {
                if let Some(Value::String(s)) = name.literal {
                    assert_eq!(s, "my_fn");
                } else {
                    assert!(false);
                }
            } else {
                assert!(false);
            }

            assert_eq!(arguments.len(), 0);
        } else {
            assert!(false);
        }
    }
    #[test]
    fn fn_call_args() {
        let result = test_parse_expr(vec![
            token_literal(TokenType::Identifier, Value::String("my_fn".to_string())),
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
            token(TokenType::CloseParen),
        ]);

        assert!(result.is_ok());

        let expr = result.expect("").expect("");

        if let Expr::Call(CallExpr {
            callee: _,
            arguments,
        }) = expr
        {
            assert_eq!(arguments.len(), 1);
        } else {
            assert!(false);
        }
    }
    #[test]
    fn fn_call_missing_closing_paren() {
        let result = test_parse_expr(vec![
            token_literal(TokenType::Identifier, Value::String("my_fn".to_string())),
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
            token(TokenType::OpenBrace),
        ]);

        assert!(result.is_err());
    }
    #[test]
    fn fn_call_unnterminated_args() {
        let result = test_parse_expr(vec![
            token_literal(TokenType::Identifier, Value::String("my_fn".to_string())),
            token(TokenType::OpenParen),
            token_literal(TokenType::Number, Value::Number(7.0)),
            token(TokenType::Comma),
        ]);

        assert!(result.is_err());
    }

    #[test]
    fn tracks_last_token() {
        let s = TestScanner::new(vec![
            token_literal(TokenType::Number, Value::Number(7.9)),
            token(TokenType::Plus),
            token_literal(TokenType::Number, Value::Number(7.9)),
        ]);
        let mut parser = Parser::new(Box::new(s));

        assert!(parser.last.is_none());
        parser.skip().expect("");
        assert!(matches!(
            parser.last.clone().unwrap().ttype,
            TokenType::Number
        ));
        parser.peek().expect("");
        assert!(matches!(
            parser.last.clone().unwrap().ttype,
            TokenType::Number
        ));
        parser.advance().expect("");
        assert!(matches!(
            parser.last.clone().unwrap().ttype,
            TokenType::Plus
        ));
    }
}
