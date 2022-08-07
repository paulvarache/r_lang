use crate::{
    ast::*,
    lox_error::{LoxError, LoxResult},
    scanner::{token::Token, token_type::TokenType, value::Value, Scan},
};

impl LoxError {
    pub fn parser(token: Token, message: String) -> Self {
        let err = LoxError::Parser { token, message };
        err.report();
        err
    }
}

pub struct Parser<'a> {
    next: Option<Token>,
    scanner: Box<dyn Scan + 'a>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Box<dyn Scan + 'a>) -> Self {
        Self {
            scanner,
            next: None,
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
            .ok_or_else(|| LoxError::scanner(1, "expected identifer after var".to_string()))?;
        let mut initializer = Expr::Literal(LiteralExpr { value: Value::Nil });

        if let Some(token) = self.is_match(&[TokenType::Equal])? {
            initializer = self.expression()?.ok_or_else(|| {
                LoxError::parser(token, "expected expression after =".to_string())
            })?;
        }

        self.expect_semi_colon("expected ; after var declaration".to_string())?;

        Ok(Some(Stmt::Var(VarStmt {
            name,
            initializer: Box::new(initializer),
        })))
    }

    // statement -> exprStmt
    //            | ifStmt
    //            | printStmt
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
        match self.expression()? {
            Some(expr) => self
                .expect_semi_colon("expected ; after value".to_string())
                .and(Ok(Some(Stmt::Print(PrintStmt {
                    expression: Box::new(expr),
                })))),
            None => Ok(None),
        }
    }
    fn if_statement(&mut self) -> LoxResult<Option<Stmt>> {
        let token = self.is_match(&[TokenType::OpenParen])?.ok_or_else(|| {
            LoxError::scanner(self.scanner.line(), "expected ( after if".to_string())
        })?;

        let predicate = self.expression()?.ok_or_else(|| {
            LoxError::parser(
                token.clone(),
                "expected expression in if parens".to_string(),
            )
        })?;
        let token = self.is_match(&[TokenType::CloseParen])?.ok_or_else(|| {
            LoxError::parser(token.clone(), "expected ) after if predicate".to_string())
        })?;
        let then_branch = self.statement()?.ok_or_else(|| {
            LoxError::parser(
                token.clone(),
                "expected statement after if predicate".to_string(),
            )
        })?;
        let mut else_branch = None;
        if let Some(token) = self.is_match(&[TokenType::Else])? {
            else_branch = Some(self.statement()?.ok_or_else(|| {
                LoxError::parser(token, "expected statement in else".to_string())
            })?);
        }
        Ok(Some(Stmt::If(IfStmt {
            predicate: Box::new(predicate),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        })))
    }
    fn while_statement(&mut self) -> LoxResult<Option<Stmt>> {
        let token = self.is_match(&[TokenType::OpenParen])?.ok_or_else(|| {
            LoxError::scanner(self.scanner.line(), "expected ( after while".to_string())
        })?;

        let predicate = self.expression()?.ok_or_else(|| {
            LoxError::parser(
                token.clone(),
                "expected expression in while parens".to_string(),
            )
        })?;
        let token = self.is_match(&[TokenType::CloseParen])?.ok_or_else(|| {
            LoxError::parser(token.clone(), "expected ) after while predicate".to_string())
        })?;
        let body = self.statement()?.ok_or_else(|| {
            LoxError::parser(
                token.clone(),
                "expected statement after if predicate".to_string(),
            )
        })?;
        Ok(Some(Stmt::While(WhileStmt {
            predicate: Box::new(predicate),
            body: Box::new(body),
        })))
    }
    // exprStmt -> expression ';'
    fn expression_statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.expression()? {
            Some(expr) => self
                .expect_semi_colon("expected ; after expression".to_string())
                .and(Ok(Some(Stmt::Expression(ExpressionStmt {
                    expression: Box::new(expr),
                })))),

            None => Ok(None),
        }
    }

    fn block(&mut self) -> Result<Option<Stmt>, LoxError> {
        let mut statements = Vec::new();

        loop {
            match self.peek()? {
                Some(Token {
                    ttype: TokenType::CloseBrace,
                    line: _,
                    lexeme: _,
                    literal: _,
                }) => {
                    self.skip()?;
                    break;
                }
                Some(_) => match self.declaration()? {
                    Some(stmt) => statements.push(stmt),
                    None => return Err(LoxError::scanner(1, "unterminated block".to_string())),
                },
                None => return Err(LoxError::scanner(1, "unterminated block".to_string())),
            };
        }
        Ok(Some(Stmt::Block(BlockStmt { statements })))
    }

    // assignment -> IDENTIFIER "=" assignment
    //             | logic_or
    fn assignment(&mut self) -> LoxResult<Option<Expr>> {
        if let Some(expr) = self.logic_or()? {
            if let Some(token) = self.is_match(&[TokenType::Equal])? {
                let value = self
                    .assignment()?
                    .ok_or_else(|| LoxError::parser(token, "expected assignment".to_string()))?;
                if let Expr::Var(v) = expr {
                    return Ok(Some(Expr::Assign(AssignExpr {
                        name: v.name,
                        value: Box::new(value),
                    })));
                }
                return Err(LoxError::scanner(
                    self.scanner.line(),
                    "invalid assignment target".to_string(),
                ));
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
                if let Some(right) = self.logic_and()? {
                    expr = Expr::Logical(LogicalExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right)
                    });
                } else {
                    return Err(LoxError::parser(operator, "Expected expression after 'or'".to_string()))
                }
            }
            return Ok(Some(expr));
        }
        Ok(None)
    }
    // logic_or -> equality ( "and" equality )* ;
    fn logic_and(&mut self) -> LoxResult<Option<Expr>> {
        if let Some(mut expr) = self.equality()? {
            while let Some(operator) = self.is_match(&[TokenType::And])? {
                if let Some(right) = self.equality()? {
                    expr = Expr::Logical(LogicalExpr {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right)
                    });
                } else {
                    return Err(LoxError::parser(operator, "Expected expression after 'and'".to_string()))
                }
            }
            return Ok(Some(expr));
        }
        Ok(None)
    }

    // expression -> equality
    pub fn expression(&mut self) -> LoxResult<Option<Expr>> {
        self.assignment()
    }

    // equality -> comparison ( ('==' | '!=') comparison )*
    fn equality(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.comparison()? {
            Some(mut expr) => {
                while let Some(token) =
                    self.is_match(&[TokenType::EqualEqual, TokenType::BangEqual])?
                {
                    match self.comparison()? {
                        Some(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Box::new(expr),
                                operator: token,
                                right: Box::new(right),
                            });
                        }
                        None => {
                            let t = token.ttype.clone();
                            return Err(LoxError::parser(
                                token,
                                format!("expected comparison after {}", t),
                            ));
                        }
                    }
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
                while let Some(token) = self.is_match(&[
                    TokenType::Greater,
                    TokenType::GreaterEqual,
                    TokenType::Less,
                    TokenType::LessEqual,
                ])? {
                    match self.term()? {
                        Some(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Box::new(expr),
                                operator: token,
                                right: Box::new(right),
                            });
                        }
                        None => {
                            let t = token.ttype.clone();
                            return Err(LoxError::parser(
                                token,
                                format!("expected term after {}", t),
                            ));
                        }
                    }
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
                while let Some(token) = self.is_match(&[TokenType::Minus, TokenType::Plus])? {
                    match self.factor()? {
                        Some(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Box::new(expr),
                                operator: token,
                                right: Box::new(right),
                            });
                        }
                        None => {
                            let t = token.ttype.clone();
                            return Err(LoxError::parser(
                                token,
                                format!("expected factor after {}", t),
                            ));
                        }
                    }
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
                while let Some(token) = self.is_match(&[TokenType::Slash, TokenType::Star])? {
                    match self.unary()? {
                        Some(right) => {
                            expr = Expr::Binary(BinaryExpr {
                                left: Box::new(expr),
                                operator: token,
                                right: Box::new(right),
                            });
                        }
                        None => {
                            let t = token.ttype.clone();
                            return Err(LoxError::parser(
                                token,
                                format!("expected unary after {}", t),
                            ));
                        }
                    }
                }
                Ok(Some(expr))
            }
            None => Ok(None),
        }
    }
    // unary -> ('!' | '-') unary
    //        | primary
    fn unary(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.peek()? {
            Some(token) => match token.ttype {
                TokenType::Bang | TokenType::Minus => {
                    self.skip()?;
                    match self.unary()? {
                        Some(right) => Ok(Some(Expr::Unary(UnaryExpr {
                            operator: token,
                            right: Box::new(right),
                        }))),
                        None => {
                            let t = token.ttype.clone();
                            Err(LoxError::parser(
                                token,
                                format!("Expected unary after {}", t),
                            ))
                        }
                    }
                }
                TokenType::Eof => Ok(None),
                _ => self.primary(),
            },
            None => Ok(None),
        }
    }
    // primary -> NUMBER | STRING | true | false | nil | '(' expression ')' | IDENTIFIER
    fn primary(&mut self) -> Result<Option<Expr>, LoxError> {
        match self.advance()? {
            Some(token) => match token.ttype {
                TokenType::False => Ok(Some(Expr::Literal(LiteralExpr {
                    value: Value::Bool(false),
                }))),
                TokenType::True => Ok(Some(Expr::Literal(LiteralExpr {
                    value: Value::Bool(true),
                }))),
                TokenType::Nil => Ok(Some(Expr::Literal(LiteralExpr { value: Value::Nil }))),
                TokenType::Number | TokenType::String => Ok(Some(Expr::Literal(LiteralExpr {
                    value: token.literal.unwrap_or(Value::Number(0.0)),
                }))),
                TokenType::OpenParen => match self.expression()? {
                    Some(expr) => {
                        let closing = self.advance()?;
                        match closing {
                            Some(c) if matches!(c.ttype, TokenType::CloseParen) => {
                                Ok(Some(Expr::Grouping(GroupingExpr {
                                    expression: Box::new(expr),
                                })))
                            }
                            Some(c) => {
                                Err(LoxError::parser(c, "expected closing paren".to_string()))
                            }
                            _ => Err(LoxError::parser(
                                token,
                                "expected closing paren got Eof".to_string(),
                            )),
                        }
                    }
                    None => Err(LoxError::parser(
                        token,
                        format!("expected expression after {}", TokenType::OpenParen),
                    )),
                },
                TokenType::Identifier => Ok(Some(Expr::Var(VarExpr { name: token }))),
                TokenType::Eof => Ok(None),
                _ => {
                    let t = token.ttype.clone();
                    Err(LoxError::parser(
                        token,
                        format!("expected primary got {}", t),
                    ))
                }
            },
            _ => Ok(None),
        }
    }

    fn expect_semi_colon(&mut self, message: String) -> Result<(), LoxError> {
        match self.is_match(&[TokenType::Semicolon])? {
            Some(_) => Ok(()),
            None => Err(LoxError::scanner(self.scanner.line(), message)),
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
            self.next = None;
        } else {
            self.scanner.next()?;
        }
        Ok(())
    }

    fn peek(&mut self) -> Result<Option<Token>, LoxError> {
        if matches!(self.next, None) {
            self.next = self.read_next()?;
        }
        Ok(self.next.clone())
    }

    fn read_next(&mut self) -> Result<Option<Token>, LoxError> {
        self.scanner.next()
    }

    fn advance(&mut self) -> Result<Option<Token>, LoxError> {
        match self.next.clone() {
            Some(token) => {
                self.next = None;
                Ok(Some(token))
            }
            _ => Ok(self.read_next()?),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ast::{Expr, LiteralExpr, Stmt, VarExpr},
        lox_error::LoxResult,
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

        let res = parser.expect_semi_colon(format!(""));
        assert!(res.is_ok())
    }

    #[test]
    fn expext_semi_colon_failure() {
        let s = TestScanner::new(vec![token(TokenType::Minus)]);
        let mut parser = Parser::new(Box::new(s));

        let res = parser.expect_semi_colon(format!(""));
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

}
