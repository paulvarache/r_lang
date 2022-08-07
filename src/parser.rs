use crate::{
    ast::*,
    lox_error::LoxError,
    scanner::{Scan, token::Token, token_type::TokenType, value::Value},
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
        match self.is_match(&[TokenType::Identifier])? {
            Some(name) => {
                let mut initializer = Expr::Literal(LiteralExpr { value: Value::Nil });

                match self.is_match(&[TokenType::Equal])? {
                    Some(_) => match self.expression()? {
                        Some(init) => initializer = init,
                        None => {
                            return Err(LoxError::scanner(
                                1,
                                "expected expression after =".to_string(),
                            ))
                        }
                    },
                    None => {}
                };
                self.expect_semi_colon("expected ; after var declaration".to_string())?;

                Ok(Some(Stmt::Var(VarStmt {
                    name,
                    initializer: Box::new(initializer),
                })))
            }
            None => Err(LoxError::scanner(
                1,
                "expected identifer after var".to_string(),
            )),
        }
    }

    // statement -> exprStmt
    //            | printStmt
    //            | block
    pub fn statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.peek()? {
            Some(token) => match token.ttype {
                TokenType::Print => {
                    self.skip()?;
                    self.print_statement()
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
    fn print_statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.expression()? {
            Some(expr) => self
                .expect_semi_colon("expected ; after value".to_string())
                .and(Ok(Some(Stmt::Print(PrintStmt {
                    expression: Box::new(expr),
                })))),
            None => Ok(None),
        }
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

    // expression -> equality
    pub fn expression(&mut self) -> Result<Option<Expr>, LoxError> {
        self.equality()
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
                            Some(c) => Err(LoxError::parser(c, "expected closing paren".to_string())),
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
        scanner::{Scan, token::Token, token_type::TokenType, value::Value},
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
        let result = test_parse_expr(vec![
            token(TokenType::Eof),
        ]);

        assert!(result.is_ok());
        let expr = result.expect("Unexpected fail");
        assert!(expr.is_none())
    }

    #[test]
    fn expext_semi_colon_success() {
        let s = TestScanner::new(vec![
            token(TokenType::Semicolon),
        ]);
        let mut parser = Parser::new(Box::new(s));

        let res = parser.expect_semi_colon(format!(""));
        assert!(res.is_ok())
    }

    #[test]
    fn expext_semi_colon_failure() {
        let s = TestScanner::new(vec![
            token(TokenType::Minus),
        ]);
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
}
