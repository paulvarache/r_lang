use crate::{
    ast::*,
    lox_error::LoxError,
    scanner::{scanner::Scanner, token::Token, token_type::TokenType, value::Value},
};

impl LoxError {
    pub fn parser(token: Token, message: String) -> Self {
        LoxError::Parser { token, message }
    }
}

pub struct Parser<'a> {
    next: Option<Token>,
    scanner: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(scanner: Scanner<'a>) -> Self {
        Self {
            scanner,
            next: None,
        }
    }

    // declaration -> "var" var_declaration
    //              | statement
    pub fn declaration(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.is_match(&[TokenType::Var])? {
            Some(token) => self.var_declaration(),
            None => self.statement(),
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
                        None => return Err(LoxError::scanner(1, format!("expected expression after =")))
                    },
                    None => match self.is_match(&[TokenType::Semicolon])? {
                        Some(_) => {},
                        None => return Err(LoxError::scanner(1, format!("expected ; after var declaration")))
                    }
                };

                Ok(Some(Stmt::Var(VarStmt { name, initializer: Box::new(initializer) })))

            },
            None => Err(LoxError::scanner(1, format!("expected identifer after var")))
        }
    }

    // statement -> "print" expression
    //            | expression
    pub fn statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.is_match(&[TokenType::Print])? {
            Some(_) => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.expression()? {
            Some(expr) => match self.is_match(&[TokenType::Semicolon])? {
                Some(_) => Ok(Some(Stmt::Print(PrintStmt {
                    expression: Box::new(expr),
                }))),
                None => Err(LoxError::scanner(
                    self.scanner.line(),
                    format!("expected ; after value"),
                )),
            },
            None => Ok(None),
        }
    }
    fn expression_statement(&mut self) -> Result<Option<Stmt>, LoxError> {
        match self.expression()? {
            Some(expr) => match self.is_match(&[TokenType::Semicolon])? {
                Some(_) => Ok(Some(Stmt::Expression(ExpressionStmt {
                    expression: Box::new(expr),
                }))),
                None => Err(LoxError::scanner(
                    self.scanner.line(),
                    format!("expected ; after expression"),
                )),
            },
            None => Ok(None),
        }
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
                    value: token.literal.or(Some(Value::Number(0.0))).unwrap(),
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
                            Some(c) => Err(LoxError::parser(c, format!("expected closing paren"))),
                            _ => Err(LoxError::parser(
                                token,
                                format!("expected closing paren got Eof"),
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
                _ => Err(LoxError::parser(token, format!("expected primary",))),
            },
            _ => Ok(None),
        }
    }

    fn is_match(&mut self, types: &[TokenType]) -> Result<Option<Token>, LoxError> {
        for t in types {
            match self.peek()? {
                Some(token) if token.ttype == *t => {
                    return Ok(self.advance()?);
                }
                _ => {}
            }
        }
        return Ok(None);
    }

    fn synchronize(&mut self) -> Result<(), LoxError> {
        // Check the next token
        match self.advance()? {
            Some(token) => match token.ttype {
                // Stop if semi colon
                TokenType::Semicolon => {
                    return Ok(());
                }
                _ => {
                    // peek the next token. If any of the special types, synchro is succesfull
                    // otherwise, advance
                    while let Some(token) = self.peek()? {
                        match token.ttype {
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
                            _ => {
                                self.advance()?;
                            }
                        }
                    }
                }
            },
            _ => {}
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
