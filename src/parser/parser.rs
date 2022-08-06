use crate::{
    ast::*,
    scanner::{scanner::Scanner, token::Token, token_type::TokenType, value::Value},
    LoxError,
};

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

    // expression -> equality
    pub fn expression(&mut self) -> Result<Expr, LoxError> {
        self.equality()
    }

    // equality -> comparison ( ('==' | '!=') comparison )*
    fn equality(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.comparison()?;

        while let Some(token) = self.is_match(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let right = self.comparison()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }
    // comparison -> term ( ('>' | '>=' | '<' | '<=') term )*
    fn comparison(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.term()?;

        while let Some(token) = self.is_match(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let right = self.term()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }
    // term -> factor ( ('-' | '+') factor )*
    fn term(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.factor()?;

        while let Some(token) = self.is_match(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }
    // factor -> unary ( ('/' | '*') unary )*
    fn factor(&mut self) -> Result<Expr, LoxError> {
        let mut expr = self.unary()?;

        while let Some(token) = self.is_match(&[TokenType::Slash, TokenType::Star]) {
            let right = self.unary()?;
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            });
        }
        Ok(expr)
    }
    // unary -> ('!' | '-') unary
    //        | primary
    fn unary(&mut self) -> Result<Expr, LoxError> {
        match self.peek() {
            Some(token) => match token.ttype {
                TokenType::Bang | TokenType::Minus => {
                    self.skip();
                    let right = self.unary()?;
                    Ok(Expr::Unary(UnaryExpr {
                        operator: token,
                        right: Box::new(right),
                    }))
                }
                _ => self.primary(),
            },
            None => Err(LoxError::new(1, format!("expected unary got Eof"))),
        }
    }
    // primary -> NUMBER | STRING | true | false | nil | '(' expression ')'
    fn primary(&mut self) -> Result<Expr, LoxError> {
        match self.advance() {
            Some(token) => match token.ttype {
                TokenType::False => Ok(Expr::Literal(LiteralExpr {
                    value: Value::Bool(false),
                })),
                TokenType::True => Ok(Expr::Literal(LiteralExpr {
                    value: Value::Bool(true),
                })),
                TokenType::Nil => Ok(Expr::Literal(LiteralExpr { value: Value::Nil })),
                TokenType::Number | TokenType::String => Ok(Expr::Literal(LiteralExpr {
                    value: token.literal.or(Some(Value::Number(0.0))).unwrap(),
                })),
                TokenType::OpenParen => {
                    let expr = self.expression()?;
                    let closing = self.advance();
                    match closing {
                        Some(c) if matches!(c.ttype, TokenType::CloseParen) => {
                            Ok(Expr::Grouping(GroupingExpr {
                                expression: Box::new(expr),
                            }))
                        }
                        Some(c) => Err(LoxError::new(
                            c.line,
                            format!("expected closing paren got {}", c.ttype),
                        )),
                        _ => Err(LoxError::new(
                            token.line,
                            format!("expected closing paren got Eof"),
                        )),
                    }
                }
                _ => Err(LoxError::new(
                    token.line,
                    format!("expected primary, got {}", token.ttype),
                )),
            },
            _ => Err(LoxError::new(1, format!("could not get token"))),
        }
    }

    fn is_match(&mut self, types: &[TokenType]) -> Option<Token> {
        for t in types {
            match self.peek() {
                Some(token) if token.ttype == *t => {
                    return self.advance();
                }
                _ => {}
            }
        }
        return None;
    }

    fn synchronize(&mut self) {
        // Check the next token
        match self.advance() {
            Some(token) => match token.ttype {
                // Stop if semi colon
                TokenType::Semicolon => {
                    return;
                }
                _ => {
                    // peek the next token. If any of the special types, synchro is succesfull
                    // otherwise, advance
                    while let Some(token) = self.peek() {
                        match token.ttype {
                            TokenType::Class
                            | TokenType::Fun
                            | TokenType::Var
                            | TokenType::For
                            | TokenType::If
                            | TokenType::While
                            | TokenType::Print
                            | TokenType::Return => {
                                return;
                            }
                            _ => {
                                self.advance();
                            }
                        }
                    }
                }
            },
            _ => {}
        }
    }

    fn skip(&mut self) {
        if !matches!(self.next, None) {
            self.next = None
        } else {
            self.scanner.next().ok();
        }
    }

    fn peek(&mut self) -> Option<Token> {
        if matches!(self.next, None) {
            self.next = self.read_next();
        }
        self.next.clone()
    }

    fn read_next(&mut self) -> Option<Token> {
        self.scanner.next().or::<Token>(Ok(None)).ok().unwrap()
    }

    fn advance(&mut self) -> Option<Token> {
        match self.next.clone() {
            Some(token) => {
                self.next = None;
                Some(token)
            }
            _ => self.read_next(),
        }
    }
}
