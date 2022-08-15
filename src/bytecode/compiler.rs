use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::ParserError;
use crate::error::ParserErrorCode;
use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::Scan;

use super::chunk::Chunk;
use super::emitter::Emit;
use super::opcode::OpCode;
use super::precedence::Precedence;
use super::value::Value;

type FnPtr<'a> = fn(&mut Compiler<'a>, bool) -> LoxResult<()>;

#[derive(Default)]
struct ParseRule<'a> {
    prefix: Option<FnPtr<'a>>,
    infix: Option<FnPtr<'a>>,
    precedence: Precedence,
}

pub struct Compiler<'a> {
    last: Token,
    next: Option<Token>,
    pub scanner: Box<dyn Scan + 'a>,
    pub emitter: Box<dyn Emit + 'a>,
}

macro_rules! binary_rule {
    ($p:path) => {
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: $p,
        }
    };
}

macro_rules! literal_rule {
    () => {
        ParseRule {
            prefix: Some(Compiler::literal),
            infix: None,
            precedence: Precedence::None,
        }
    };
}

macro_rules! comparison_rule {
    () => {
        ParseRule {
            prefix: None,
            infix: Some(Compiler::binary),
            precedence: Precedence::Comparison,
        }
    };
}

impl<'a> Compiler<'a> {
    pub fn new(scanner: Box<dyn Scan + 'a>, emitter: Box<dyn Emit + 'a>) -> Self {
        Self {
            next: None,
            last: Token::default(),
            scanner,
            emitter,
        }
    }
    pub fn compile(&mut self) -> LoxResult<Box<Chunk>> {
        while self.peek()?.ttype != TokenType::Eof {
            let result = self.declaration();
            if let Err(err) = result {
                self.synchronise()?;
                return Err(err);
            }
        }
        Ok(self.emitter.get_chunk())
    }
    fn synchronise(&mut self) -> LoxResult<()> {
        while self.peek()?.ttype != TokenType::Eof {
            if self.last.ttype == TokenType::Semicolon
                || matches!(
                    self.peek()?.ttype,
                    TokenType::Class
                        | TokenType::Fun
                        | TokenType::Var
                        | TokenType::For
                        | TokenType::If
                        | TokenType::While
                        | TokenType::Print
                        | TokenType::Return
                )
            {
                break;
            }
            self.skip()?;
        }
        Ok(())
    }
    fn get_rule(&self, ttype: TokenType) -> ParseRule<'a> {
        match ttype {
            TokenType::OpenParen => ParseRule {
                prefix: Some(Compiler::grouping),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::CloseParen => ParseRule::default(),
            TokenType::OpenBrace => todo!(),
            TokenType::CloseBrace => todo!(),
            TokenType::Comma => todo!(),
            TokenType::Dot => todo!(),
            TokenType::Minus => ParseRule {
                prefix: Some(Compiler::unary),
                infix: Some(Compiler::binary),
                precedence: Precedence::Term,
            },
            TokenType::Plus => binary_rule!(Precedence::Term),
            TokenType::Semicolon => ParseRule::default(),
            TokenType::Slash => binary_rule!(Precedence::Factor),
            TokenType::Star => binary_rule!(Precedence::Factor),
            TokenType::Bang => ParseRule {
                prefix: Some(Compiler::unary),
                infix: None,
                precedence: Precedence::Term,
            },
            TokenType::BangEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Equal => todo!(),
            TokenType::EqualEqual => ParseRule {
                prefix: None,
                infix: Some(Compiler::binary),
                precedence: Precedence::Equality,
            },
            TokenType::Greater => comparison_rule!(),
            TokenType::GreaterEqual => comparison_rule!(),
            TokenType::Less => comparison_rule!(),
            TokenType::LessEqual => comparison_rule!(),
            TokenType::Identifier => ParseRule {
                prefix: Some(Compiler::variable),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::String => ParseRule {
                prefix: Some(Compiler::string),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Number => ParseRule {
                prefix: Some(Compiler::number),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::And => todo!(),
            TokenType::Class => todo!(),
            TokenType::Else => todo!(),
            TokenType::False => literal_rule!(),
            TokenType::Fun => todo!(),
            TokenType::For => todo!(),
            TokenType::If => todo!(),
            TokenType::Nil => literal_rule!(),
            TokenType::Or => todo!(),
            TokenType::Print => todo!(),
            TokenType::Return => todo!(),
            TokenType::Super => todo!(),
            TokenType::This => todo!(),
            TokenType::True => literal_rule!(),
            TokenType::Var => todo!(),
            TokenType::While => todo!(),
            TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Undefined => panic!("undefined token for rule"),
        }
    }
    // declaration -> class_declaration
    //              | function_declaration
    //              | var_declaration
    //              | statement;
    fn declaration(&mut self) -> LoxResult<()> {
        match self.peek()?.ttype {
            TokenType::Var => self.var_declaration(),
            _ => self.statement(),
        }
    }
    fn var_declaration(&mut self) -> LoxResult<()> {
        let keywork_token = self.advance()?;
        // Add the name of the variable to the constants table, remember the address and span
        // to define it later
        let (var_addr, var_span) =
            self.parse_variable(ParserErrorCode::MissingIdentifierAfterVarKeyword)?;
        let token = self.peek()?;
        if token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
        } else {
            self.emit(OpCode::Nil.into(), token.span);
        }
        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonOrEqualAfterVarDeclaration,
        )?;
        // Use the name constant address and span to emit the global define bytecode
        self.define_variable(var_addr, keywork_token.span, var_span);
        Ok(())
    }
    fn parse_variable(&mut self, code: ParserErrorCode) -> LoxResult<(u8, Span)> {
        let name = self.consume(TokenType::Identifier, code)?;

        Ok((self.identifer_constant(&name), name.span))
    }
    fn define_variable(&mut self, const_addr: u8, keyword_span: Span, name_span: Span) {
        self.emit(OpCode::DefineGlobal.into(), keyword_span);
        self.emit(const_addr, name_span)
    }
    fn variable(&mut self, can_assign: bool) -> LoxResult<()> {
        self.named_variable(&self.last.clone(), can_assign)
    }
    fn named_variable(&mut self, name: &Token, can_assign: bool) -> LoxResult<()> {
        let addr = self.identifer_constant(&name);
        let token = self.peek()?;
        if can_assign && token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit(OpCode::GlobalSet.into(), token.span);
            self.emit(addr, name.span);
        } else {
            self.emit_bytes(OpCode::GlobalGet.into(), addr, name.span);
        }
        Ok(())
    }
    fn identifer_constant(&mut self, name: &Token) -> u8 {
        let value = Value::String(name.lexeme.clone());
        self.emitter.make_constant(value)
    }
    // statement -> expression_statement
    //            | for_statement
    //            | if_statement
    //            | print_statement
    //            | return_statement
    //            | while_statement
    //            | block;
    fn statement(&mut self) -> LoxResult<()> {
        let token = self.peek()?;
        match token.ttype {
            TokenType::Print => self.print_statement(),
            _ => self.expression_statement(),
        }
    }
    fn print_statement(&mut self) -> LoxResult<()> {
        let print_token = self.advance()?;
        self.expression()?;
        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonAfterPrintStatement,
        )?;
        self.emit(OpCode::Print.into(), print_token.span);
        Ok(())
    }
    fn expression_statement(&mut self) -> LoxResult<()> {
        self.expression()?;
        let token = self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonAfterExpressionStatement,
        )?;
        self.emit(OpCode::Pop.into(), token.span);
        Ok(())
    }
    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment, ParserErrorCode::UnterminatedBlock)?;
        Ok(())
    }
    fn emit(&mut self, byte: u8, span: Span) {
        self.emitter.emit(byte, span)
    }
    fn emit_bytes(&mut self, a: u8, b: u8, span: Span) {
        self.emit(a, span);
        self.emit(b, span);
    }
    fn emit_constant(&mut self, value: Value, span: Span) {
        self.emitter.emit_constant(value, span);
    }
    fn peek(&mut self) -> LoxResult<Token> {
        if let Some(token) = &self.next {
            Ok(token.clone())
        } else {
            let next_token = self.scanner.next()?.expect("trying to read past EOF");
            self.next = Some(next_token.clone());
            Ok(next_token)
        }
    }
    fn advance(&mut self) -> LoxResult<Token> {
        self.last = self.peek()?;
        self.next = None;
        Ok(self.last.clone())
    }
    fn skip(&mut self) -> Result<(), LoxError> {
        self.advance()?;
        Ok(())
    }
    fn consume(&mut self, ttype: TokenType, code: ParserErrorCode) -> LoxResult<Token> {
        let token = self.peek()?;
        if token.ttype == ttype {
            self.skip()?;
            Ok(token)
        } else {
            Err(self.error(code)) // error reporting will grab the correct tokens as we didn't consume anything here
        }
    }
    fn grouping(&mut self, _can_assign: bool) -> LoxResult<()> {
        self.expression()?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterGroup,
        )?;
        Ok(())
    }
    fn unary(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        self.parse_precedence(
            Precedence::Unary,
            ParserErrorCode::MissingUnaryRightHandSide,
        )?;
        match token.ttype {
            TokenType::Minus => self.emit(OpCode::Negate.into(), token.span),
            TokenType::Bang => self.emit(OpCode::Not.into(), token.span),
            _ => unreachable!(),
        }

        Ok(())
    }
    fn binary(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        let rule = self.get_rule(token.ttype.clone());
        if let Some(p) = Precedence::from_index(rule.precedence.get_enum_index() + 1) {
            self.parse_precedence(p, ParserErrorCode::MissingTermRightHandSide)?;
            match token.ttype.clone() {
                TokenType::Plus => self.emit(OpCode::Add.into(), token.span),
                TokenType::Minus => self.emit(OpCode::Subtract.into(), token.span),
                TokenType::Star => self.emit(OpCode::Multiply.into(), token.span),
                TokenType::Slash => self.emit(OpCode::Divide.into(), token.span),
                TokenType::EqualEqual => self.emit(OpCode::Equal.into(), token.span),
                TokenType::BangEqual => {
                    self.emit_bytes(OpCode::Equal.into(), OpCode::Not.into(), token.span)
                }
                TokenType::Greater => self.emit(OpCode::Greater.into(), token.span),
                TokenType::GreaterEqual => {
                    self.emit_bytes(OpCode::Less.into(), OpCode::Not.into(), token.span)
                }
                TokenType::Less => self.emit(OpCode::Less.into(), token.span),
                TokenType::LessEqual => {
                    self.emit_bytes(OpCode::Greater.into(), OpCode::Not.into(), token.span)
                }
                _ => panic!("invalid binary operator {:?}", token.ttype),
            }
        }

        Ok(())
    }
    fn number(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = &self.last;
        let f = token.lexeme.parse::<f64>().unwrap(); // Invalid numbers is handled at scan time
        self.emit_constant(Value::Number(f), token.span);
        Ok(())
    }
    fn string(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = &self.last;
        self.emit_constant(Value::String(token.lexeme.clone()), token.span);
        Ok(())
    }
    fn literal(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = &self.last;
        match token.ttype {
            TokenType::Nil => self.emit(OpCode::Nil.into(), token.span),
            TokenType::True => self.emit(OpCode::True.into(), token.span),
            TokenType::False => self.emit(OpCode::False.into(), token.span),
            _ => {}
        }
        Ok(())
    }
    fn parse_precedence(&mut self, precedence: Precedence, code: ParserErrorCode) -> LoxResult<()> {
        let token = self.advance()?;
        let rule = self.get_rule(token.ttype);
        let prefix = rule.prefix.ok_or_else(|| self.error(code))?;
        let can_assign = precedence <= Precedence::Assignment;
        prefix(self, can_assign)?;

        loop {
            let token = self.peek()?;

            let rule = self.get_rule(token.ttype.clone());
            if precedence <= rule.precedence {
                self.skip()?;
                if let Some(infix) = self.get_rule(token.ttype).infix {
                    infix(self, can_assign)?
                }
            } else {
                break;
            }
        }
        if can_assign && self.peek()?.ttype == TokenType::Equal {
            Err(self.error(ParserErrorCode::InvalidAssignmentTarget))
        } else {
            Ok(())
        }
    }
    fn error(&mut self, code: ParserErrorCode) -> LoxError {
        let next = self.peek().unwrap(); // Happy to panic if our code errors before reading the first token
        self.report_error(&self.last, &next, code)
    }
    fn report_error(&self, token: &Token, next_token: &Token, code: ParserErrorCode) -> LoxError {
        LoxError::Parser(ParserError {
            token: token.clone(),
            next_token: next_token.clone(),
            code,
        })
    }
}
