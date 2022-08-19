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
use super::local::Local;
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
    locals: Vec<Local>,
    scope_depth: usize,
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
            locals: Vec::new(),
            scope_depth: 0,
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
            TokenType::Or => ParseRule {
                prefix: None,
                infix: Some(Compiler::or),
                precedence: Precedence::Or,
            },
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

        self.declare_variable(&name)?;

        if self.scope_depth > 0 {
            return Ok((0, Span::new(0, 0, 0, 0)));
        }

        Ok((self.identifer_constant(&name), name.span))
    }
    fn define_variable(&mut self, const_addr: u8, keyword_span: Span, name_span: Span) {
        if self.scope_depth > 0 {
            if let Some(local) = self.locals.last_mut() {
                local.initialize(self.scope_depth);
            }
            return;
        }
        self.emit(OpCode::DefineGlobal.into(), keyword_span);
        self.emit(const_addr, name_span)
    }
    fn variable(&mut self, can_assign: bool) -> LoxResult<()> {
        self.named_variable(&self.last.clone(), can_assign)
    }
    fn named_variable(&mut self, name: &Token, can_assign: bool) -> LoxResult<()> {
        let get_op;
        let set_op;
        let addr: u8;
        let arg = self.resolve_local(name)?;
        if let Some(arg) = arg {
            addr = arg;
            get_op = OpCode::LocalGet;
            set_op = OpCode::LocalSet;
        } else {
            addr = self.identifer_constant(&name);
            get_op = OpCode::GlobalGet;
            set_op = OpCode::GlobalSet;
        }
        let token = self.peek()?;
        if can_assign && token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit(set_op.into(), token.span);
            self.emit(addr, name.span);
        } else {
            self.emit_bytes(get_op.into(), addr, name.span);
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
            TokenType::If => self.if_statement(),
            TokenType::While => self.while_statement(),
            TokenType::For => self.for_statement(),
            TokenType::OpenBrace => {
                self.skip()?;
                self.begin_scope();
                self.block()?;
                self.end_scope();
                Ok(())
            }
            _ => self.expression_statement(),
        }
    }
    // block -> "{" declaration* "}" ;
    fn block(&mut self) -> LoxResult<()> {
        while self.peek()?.ttype != TokenType::CloseBrace {
            self.declaration()?;
        }
        self.consume(TokenType::CloseBrace, ParserErrorCode::UnterminatedBlock)?;
        Ok(())
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
    fn if_statement(&mut self) -> LoxResult<()> {
        let if_token = self.advance()?;
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterIfKeyword,
        )?;
        self.expression()?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterIfPredicate,
        )?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse, if_token.span);
        self.emit(OpCode::Pop.into(), if_token.span);
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::Jump, if_token.span);
        self.patch_jump(then_jump)?;
        self.emit(OpCode::Pop.into(), if_token.span);
        if self.peek()?.ttype == TokenType::Else {
            self.skip()?;
            self.statement()?;
        }
        self.patch_jump(else_jump)?;

        Ok(())
    }
    fn while_statement(&mut self) -> LoxResult<()> {
        let loop_start = self.emitter.get_chunk().len();
        let while_token = self.advance()?;
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterWhileKeyword,
        )?;
        self.expression()?;
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterWhilePredicate,
        )?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, while_token.span);
        self.emit(OpCode::Pop.into(), while_token.span);
        self.statement()?;
        self.emit_loop(loop_start, while_token.span)?;

        self.patch_jump(exit_jump)?;
        self.emit(OpCode::Pop.into(), while_token.span);

        Ok(())
    }
    fn for_statement(&mut self) -> LoxResult<()> {
        let for_token = self.advance()?;
        self.begin_scope();
        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterWhileKeyword,
        )?;
        match self.peek()?.ttype {
            TokenType::Semicolon => {}
            TokenType::Var => self.var_declaration()?,
            _ => self.expression()?,
        }

        let mut loop_start = self.emitter.get_chunk().len();

        let mut exit_jump = None;

        if self.peek()?.ttype != TokenType::Semicolon {
            self.expression()?;
            self.consume(
                TokenType::Semicolon,
                ParserErrorCode::MissingSemicolonAfterForIteration,
            )?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, for_token.span));
            self.emit(OpCode::Pop.into(), for_token.span);
        }

        if self.peek()?.ttype != TokenType::CloseParen {
            let body_jump = self.emit_jump(OpCode::Jump, for_token.span);
            let increment_start = self.emitter.get_chunk().len();
            self.expression()?;
            self.emit(OpCode::Pop.into(), for_token.span);
            self.consume(
                TokenType::CloseParen,
                ParserErrorCode::MissingClosingParenAfterFor,
            )?;

            self.emit_loop(loop_start, for_token.span)?;
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.statement()?;
        self.emit_loop(loop_start, for_token.span)?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            self.emit(OpCode::Pop.into(), for_token.span);
        }

        self.end_scope();
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
    fn or(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        let else_jump = self.emit_jump(OpCode::JumpIfFalse, token.span);
        let end_jump = self.emit_jump(OpCode::Jump, token.span);

        self.patch_jump(else_jump)?;
        self.emit(OpCode::Pop.into(), token.span);
        self.parse_precedence(Precedence::Or, ParserErrorCode::MissingTermRightHandSide)?;

        self.patch_jump(end_jump)?;

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

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let mut n: u8 = 0;

        // Count how many are in the current depth
        for i in (0..self.locals.len()).rev() {
            if let Some(depth) = self.locals[i].depth {
                if depth < self.scope_depth {
                    break;
                }
            } else {
                break;
            }
            n += 1;
        }

        // Don't emit the extra POPN if there is nothing to pop
        if n != 0 {
            // Pop that many
            self.emit(OpCode::Popn.into(), self.last.span);
            self.emit(n, self.last.span);
        }

        // Remove the locals
        self.locals
            .truncate(self.locals.len().saturating_sub(n as usize));
        self.scope_depth -= 1;
    }

    fn declare_variable(&mut self, name: &Token) -> LoxResult<()> {
        if self.scope_depth == 0 {
            return Ok(());
        }
        for local in self.locals.iter().rev() {
            if let Some(depth) = local.depth {
                if depth < self.scope_depth {
                    break;
                }
            }

            if local.name.lexeme == name.lexeme {
                return Err(self.error(ParserErrorCode::LocalAlreadyDefined));
            }
        }
        self.add_local(name)
    }

    fn add_local(&mut self, name: &Token) -> LoxResult<()> {
        if self.locals.len() == u8::MAX.into() {
            return Err(self.error(ParserErrorCode::TooManyLocals));
        }
        self.locals.push(Local::new(name));
        Ok(())
    }

    fn resolve_local(&mut self, name: &Token) -> LoxResult<Option<u8>> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name.lexeme == name.lexeme {
                if local.depth.is_none() {
                    return Err(self.error(ParserErrorCode::ReadOwnLocalBeforeInitialized));
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    fn emit_jump(&mut self, opcode: OpCode, span: Span) -> usize {
        self.emit(opcode.into(), span);
        self.emit(0xFF, span);
        self.emit(0xFF, span);

        self.emitter.get_chunk().len() - 2
    }

    fn patch_jump(&mut self, addr: usize) -> LoxResult<()> {
        let jump = (self.emitter.get_chunk().len() - addr - 2) as u16;

        if jump > u16::MAX {
            return Err(self.error(ParserErrorCode::JumpTooLong));
        }

        self.emitter.patch(addr, (jump >> 8) as u8 & 0xFF);
        self.emitter.patch(addr + 1, jump as u8 & 0xFF);
        Ok(())
    }

    fn emit_loop(&mut self, addr: usize, span: Span) -> LoxResult<()> {
        self.emit(OpCode::Loop.into(), span);

        let offset = (self.emitter.get_chunk().len() - addr + 2) as u16;
        if offset > u16::MAX {
            return Err(self.error(ParserErrorCode::JumpTooLong));
        }

        self.emit((offset >> 8) as u8 & 0xFF, span);
        self.emit(offset as u8 & 0xFF, span);
        Ok(())
    }
}
