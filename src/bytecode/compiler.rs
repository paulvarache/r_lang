use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::error::LoxError;
use crate::error::LoxResult;
use crate::error::ParserError;
use crate::error::ParserErrorCode;
use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::Scan;

use super::debug::disassemble_chunk;
use super::function::Function;
use super::function::FunctionType;
use super::function_compiler::FunctionCompiler;
use super::opcode::OpCode;
use super::precedence::Precedence;
use super::sourcemap::Sourcemap;
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
    function_compiler: RefCell<FunctionCompiler>,
    function_type: FunctionType,
    sourcemaps: HashMap<usize, Sourcemap>,
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
    pub fn new(scanner: Box<dyn Scan + 'a>) -> Self {
        Self {
            next: None,
            last: Token::default(),
            scanner,
            function_compiler: RefCell::new(FunctionCompiler::new("__", FunctionType::Script)),
            function_type: FunctionType::Script,
            sourcemaps: HashMap::new(),
        }
    }
    pub fn compile(&mut self) -> LoxResult<Function> {
        while self.peek()?.ttype != TokenType::Eof {
            let result = self.declaration();
            if let Err(err) = result {
                self.synchronise()?;
                return Err(err);
            }
        }
        self.end_compiler();
        let function_compiler = self
            .function_compiler
            .replace(FunctionCompiler::new("__", FunctionType::Script));

        let function = Function::new(
            "__".to_string(),
            function_compiler.chunk,
            0,
            function_compiler.upvalues.len() as u8,
        );

        self.sourcemaps
            .insert(function.id(), function_compiler.sourcemap);

        Ok(function)
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
                infix: Some(Compiler::call),
                precedence: Precedence::Call,
            },
            TokenType::CloseParen => ParseRule::default(),
            TokenType::OpenBrace => todo!(),
            TokenType::CloseBrace => todo!(),
            TokenType::Comma => ParseRule::default(),
            TokenType::Dot => ParseRule {
                prefix: None,
                infix: Some(Compiler::dot),
                precedence: Precedence::Call,
            },
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
            TokenType::Print => ParseRule::default(),
            TokenType::Return => ParseRule::default(),
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
            TokenType::Class => self.class_declaration(),
            TokenType::Fun => self.fun_declaration(),
            TokenType::Var => self.var_declaration(),
            _ => self.statement(),
        }
    }
    fn class_declaration(&mut self) -> LoxResult<()> {
        let class_token = self.advance()?;
        let name = self.consume(
            TokenType::Identifier,
            ParserErrorCode::MissingIdentifierAfterClassKeyword,
        )?;
        let class_name_addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&name);
        let res = self.function_compiler.borrow_mut().declare_variable(&name);
        if let Err(code) = res {
            return Err(self.error(code));
        }

        let span = Span::new_from_range(class_token.span, name.span);

        self.emit_bytes(OpCode::Class, class_name_addr, span);

        self.function_compiler
            .borrow_mut()
            .define_variable(class_name_addr, span);

        self.consume(
            TokenType::OpenBrace,
            ParserErrorCode::MissingOpenBraceAfterClassName,
        )?;
        self.consume(
            TokenType::CloseBrace,
            ParserErrorCode::MissingOpenBraceAfterClassDeclaration,
        )?;

        Ok(())
    }
    fn fun_declaration(&mut self) -> LoxResult<()> {
        let fun_token = self.advance()?;
        let (const_addr, name) =
            self.parse_variable(ParserErrorCode::FunctionCallToManyArguments)?; //TODO: real error code
        self.function_compiler.borrow_mut().mark_initialized();
        self.function(FunctionType::Function, &name)?;
        let span = Span::new_from_range(fun_token.span, self.last.span);
        self.function_compiler
            .borrow_mut()
            .define_variable(const_addr, span);
        Ok(())
    }
    fn function(&mut self, function_type: FunctionType, name: &Token) -> LoxResult<()> {
        let start = self.last.clone();
        let previous = self
            .function_compiler
            .replace(FunctionCompiler::new(&name.lexeme, function_type));

        mem::replace(
            &mut self.function_compiler.borrow_mut().enclosing,
            Some(Box::new(previous)),
        );

        self.begin_scope();

        self.consume(
            TokenType::OpenParen,
            ParserErrorCode::MissingOpenParenAfterFunIdentifier,
        )?;
        if self.peek()?.ttype != TokenType::CloseParen {
            loop {
                if self.function_compiler.borrow().arity() == 255 {
                    return Err(self.error(ParserErrorCode::FunctionCallToManyArguments));
                }
                self.function_compiler.borrow_mut().increase_arity();
                let (addr, name) =
                    self.parse_variable(ParserErrorCode::UnterminatedArgumentList)?;
                let span = Span::new_from_range(start.span, self.last.span);
                self.function_compiler
                    .borrow_mut()
                    .define_variable(addr, span);

                if self.peek()?.ttype != TokenType::Comma {
                    break;
                } else {
                    self.skip()?; // Skip the comma if here
                }
            }
        }
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterArgumentList,
        )?;
        self.consume(
            TokenType::OpenBrace,
            ParserErrorCode::MissingOpenBraceAfterFunctionDefinition,
        )?;

        self.block()?;

        self.end_compiler();

        let previous = self
            .function_compiler
            .borrow_mut()
            .enclosing
            .take()
            .unwrap();

        let function_compiler = self.function_compiler.replace(*previous);

        let arity = function_compiler.arity();

        let function = Function::new(
            name.lexeme.clone(),
            function_compiler.chunk,
            arity,
            function_compiler.upvalues.len() as u8,
        );

        self.sourcemaps
            .insert(function.id(), function_compiler.sourcemap);

        let const_addr = self
            .function_compiler
            .borrow_mut()
            .make_constant(Value::Func(Rc::new(function)));
        let end = self.last.clone();
        let span = Span::new_from_range(start.span, end.span);
        self.emit_bytes(OpCode::Closure, const_addr, span);

        for upvalue in &function_compiler.upvalues {
            self.emit_bytes(if upvalue.0 { 1 } else { 0 }, upvalue.1, span);
        }

        Ok(())
    }
    fn var_declaration(&mut self) -> LoxResult<()> {
        let keywork_token = self.advance()?;
        // Add the name of the variable to the constants table, remember the address and span
        // to define it later
        let (var_addr, name) =
            self.parse_variable(ParserErrorCode::MissingIdentifierAfterVarKeyword)?;
        let token = self.peek()?;
        if token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
        } else {
            self.emit(OpCode::Nil, token.span);
        }
        self.consume(
            TokenType::Semicolon,
            ParserErrorCode::MissingSemicolonOrEqualAfterVarDeclaration,
        )?;
        let span = Span::new_from_range(keywork_token.span, self.last.span);
        // Use the name constant address and span to emit the global define bytecode
        self.function_compiler
            .borrow_mut()
            .define_variable(var_addr, span);
        Ok(())
    }
    fn parse_variable(&mut self, code: ParserErrorCode) -> LoxResult<(u8, Token)> {
        let name = self.consume(TokenType::Identifier, code)?;
        let res = self.function_compiler.borrow_mut().declare_variable(&name);

        if let Err(code) = res {
            return Err(self.error(code));
        }

        if self.function_compiler.borrow().is_local_scope() {
            return Ok((0, name.clone()));
        }

        Ok((
            self.function_compiler
                .borrow_mut()
                .identifer_constant(&name),
            name.clone(),
        ))
    }
    fn variable(&mut self, can_assign: bool) -> LoxResult<()> {
        self.named_variable(&self.last.clone(), can_assign)
    }
    fn named_variable(&mut self, name: &Token, can_assign: bool) -> LoxResult<()> {
        let get_op;
        let set_op;
        let addr: u8;
        let arg = self.function_compiler.borrow().resolve_local(name);
        if let Err(code) = arg {
            return Err(self.error(code));
        }
        if let Ok(Some(arg)) = arg {
            addr = arg;
            get_op = OpCode::LocalGet;
            set_op = OpCode::LocalSet;
        } else {
            let res = self.function_compiler.borrow_mut().resolve_upvalue(&name);
            if let Err(code) = res {
                return Err(self.error(code));
            }
            if let Ok(Some(upvalue_addr)) = res {
                addr = upvalue_addr;
                get_op = OpCode::UpvalueGet;
                set_op = OpCode::UpvalueSet;
            } else {
                addr = self
                    .function_compiler
                    .borrow_mut()
                    .identifer_constant(&name);
                get_op = OpCode::GlobalGet;
                set_op = OpCode::GlobalSet;
            }
        }
        let token = self.peek()?;
        if can_assign && token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit(set_op, token.span);
            self.emit(addr, name.span);
        } else {
            self.emit_bytes(get_op, addr, name.span);
        }
        Ok(())
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
            TokenType::Return => self.return_statement(),
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
        self.emit(OpCode::Print, print_token.span);
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
        self.emit(OpCode::Pop, if_token.span);
        self.statement()?;
        let else_jump = self.emit_jump(OpCode::Jump, if_token.span);
        self.patch_jump(then_jump)?;
        self.emit(OpCode::Pop, if_token.span);
        if self.peek()?.ttype == TokenType::Else {
            self.skip()?;
            self.statement()?;
        }
        self.patch_jump(else_jump)?;

        Ok(())
    }
    fn return_statement(&mut self) -> LoxResult<()> {
        let return_token = self.advance()?;
        if matches!(
            self.function_compiler.borrow().function_type,
            FunctionType::Script
        ) {
            return Err(self.error(ParserErrorCode::TopLevelReturn));
        }
        let next = self.peek()?;
        if next.ttype == TokenType::Semicolon {
            let semicolon_token = self.advance()?;
            self.emit_return(Span::new_from_range(
                return_token.span,
                semicolon_token.span,
            ));
        } else {
            self.expression()?;
            let semicolon_token = self.consume(
                TokenType::Semicolon,
                ParserErrorCode::MissingSemicolonAfterReturnStatement,
            )?;
            self.emit(
                OpCode::Return,
                Span::new_from_range(return_token.span, semicolon_token.span),
            );
        }
        Ok(())
    }
    fn while_statement(&mut self) -> LoxResult<()> {
        let loop_start = self.function_compiler.borrow().current_addr();
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
        self.emit(OpCode::Pop, while_token.span);
        self.statement()?;
        self.emit_loop(loop_start, while_token.span)?;

        self.patch_jump(exit_jump)?;
        self.emit(OpCode::Pop, while_token.span);

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

        let mut loop_start = self.function_compiler.borrow().current_addr();

        let mut exit_jump = None;

        if self.peek()?.ttype != TokenType::Semicolon {
            self.expression()?;
            self.consume(
                TokenType::Semicolon,
                ParserErrorCode::MissingSemicolonAfterForIteration,
            )?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, for_token.span));
            self.emit(OpCode::Pop, for_token.span);
        }

        if self.peek()?.ttype != TokenType::CloseParen {
            let body_jump = self.emit_jump(OpCode::Jump, for_token.span);
            let increment_start = self.function_compiler.borrow().current_addr();
            self.expression()?;
            self.emit(OpCode::Pop, for_token.span);
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
            self.emit(OpCode::Pop, for_token.span);
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
        self.emit(OpCode::Pop, token.span);
        Ok(())
    }
    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment, ParserErrorCode::UnterminatedBlock)?;
        Ok(())
    }
    fn emit<T: Into<u8>>(&mut self, byte: T, span: Span) {
        self.function_compiler.borrow_mut().emit(byte, span);
    }
    fn emit_bytes<T: Into<u8>, U: Into<u8>>(&mut self, a: T, b: U, span: Span) {
        self.function_compiler.borrow_mut().emit_bytes(a, b, span);
    }
    fn emit_constant(&mut self, value: Value, span: Span) {
        self.function_compiler
            .borrow_mut()
            .emit_constant(value, span);
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
    fn call(&mut self, _can_assign: bool) -> LoxResult<()> {
        // Get the span of the last emitted byte (the function name definition)
        let addr = self.function_compiler.borrow().current_addr() - 1;
        let start = self
            .function_compiler
            .borrow()
            .sourcemap
            .locate_byte(addr)
            .map(|s| s.clone())
            .unwrap_or_else(|| Span::default());
        let arg_count = self.argument_list()?;
        let end = self.last.clone();
        self.emit_bytes(
            OpCode::Call,
            arg_count,
            Span::new_from_range(start, end.span),
        );
        Ok(())
    }
    fn argument_list(&mut self) -> LoxResult<u8> {
        let mut arg_count = 0;
        if self.peek()?.ttype != TokenType::CloseParen {
            loop {
                if arg_count == 255 {
                    return Err(self.error(ParserErrorCode::FunctionCallToManyArguments));
                }
                arg_count += 1;
                self.expression()?;

                if self.peek()?.ttype != TokenType::Comma {
                    break;
                } else {
                    self.skip()?;
                }
            }
        }
        self.consume(
            TokenType::CloseParen,
            ParserErrorCode::MissingClosingParenAfterArgumentList,
        )?;
        Ok(arg_count)
    }
    fn dot(&mut self, can_assign: bool) -> LoxResult<()> {
        let dot_token_span = self.last.span;
        let name = self.consume(
            TokenType::Identifier,
            ParserErrorCode::MissingIdentifierAfterCallDot,
        )?;
        let const_addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&name);

        let next = self.peek()?;
        if can_assign && next.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit_bytes(
                OpCode::PropertySet,
                const_addr,
                Span::new_from_range(dot_token_span, self.last.span),
            );
        } else {
            self.emit_bytes(
                OpCode::PropertyGet,
                const_addr,
                Span::new_from_range(dot_token_span, self.last.span),
            );
        }
        Ok(())
    }
    fn unary(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        self.parse_precedence(
            Precedence::Unary,
            ParserErrorCode::MissingUnaryRightHandSide,
        )?;
        match token.ttype {
            TokenType::Minus => self.emit(OpCode::Negate, token.span),
            TokenType::Bang => self.emit(OpCode::Not, token.span),
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
                TokenType::Plus => self.emit(OpCode::Add, token.span),
                TokenType::Minus => self.emit(OpCode::Subtract, token.span),
                TokenType::Star => self.emit(OpCode::Multiply, token.span),
                TokenType::Slash => self.emit(OpCode::Divide, token.span),
                TokenType::EqualEqual => self.emit(OpCode::Equal, token.span),
                TokenType::BangEqual => self.emit_bytes(OpCode::Equal, OpCode::Not, token.span),
                TokenType::Greater => self.emit(OpCode::Greater, token.span),
                TokenType::GreaterEqual => self.emit_bytes(OpCode::Less, OpCode::Not, token.span),
                TokenType::Less => self.emit(OpCode::Less, token.span),
                TokenType::LessEqual => self.emit_bytes(OpCode::Greater, OpCode::Not, token.span),
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
        self.emit(OpCode::Pop, token.span);
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
            TokenType::Nil => self.emit(OpCode::Nil, token.span),
            TokenType::True => self.emit(OpCode::True, token.span),
            TokenType::False => self.emit(OpCode::False, token.span),
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

    pub fn locate_byte(&self, func_id: usize, addr: usize) -> Option<Span> {
        self.sourcemaps
            .get(&func_id)
            .and_then(|sm| sm.locate_byte(addr))
            .map(|s| s.clone())
    }

    fn emit_jump(&mut self, opcode: OpCode, span: Span) -> usize {
        self.function_compiler.borrow_mut().emit_jump(opcode, span)
    }

    fn patch_jump(&mut self, addr: usize) -> LoxResult<()> {
        let res = self.function_compiler.borrow_mut().patch_jump(addr);

        if let Err(code) = res {
            return Err(self.error(code));
        }
        Ok(())
    }

    fn begin_scope(&self) {
        self.function_compiler.borrow_mut().begin_scope();
    }

    fn end_scope(&self) {
        self.function_compiler
            .borrow_mut()
            .end_scope(self.last.span);
    }

    fn emit_loop(&mut self, addr: usize, span: Span) -> LoxResult<()> {
        let res = self.function_compiler.borrow_mut().emit_loop(addr, span);

        if let Err(code) = res {
            return Err(self.error(code));
        }
        Ok(())
    }

    fn emit_return(&mut self, span: Span) {
        self.emit_bytes(OpCode::Nil, OpCode::Return, span);
    }

    fn end_compiler(&mut self) {
        self.emit_return(self.last.span);
        #[cfg(feature = "print_debug_code")]
        {
            let name = if self.function_compiler.borrow().name == "__".to_string() {
                "<script>".to_string()
            } else {
                self.function_compiler.borrow().name.clone()
            };
            disassemble_chunk(&self.function_compiler.borrow().chunk, name);
        }
    }
}
