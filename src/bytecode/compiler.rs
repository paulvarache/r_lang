use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::rc::Rc;

use crate::error::CompilerError;
use crate::error::CompilerErrorCode;
use crate::error::LoxError;
use crate::error::LoxResult;
use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;
use crate::scanner::Scan;

use super::class_compiler::ClassCompiler;
use super::class_compiler::ClassCompilerLink;
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
    class_compiler: ClassCompilerLink,
    sourcemaps: HashMap<usize, Sourcemap>,
    use_declarations_finished: bool,
    enums: HashMap<String, Vec<String>>,
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
            sourcemaps: HashMap::new(),
            class_compiler: None,
            use_declarations_finished: false,
            enums: HashMap::new(),
        }
    }
    pub fn compile(&mut self) -> Result<Function, Vec<LoxError>> {
        let mut errors = Vec::new();

        loop {
            match self.peek() {
                Err(err) => {
                    errors.push(err);
                    return Err(errors);
                }
                Ok(next) => {
                    if next.ttype == TokenType::Eof {
                        break;
                    }
                    let result = self.declaration();
                    if let Err(err) = result {
                        errors.push(err);
                        if let Err(err) = self.synchronise() {
                            errors.push(err);
                            return Err(errors);
                        }
                    }
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
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
                        | TokenType::Let
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
            TokenType::OpenBrace => ParseRule::default(),
            TokenType::CloseBrace => ParseRule::default(),
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
            TokenType::Equal => ParseRule::default(),
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
                prefix: Some(Compiler::identifier),
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
            TokenType::And => ParseRule {
                prefix: None,
                infix: Some(Compiler::and),
                precedence: Precedence::And,
            },
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
            TokenType::Super => ParseRule {
                prefix: Some(Compiler::super_),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::This => ParseRule {
                prefix: Some(Compiler::this),
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::True => literal_rule!(),
            TokenType::Let => todo!(),
            TokenType::While => todo!(),
            TokenType::Eof => ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            TokenType::Colon => todo!(),
            TokenType::Use => todo!(),
            TokenType::Undefined => panic!("undefined token for rule"),
            TokenType::Assert => todo!(),
            TokenType::OpenSqr => ParseRule {
                prefix: None,
                infix: Some(Compiler::index),
                precedence: Precedence::Call,
            },
            TokenType::CloseSqr => ParseRule::default(),
            TokenType::Enum => ParseRule::default(),
            TokenType::Backtick => ParseRule {
                prefix: Some(Compiler::template_literal),
                infix: None,
                precedence: Precedence::None,
            },
        }
    }
    // declaration -> class_declaration
    //              | function_declaration
    //              | enum_declaration
    //              | var_declaration
    //              | use_declaration
    //              | statement;
    fn declaration(&mut self) -> LoxResult<()> {
        match self.peek()?.ttype {
            TokenType::Use => self.use_declaration(),
            TokenType::Class => {
                self.use_declarations_finished = true;
                self.class_declaration()
            }
            TokenType::Fun => {
                self.use_declarations_finished = true;
                self.fun_declaration()
            }
            TokenType::Let => {
                self.use_declarations_finished = true;
                self.var_declaration()
            }
            TokenType::Enum => {
                self.use_declarations_finished = true;
                self.enum_declaration()
            }
            _ => {
                self.use_declarations_finished = true;
                self.statement()
            }
        }
    }
    fn use_declaration(&mut self) -> LoxResult<()> {
        if self.use_declarations_finished {
            let err = self.error(CompilerErrorCode::NonHeaderUse);
            self.skip()?; // Consume the use token, or synchronisation will loop on that error
            return Err(err);
        }
        let use_token = self.advance()?;

        let mut names = Vec::new();

        loop {
            let next = self.peek()?;
            if next.ttype == TokenType::Identifier {
                self.skip()?;
                names.push(next.lexeme);
                let next = self.peek()?;
                match next.ttype {
                    TokenType::Semicolon => {
                        self.skip()?;
                        break;
                    }
                    TokenType::Colon => {
                        self.skip()?;
                        self.consume(TokenType::Colon, CompilerErrorCode::MissingUseColon)?;
                    }
                    _ => {
                        return Err(self.error(CompilerErrorCode::UnterminatedUse))?;
                    }
                }
            } else {
                return Err(self.error(CompilerErrorCode::UnterminatedUse))?;
            }
        }

        let fn_name = names.last().unwrap().clone();

        let const_addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&Token::new(
                TokenType::Identifier,
                fn_name,
                Span::new_from_range(use_token.span, self.last.span),
            ));

        self.named_variable(
            &Token::new(
                TokenType::Identifier,
                names.join("::"),
                Span::new_from_range(use_token.span, self.last.span),
            ),
            false,
        )?;

        if const_addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }

        self.emit_bytes(
            OpCode::DefineGlobal,
            const_addr as u8,
            Span::new_from_range(use_token.span, self.last.span),
        );

        Ok(())
    }
    fn assert_statement(&mut self) -> LoxResult<()> {
        let assert_keywork = self.advance()?;
        let addr;
        self.expression()?;
        if self.peek()?.ttype == TokenType::Comma {
            self.skip()?;
            self.expression()?;
        } else {
            addr = self
                .function_compiler
                .borrow_mut()
                .get_assert_default_msg_addr();
            self.emit_constant_from_addr(addr, assert_keywork.span);
        }
        self.consume(
            TokenType::Semicolon,
            CompilerErrorCode::MissingSemicolonAfterAssertStatement,
        )?;
        self.emit(
            OpCode::Assert,
            Span::new_from_range(assert_keywork.span, self.last.span),
        );
        Ok(())
    }
    fn class_declaration(&mut self) -> LoxResult<()> {
        let class_token = self.advance()?;
        let name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingIdentifierAfterClassKeyword,
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

        if class_name_addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }

        self.emit_bytes(OpCode::Class, class_name_addr as u8, span);

        self.function_compiler
            .borrow_mut()
            .define_variable(class_name_addr, span);

        let current = self.class_compiler.take();

        self.class_compiler
            .replace(Box::new(ClassCompiler::new(current)));

        let next = self.peek()?;
        if next.ttype == TokenType::Less {
            self.skip()?;
            self.consume(
                TokenType::Identifier,
                CompilerErrorCode::MissingSuperclassName,
            )?;
            self.variable(false)?;
            if name.lexeme == self.last.lexeme {
                return Err(self.error(CompilerErrorCode::InheritFromSelf));
            }

            self.begin_scope();
            let res = self.function_compiler.borrow_mut().add_local(&Token::new(
                TokenType::Super,
                "super".to_string(),
                Span::default(),
            ));

            if let Err(code) = res {
                return Err(self.error(code));
            }

            self.function_compiler
                .borrow_mut()
                .define_variable(0, Span::default());

            self.named_variable(&name, false)?;
            self.emit(OpCode::Inherit, next.span);

            // Setting the value here.
            // Take ownership of the conpiler to be able to write to it
            // Then put it back if there was a value to start with

            let current = self.class_compiler.take();
            if let Some(mut class_compiler) = current {
                class_compiler.has_superclass = true;
                self.class_compiler.replace(class_compiler);
            }
        }

        self.named_variable(&name, false)?;

        self.consume(
            TokenType::OpenBrace,
            CompilerErrorCode::MissingOpenBraceAfterClassName,
        )?;

        loop {
            let next = self.peek()?;
            if next.ttype == TokenType::CloseBrace {
                break;
            }
            self.method()?;
        }

        self.consume(
            TokenType::CloseBrace,
            CompilerErrorCode::MissingOpenBraceAfterClassDeclaration,
        )?;

        self.emit(
            OpCode::Pop,
            Span::new_from_range(class_token.span, self.last.span),
        );

        let current = self.class_compiler.take();

        if let Some(class_compiler) = current {
            if class_compiler.has_superclass {
                self.end_scope();
            }
            if let Some(enclosing) = class_compiler.enclosing {
                self.class_compiler.replace(enclosing);
            }
        }

        Ok(())
    }
    fn method(&mut self) -> LoxResult<()> {
        let name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingIdentifierAfterFunKeyword,
        )?;
        let const_addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&name);
        let func_type = if name.lexeme == "init" {
            FunctionType::Initializer
        } else {
            FunctionType::Method
        };
        self.function(func_type, &name)?;

        if const_addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }

        self.emit_bytes(OpCode::Method, const_addr as u8, name.span);
        Ok(())
    }
    fn fun_declaration(&mut self) -> LoxResult<()> {
        let fun_token = self.advance()?;
        let (const_addr, name) =
            self.parse_variable(CompilerErrorCode::FunctionCallToManyArguments)?; //TODO: real error code
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
            CompilerErrorCode::MissingOpenParenAfterFunIdentifier,
        )?;
        if self.peek()?.ttype != TokenType::CloseParen {
            loop {
                if self.function_compiler.borrow().arity() == 255 {
                    return Err(self.error(CompilerErrorCode::FunctionCallToManyArguments));
                }
                self.function_compiler.borrow_mut().increase_arity();
                let (addr, name) =
                    self.parse_variable(CompilerErrorCode::UnterminatedArgumentList)?;
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
            CompilerErrorCode::MissingClosingParenAfterArgumentList,
        )?;
        self.consume(
            TokenType::OpenBrace,
            CompilerErrorCode::MissingOpenBraceAfterFunctionDefinition,
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
        if const_addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }
        self.emit_bytes(OpCode::Closure, const_addr as u8, span);

        for upvalue in &function_compiler.upvalues {
            if upvalue.1 > u8::MAX.into() {
                return Err(self.error(CompilerErrorCode::TooManyGlobals));
            }
            self.emit_bytes(if upvalue.0 { 1 } else { 0 }, upvalue.1 as u8, span);
        }

        Ok(())
    }
    fn var_declaration(&mut self) -> LoxResult<()> {
        let keywork_token = self.advance()?;
        // Add the name of the variable to the constants table, remember the address and span
        // to define it later
        let (var_addr, name) =
            self.parse_variable(CompilerErrorCode::MissingIdentifierAfterVarKeyword)?;
        let token = self.peek()?;
        if token.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
        } else {
            self.emit(OpCode::Nil, token.span);
        }
        self.consume(
            TokenType::Semicolon,
            CompilerErrorCode::MissingSemicolonOrEqualAfterVarDeclaration,
        )?;
        let span = Span::new_from_range(keywork_token.span, self.last.span);
        // Use the name constant address and span to emit the global define bytecode
        self.function_compiler
            .borrow_mut()
            .define_variable(var_addr, span);
        Ok(())
    }
    fn enum_declaration(&mut self) -> LoxResult<()> {
        self.skip()?;
        let mut values = Vec::new();
        if !matches!(
            self.function_compiler.borrow().function_type,
            FunctionType::Script
        ) {
            return Err(self.error(CompilerErrorCode::NonTopLevelEnum));
        }

        let name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingEnumIdentifier,
        )?;
        self.consume(
            TokenType::OpenBrace,
            CompilerErrorCode::MissingEnumOpenBrace,
        )?;

        while self.peek()?.ttype != TokenType::CloseBrace {
            let variant =
                self.consume(TokenType::Identifier, CompilerErrorCode::MissingEnumVariant)?;
            values.push(variant.lexeme.clone());
            self.consume(TokenType::Comma, CompilerErrorCode::MissingEnumVariantComma)?;
        }

        self.consume(
            TokenType::CloseBrace,
            CompilerErrorCode::MissingEnumClosingBrace,
        )?;

        self.enums.insert(name.lexeme.clone(), values);

        Ok(())
    }
    fn parse_variable(&mut self, code: CompilerErrorCode) -> LoxResult<(u32, Token)> {
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
    fn identifier(&mut self, can_assign: bool) -> LoxResult<()> {
        let name = self.last.lexeme.clone();
        if self.enums.contains_key(&name) {
            self.enumm(&name)
        } else {
            self.named_variable(&self.last.clone(), can_assign)
        }
    }
    fn enumm(&mut self, name: &String) -> LoxResult<()> {
        let name_span = self.last.span;
        self.consume(TokenType::Dot, CompilerErrorCode::MissingEnumDot)?;
        let variant_name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingEnumAccessVariantName,
        )?;
        if let Some(enum_values) = self.enums.get(name) {
            if let Some(val) = enum_values.iter().position(|n| variant_name.lexeme.eq(n)) {
                self.emit_constant(
                    Value::Number(val as f64),
                    Span::new_from_range(name_span, self.last.span),
                );
            } else {
                return Err(self.error(CompilerErrorCode::UndefinedEnumVariant));
            }
        }
        Ok(())
    }
    fn variable(&mut self, can_assign: bool) -> LoxResult<()> {
        self.named_variable(&self.last.clone(), can_assign)
    }
    fn named_variable(&mut self, name: &Token, can_assign: bool) -> LoxResult<()> {
        let get_op;
        let set_op;
        let addr: u32;
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
            self.emit(addr as u8, name.span);
        } else {
            self.emit_bytes(get_op, addr as u8, name.span);
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
            TokenType::Assert => self.assert_statement(),
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
        self.consume(TokenType::CloseBrace, CompilerErrorCode::UnterminatedBlock)?;
        Ok(())
    }
    fn print_statement(&mut self) -> LoxResult<()> {
        let print_token = self.advance()?;
        self.expression()?;
        self.consume(
            TokenType::Semicolon,
            CompilerErrorCode::MissingSemicolonAfterPrintStatement,
        )?;
        self.emit(OpCode::Print, print_token.span);
        Ok(())
    }
    fn if_statement(&mut self) -> LoxResult<()> {
        let if_token = self.advance()?;
        self.expression()?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse, if_token.span);
        self.emit(OpCode::Pop, if_token.span);

        if self.last.lexeme == "Syscall3" {
            self.branch()?;
        } else {
            self.branch()?;
        }
        let else_jump = self.emit_jump(OpCode::Jump, if_token.span);
        self.patch_jump(then_jump)?;
        self.emit(OpCode::Pop, if_token.span);
        if self.peek()?.ttype == TokenType::Else {
            self.skip()?;
            if self.peek()?.ttype == TokenType::If {
                self.if_statement()?;
            } else {
                self.branch()?;
            }
        }
        self.patch_jump(else_jump)?;

        Ok(())
    }
    fn branch(&mut self) -> LoxResult<()> {
        self.consume(
            TokenType::OpenBrace,
            CompilerErrorCode::MissingOpenBraceAfterIf,
        )?;
        self.begin_scope();
        self.block()?;
        self.end_scope();
        Ok(())
    }
    fn return_statement(&mut self) -> LoxResult<()> {
        let return_token = self.advance()?;
        if matches!(
            self.function_compiler.borrow().function_type,
            FunctionType::Script
        ) {
            return Err(self.error(CompilerErrorCode::TopLevelReturn));
        }
        let next = self.peek()?;
        if next.ttype == TokenType::Semicolon {
            let semicolon_token = self.advance()?;
            self.emit_return(Span::new_from_range(
                return_token.span,
                semicolon_token.span,
            ));
        } else {
            if matches!(
                self.function_compiler.borrow().function_type,
                FunctionType::Initializer
            ) {
                return Err(self.error(CompilerErrorCode::InitializerReturnValue));
            }
            self.expression()?;
            let semicolon_token = self.consume(
                TokenType::Semicolon,
                CompilerErrorCode::MissingSemicolonAfterReturnStatement,
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
        self.expression()?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse, while_token.span);
        self.emit(OpCode::Pop, while_token.span);
        self.branch()?;
        self.emit_loop(loop_start, while_token.span)?;

        self.patch_jump(exit_jump)?;
        self.emit(OpCode::Pop, while_token.span);

        Ok(())
    }
    fn for_statement(&mut self) -> LoxResult<()> {
        let for_token = self.advance()?;
        self.begin_scope();
        match self.peek()?.ttype {
            TokenType::Semicolon => {}
            TokenType::Let => self.var_declaration()?,
            _ => self.expression()?,
        }

        let mut loop_start = self.function_compiler.borrow().current_addr();

        let mut exit_jump = None;

        if self.peek()?.ttype != TokenType::Semicolon {
            self.expression()?;
            self.consume(
                TokenType::Semicolon,
                CompilerErrorCode::MissingSemicolonAfterForIteration,
            )?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse, for_token.span));
            self.emit(OpCode::Pop, for_token.span);
        }

        if self.peek()?.ttype != TokenType::OpenBrace {
            let body_jump = self.emit_jump(OpCode::Jump, for_token.span);
            let increment_start = self.function_compiler.borrow().current_addr();
            self.expression()?;
            self.emit(OpCode::Pop, for_token.span);
            self.consume(
                TokenType::OpenBrace,
                CompilerErrorCode::MissingClosingParenAfterFor,
            )?;

            self.emit_loop(loop_start, for_token.span)?;
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        } else {
            self.skip()?;
        }

        self.begin_scope();
        self.block()?;
        self.end_scope();
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
            CompilerErrorCode::MissingSemicolonAfterExpressionStatement,
        )?;
        self.emit(OpCode::Pop, token.span);
        Ok(())
    }
    fn expression(&mut self) -> LoxResult<()> {
        self.parse_precedence(Precedence::Assignment, CompilerErrorCode::UnterminatedBlock)?;
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
    fn emit_constant_from_addr(&mut self, addr: u32, span: Span) {
        self.function_compiler
            .borrow_mut()
            .emit_constant_from_addr(addr, span);
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
    fn consume(&mut self, ttype: TokenType, code: CompilerErrorCode) -> LoxResult<Token> {
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
            CompilerErrorCode::MissingClosingParenAfterGroup,
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
                    return Err(self.error(CompilerErrorCode::FunctionCallToManyArguments));
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
            CompilerErrorCode::MissingClosingParenAfterArgumentList,
        )?;
        Ok(arg_count)
    }
    fn dot(&mut self, can_assign: bool) -> LoxResult<()> {
        let name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingIdentifierAfterCallDot,
        )?;
        let const_addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&name);

        if const_addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }

        let next = self.peek()?;
        if can_assign && next.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit_bytes(OpCode::PropertySet, const_addr as u8, self.last.span);
        } else if next.ttype == TokenType::OpenParen {
            self.skip()?;
            let arg_count = self.argument_list()?;
            let span = Span::new_from_range(name.span, self.last.span);
            self.emit_bytes(OpCode::Invoke, const_addr as u8, span);
            self.emit(arg_count, span);
        } else {
            self.emit_bytes(OpCode::PropertyGet, const_addr as u8, self.last.span);
        }
        Ok(())
    }
    fn index(&mut self, can_assign: bool) -> LoxResult<()> {
        let open_square_span = self.last.span;
        self.expression()?;
        self.consume(
            TokenType::CloseSqr,
            CompilerErrorCode::MissingClosingSqrAfterIndex,
        )?;
        let next = self.peek()?;
        if can_assign && next.ttype == TokenType::Equal {
            self.skip()?;
            self.expression()?;
            self.emit(
                OpCode::IndexSet,
                Span::new_from_range(open_square_span, self.last.span),
            );
        } else {
            self.emit(
                OpCode::IndexGet,
                Span::new_from_range(open_square_span, self.last.span),
            );
        }

        Ok(())
    }
    fn this(&mut self, _can_assign: bool) -> LoxResult<()> {
        if self.class_compiler.is_none() {
            return Err(self.error(CompilerErrorCode::ThisOutsideMethod));
        }
        self.variable(false)
    }
    fn super_(&mut self, _can_assign: bool) -> LoxResult<()> {
        if let Some(class_compiler) = &self.class_compiler {
            if !class_compiler.has_superclass {
                return Err(self.error(CompilerErrorCode::SuperOutsideChildClass));
            }
        } else {
            return Err(self.error(CompilerErrorCode::SuperOutsideClass));
        }
        let super_token = self.last.clone();
        self.consume(
            TokenType::Dot,
            CompilerErrorCode::MissingDotAfterSuperKeyword,
        )?;
        let name = self.consume(
            TokenType::Identifier,
            CompilerErrorCode::MissingIdentiferAfterSuperDot,
        )?;
        let addr = self
            .function_compiler
            .borrow_mut()
            .identifer_constant(&name);

        if addr > u8::MAX.into() {
            return Err(self.error(CompilerErrorCode::TooManyGlobals));
        }

        self.named_variable(
            &Token::new(TokenType::This, "this".to_string(), Span::default()),
            false,
        )?;
        if self.peek()?.ttype == TokenType::OpenParen {
            self.skip()?;
            let arg_count = self.argument_list()?;
            self.named_variable(
                &Token::new(TokenType::Super, "super".to_string(), Span::default()),
                false,
            )?;
            self.emit_bytes(
                OpCode::SuperInvoke,
                addr as u8,
                Span::new_from_range(super_token.span, self.last.span),
            );
            self.emit(
                arg_count,
                Span::new_from_range(super_token.span, self.last.span),
            );
        } else {
            self.named_variable(
                &Token::new(TokenType::Super, "super".to_string(), Span::default()),
                false,
            )?;
            self.emit_bytes(
                OpCode::SuperGet,
                addr as u8,
                Span::new_from_range(super_token.span, self.last.span),
            );
        }
        Ok(())
    }
    fn template_literal(&mut self, can_assign: bool) -> LoxResult<()> {
        let mut string_count = 0;
        loop {
            let next = self.peek()?;
            match next.ttype {
                TokenType::Backtick => {
                    self.skip()?;
                    self.emit_bytes(OpCode::StrConcat, string_count as u8, Span::default());
                    break;
                }
                TokenType::OpenBrace => {
                    string_count += 1;
                    self.skip()?;
                    self.expression()?;
                    self.consume(
                        TokenType::CloseBrace,
                        CompilerErrorCode::MissingClosingBraceAfterTemplateLiteralValue,
                    )?;
                }
                TokenType::String => {
                    string_count += 1;
                    self.skip()?;
                    self.string(can_assign)?;
                }
                _ => {}
            }
        }
        Ok(())
    }
    fn unary(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        self.parse_precedence(
            Precedence::Unary,
            CompilerErrorCode::MissingUnaryRightHandSide,
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
            self.parse_precedence(p, CompilerErrorCode::MissingTermRightHandSide)?;
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
    fn and(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        let else_jump = self.emit_jump(OpCode::JumpIfFalse, token.span);

        self.emit(OpCode::Pop, token.span);
        self.parse_precedence(Precedence::And, CompilerErrorCode::MissingTermRightHandSide)?;

        self.patch_jump(else_jump)?;

        Ok(())
    }
    fn or(&mut self, _can_assign: bool) -> LoxResult<()> {
        let token = self.last.clone();
        let else_jump = self.emit_jump(OpCode::JumpIfFalse, token.span);
        let end_jump = self.emit_jump(OpCode::Jump, token.span);

        self.patch_jump(else_jump)?;
        self.emit(OpCode::Pop, token.span);
        self.parse_precedence(Precedence::Or, CompilerErrorCode::MissingTermRightHandSide)?;

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
        let value = token.lexeme.clone();
        let mut res = vec![];
        let mut bytes = value.bytes();
        loop {
            if let Some(c) = bytes.next() {
                match c {
                    b'\\' => match bytes.next() {
                        Some(b'n') => {
                            res.push(b'\n');
                        }
                        Some(b'r') => {
                            res.push(b'\r');
                        }
                        Some(b'\\') => {
                            res.push(b'\\');
                        }
                        Some(b'"') => {
                            res.push(b'"');
                        }
                        Some(b'`') => {
                            res.push(b'`');
                        }
                        Some(n) => {
                            res.push(c);
                            res.push(n);
                        }
                        _ => {}
                    },
                    _ => {
                        res.push(c);
                    }
                }
            } else {
                break;
            }
        }
        let value = String::from_utf8(res).unwrap();
        self.emit_constant(Value::String(value), token.span);
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
    fn parse_precedence(
        &mut self,
        precedence: Precedence,
        code: CompilerErrorCode,
    ) -> LoxResult<()> {
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
            Err(self.error(CompilerErrorCode::InvalidAssignmentTarget))
        } else {
            Ok(())
        }
    }
    fn error(&mut self, code: CompilerErrorCode) -> LoxError {
        let next = self.peek().unwrap(); // Happy to panic if our code errors before reading the first token
        self.report_error(&self.last, &next, code)
    }
    fn report_error(&self, token: &Token, next_token: &Token, code: CompilerErrorCode) -> LoxError {
        LoxError::Compiler(CompilerError {
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
        if matches!(
            self.function_compiler.borrow().function_type,
            FunctionType::Initializer
        ) {
            self.emit_bytes(OpCode::LocalGet, 0, span);
        } else {
            self.emit(OpCode::Nil, span);
        }
        self.emit(OpCode::Return, span);
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
