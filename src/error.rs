use std::fmt;

use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::value::Value;

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Copy, Clone, Debug)]
pub enum ScannerErrorCode {
    Unknown,
    NumberParsingError,
    UnterminatedString,
}
#[derive(Copy, Clone, Debug)]
pub enum ParserErrorCode {
    MissingIdentifierAfterVarKeyword,
    MissingExpressionAfterVarEqual,
    MissingSemicolonOrEqualAfterVarDeclaration,
    MissingExpressionAfterPrintKeyword,
    MissingSemicolonAfterPrintStatement,
    MissingSemicolonAfterExpressionStatement,
    MissingOpenParenAfterIfKeyword,
    UnterminatedIfPredicate,
    MissingStatementAfterIf,
    MissingStatementAfterElse,
    MissingOpenParenAfterWhileKeyword,
    UnterminatedWhilePredicate,
    MissingClosingParenAfterWhilePredicate,
    MissingStatementAfterWhile,
    MissingOpenParenAfterForKeyword,
    MissingSemicolonAfterForIteration,
    MissingClosingParenAfterFor,
    MissingForBody,
    UnterminatedBlock,
    UnterminatedAssignment,
    InvalidAssignmentTarget,
    UnterminatedLogicalOr,
    UnterminatedLogicalAnd,
    MissingEqualityRightHandSide,
    MissingComparisonRightHandSide,
    MissingTermRightHandSide,
    MissingFactorRightHandSide,
    MissingUnaryRightHandSide,
    MissingClosingParenAfterArgumentList,
    UnterminatedArgumentList,
    UnterminatedGroup,
    MissingClosingParenAfterGroup,
    UnexpectedTokenInExpression,
    FunctionCallToManyArguments,
    MissingIdentifierAfterFunKeyword,
    MissingOpenParenAfterFunIdentifier,
    MissingParameterNameInFunDefinition,
    FunctionDefinitionToManyArguments,
    MissingCommaAfterFunctionParameterName,
    MissingOpenBraceAfterFunctionDefinition,
    MissingExpressionAfterReturnKeyword,
    MissingSemicolonAfterReturnStatement,
    MissingIdentifierAfterClassKeyword,
    MissingOpenBraceAfterClassName,
    MissingIdentifierAfterCallDot,
    MissingSuperclassName,
    MissingDotAfterSuperKeyword,
    MissingIdentiferAfterSuperDot,
    TooManyLocals,
    LocalAlreadyDefined,
    ReadOwnLocalBeforeInitialized,
    MissingClosingParenAfterIfPredicate,
    JumpTooLong,
    TopLevelReturn,
    MissingOpenBraceAfterClassDeclaration,
    ThisOutsideMethod,
    InitializerReturnValue,
}

#[derive(Copy, Clone, Debug)]
pub enum InterpreterErrorCode {
    NumberBinaryExprOperandsIncorrectType,
    PlusExprOperandsIncorrectType,
    UnknownBinaryOperator,
    UnknownLogicalOperator,
    UnaryMinusInvalidType,
    UnaryUnknownOperator,
    AssignToUndefinedVar,
    ReadUndefinedVar,
    NotAFunction,
    FunctionArityMismatch,
    RedefiningLocalVar,
    InitVarWithUnassignedVar,
    ReturnOutsideFunction,
    AccessingNonInstanceProperty,
    InstancePropertyUndefined,
    ReturnInsideConstructor,
    ParentClassIsChildClass,
    ParentClassIsNotClass,
    SuperOutsideClass,
    SuperOutsideSuperclass,
}
#[derive(Debug)]
pub enum CompilerErrorCode {}
#[derive(Debug)]
pub enum RuntimeErrorCode {
    OutOfChunkBounds,
    OutOfConstantsBounds,
    UnaryMinusInvalidType,
    NumberBinaryExprOperandsIncorrectType,
    UndefinedGlobal,
    CallNonFunctionValue,
    FunctionCallArityMismatch,
    UndefinedProperty,
    NonInstancePropertyAccess,
    ClassInitializerArityMismatch,
}

#[derive(Debug)]
pub enum LoxErrorCode {
    Scanner(ScannerErrorCode),
}

impl fmt::Display for LoxErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxErrorCode::Scanner(code) => write!(f, "S{:0>5}", *code as u32),
        }
    }
}

#[derive(Debug)]
pub enum LoxError {
    Scanner(ScannerError),
    Parser(ParserError),
    Interpreter(InterpreterError),
    Compiler(CompilerError),
    Runtime(RuntimeError),
    Return(Value),
}

impl LoxError {
    pub fn span(&self) -> Span {
        match self {
            LoxError::Scanner(e) => e.span,
            LoxError::Parser(e) => e.token.span,
            LoxError::Interpreter(e) => e.span,
            LoxError::Compiler(e) => e.token.span,
            LoxError::Runtime(e) => Span::new(0, 0, 0, 0),
            LoxError::Return(_) => Span::new(0, 0, 0, 0),
        }
    }
}
#[derive(Debug)]
pub struct ScannerError {
    pub span: Span,
    pub next_c: Option<u8>,
    pub code: LoxErrorCode,
}

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub next_token: Token,
    pub code: ParserErrorCode,
}
#[derive(Debug)]
pub struct InterpreterError {
    pub span: Span,
    pub code: InterpreterErrorCode,
}
#[derive(Debug)]
pub struct CompilerError {
    pub token: Token,
    pub next_token: Option<Token>,
    pub code: CompilerErrorCode,
}
impl Demistify for CompilerError {
    fn demistify(&self) -> String {
        match self.code {

        }
    }
}
#[derive(Debug)]
pub struct RuntimeError {
    pub addr: usize,
    pub func_id: usize,
    pub code: RuntimeErrorCode,
}
impl Demistify for RuntimeError {
    fn demistify(&self) -> String {
        match self.code {
            RuntimeErrorCode::OutOfChunkBounds => todo!(),
            RuntimeErrorCode::OutOfConstantsBounds => todo!(),
            RuntimeErrorCode::UnaryMinusInvalidType => "'-' operand only accepts numbers".to_string(),
            RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType => "incompatible types".to_string(),
            RuntimeErrorCode::UndefinedGlobal => "undefined global variable".to_string(),
            RuntimeErrorCode::CallNonFunctionValue => "can only call functions".to_string(),
            RuntimeErrorCode::FunctionCallArityMismatch => "function call mismatch arity".to_string(),
            RuntimeErrorCode::UndefinedProperty => "undefined property".to_string(),
            RuntimeErrorCode::NonInstancePropertyAccess => "only instances have fields".to_string(),
            RuntimeErrorCode::ClassInitializerArityMismatch => "class init arity mismatch".to_string(),
        }
    }
}
pub trait Demistify {
    fn demistify(&self) -> String;
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::Return(_) => Ok(()),
            LoxError::Scanner(err) => {
                writeln!(f, "{}", err.demistify())
            }
            LoxError::Parser(err) => {
                writeln!(f, "{}", err.demistify())
            }
            LoxError::Interpreter(err) => {
                writeln!(f, "{}", err.demistify())
            }
            LoxError::Compiler(err) => writeln!(f, "{}", err.demistify()),
            LoxError::Runtime(err) => writeln!(f, "{}", err.demistify()),
        }
    }
}
