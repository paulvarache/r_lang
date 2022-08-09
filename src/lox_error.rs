use std::fmt;

use crate::scanner::token::Span;
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
    None,
    MissingIdentifierAfterVarKeyword,
    MissingExpressionAfterVarEqual,
    MissingSemicolonOrEqualAfterVarDeclaration,
    MissingExpressionAfterPrintKeyword,
    MissingSemicolonAfterPrintStatement,
    MissingSemicolonAfterExpressionStatement,
    MissingOpenParenAfterIfKeyword,
    UnterminatedIfPredicate,
    MissingClosingParenAfterIfPredicate,
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
}

#[derive(Debug)]
pub enum LoxErrorCode {
    Scanner(ScannerErrorCode),
    Parser(ParserErrorCode),
    Interpreter(InterpreterErrorCode),
}

impl fmt::Display for LoxErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxErrorCode::Scanner(code) => write!(f, "S{:0>5}", *code as u32),
            LoxErrorCode::Parser(code) => write!(f, "P{:0>5}", *code as u32),
            LoxErrorCode::Interpreter(code) => write!(f, "I{:0>5}", *code as u32),
        }
    }
}

#[derive(Debug)]
pub enum LoxError {
    Scanner(ScannerError),
    Parser(ParserError),
    Interpreter(InterpreterError),
    Return(Value),
}

impl LoxError {
    pub fn report(&self) {
        eprintln!("{}", self)
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
    pub next_token: Option<Token>,
    pub code: ParserErrorCode,
}
#[derive(Debug)]
pub struct InterpreterError {
    pub span: Span,
    pub code: InterpreterErrorCode,
}

pub trait Demistify {
    fn demistify(&self) -> String;
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::Return(_) => Ok(()),
            LoxError::Scanner(err) => {
                writeln!(f, "{}", err.demistify())?;
                write!(
                    f,
                    "({},{}) -> ({}, {}) |",
                    err.span.start.0, err.span.start.1, err.span.end.0, err.span.end.1
                )
            }
            LoxError::Parser(err) => {
                writeln!(f, "{}", err.demistify())?;
                write!(f, "{} | {}", err.token.span.start.0, err.token.lexeme.clone())
            }
            LoxError::Interpreter(err) => {
                writeln!(f, "{}", err.demistify())?;
                write!(
                    f,
                    "({},{}) -> ({}, {}) |",
                    err.span.start.0, err.span.start.1, err.span.end.0, err.span.end.1
                )
            }
        }
    }
}
