use std::fmt;

use crate::scanner::token::Token;

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Copy, Clone, Debug)]
pub enum ScannerErrorCode {}
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
}

#[derive(Copy, Clone, Debug)]
pub enum InterpreterErrorCode {}

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
    Scanner {
        line: usize,
        message: String,
    },
    Parser(ParserError),
    Interpreter {
        token: Token,
        message: String,
    },
}

impl LoxError {
    pub fn report(&self) {
        eprintln!("{}", self)
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub token: Token,
    pub next_token: Option<Token>,
    pub code: LoxErrorCode,
}

pub trait Demistify {
    fn demistify(&self) -> String;
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::Scanner { line, message } => {
                writeln!(
                    f,
                    "scanner error: {}{}",
                    message,
                    LoxErrorCode::Parser(ParserErrorCode::UnterminatedArgumentList)
                )?;
                write!(f, "{} |", line)
            }
            LoxError::Parser(err) => {
                writeln!(f, "{}", err.demistify());
                write!(f, "{} | {}", err.token.line, err.token.lexeme.clone())
            }
            LoxError::Interpreter { token, message } => {
                writeln!(f, "interpreter error: {}", message)?;
                write!(f, "{} |", token.line)
            }
        }
    }
}
