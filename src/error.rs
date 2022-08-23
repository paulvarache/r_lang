use std::fmt;

use crate::scanner::span::Span;
use crate::scanner::token::Token;
use crate::scanner::token_type::TokenType;

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Copy, Clone, Debug)]
pub enum ScannerErrorCode {
    Unknown,
    UnterminatedString,
}
#[derive(Copy, Clone, Debug)]
pub enum CompilerErrorCode {
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
    InheritFromSelf,
    SuperOutsideChildClass,
    SuperOutsideClass,
}

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
    NonClassInherit,
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
    Compiler(CompilerError),
    Runtime(RuntimeError),
}

impl LoxError {
    pub fn span(&self) -> Span {
        match self {
            LoxError::Scanner(e) => e.span,
            LoxError::Compiler(e) => e.token.span,
            LoxError::Runtime(e) => Span::new(0, 0, 0, 0),
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
pub struct CompilerError {
    pub token: Token,
    pub next_token: Token,
    pub code: CompilerErrorCode,
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
            RuntimeErrorCode::UnaryMinusInvalidType => {
                "'-' operand only accepts numbers".to_string()
            }
            RuntimeErrorCode::NumberBinaryExprOperandsIncorrectType => {
                "incompatible types".to_string()
            }
            RuntimeErrorCode::UndefinedGlobal => "undefined global variable".to_string(),
            RuntimeErrorCode::CallNonFunctionValue => "can only call functions".to_string(),
            RuntimeErrorCode::FunctionCallArityMismatch => {
                "function call mismatch arity".to_string()
            }
            RuntimeErrorCode::UndefinedProperty => "undefined property".to_string(),
            RuntimeErrorCode::NonInstancePropertyAccess => "only instances have fields".to_string(),
            RuntimeErrorCode::ClassInitializerArityMismatch => {
                "class init arity mismatch".to_string()
            }
            RuntimeErrorCode::NonClassInherit => {
                "cannot inherit from somerthing other than a class".to_string()
            }
        }
    }
}

impl CompilerError {
    fn demistify_next_token(&self) -> String {
        format!(", got {}", self.next_token.demistify())
    }
}

impl Demistify for CompilerError {
    fn demistify(&self) -> String {
        match self.code {
            CompilerErrorCode::MissingIdentifierAfterVarKeyword => {
                format!(
                    "expected identifier after 'var'{}",
                    self.demistify_next_token()
                )
            }
            CompilerErrorCode::MissingExpressionAfterVarEqual => {
                format!(
                    "expected expression after '='{}",
                    self.demistify_next_token()
                )
            }
            CompilerErrorCode::MissingSemicolonOrEqualAfterVarDeclaration => {
                if matches!(self.token.ttype, TokenType::Identifier) {
                    format!(
                        "expected ';' or '=' after variable declaration{}",
                        self.demistify_next_token()
                    )
                } else {
                    format!(
                        "expected ';' after variable declaration{}",
                        self.demistify_next_token()
                    )
                }
            }
            CompilerErrorCode::MissingExpressionAfterPrintKeyword => {
                format!(
                    "expected expression after 'print' keyword{}",
                    self.demistify_next_token()
                )
            }
            CompilerErrorCode::MissingSemicolonAfterPrintStatement => {
                format!(
                    "expected ';' after {}{}",
                    self.token.demistify(),
                    self.demistify_next_token()
                )
            }
            CompilerErrorCode::MissingSemicolonAfterExpressionStatement => {
                format!(
                    "expected ';' after {}{}",
                    self.token.demistify(),
                    self.demistify_next_token()
                )
            }
            CompilerErrorCode::MissingOpenParenAfterIfKeyword => {
                format!("expected '(' after if{}", self.demistify_next_token())
            }
            CompilerErrorCode::UnterminatedIfPredicate => "UnterminatedIfPredicate".to_string(),
            CompilerErrorCode::MissingStatementAfterIf => "MissingStatementAfterIf".to_string(),
            CompilerErrorCode::MissingStatementAfterElse => "MissingStatementAfterElse".to_string(),
            CompilerErrorCode::MissingOpenParenAfterWhileKeyword => {
                "MissingOpenParenAfterWhileKeyword".to_string()
            }
            CompilerErrorCode::UnterminatedWhilePredicate => "UnterminatedWhilePredicate".to_string(),
            CompilerErrorCode::MissingClosingParenAfterWhilePredicate => {
                "MissingClosingParenAfterWhilePredicate".to_string()
            }
            CompilerErrorCode::MissingStatementAfterWhile => "MissingStatementAfterWhile".to_string(),
            CompilerErrorCode::MissingOpenParenAfterForKeyword => {
                "MissingOpenParenAfterForKeyword".to_string()
            }
            CompilerErrorCode::MissingSemicolonAfterForIteration => {
                "MissingSemicolonAfterForIteration".to_string()
            }
            CompilerErrorCode::MissingClosingParenAfterFor => {
                "MissingClosingParenAfterFor".to_string()
            }
            CompilerErrorCode::MissingForBody => "MissingForBody".to_string(),
            CompilerErrorCode::UnterminatedBlock => "UnterminatedBlock".to_string(),
            CompilerErrorCode::UnterminatedAssignment => "UnterminatedAssignment".to_string(),
            CompilerErrorCode::InvalidAssignmentTarget => "InvalidAssignmentTarget".to_string(),
            CompilerErrorCode::UnterminatedLogicalOr => "UnterminatedLogicalOr".to_string(),
            CompilerErrorCode::UnterminatedLogicalAnd => "UnterminatedLogicalAnd".to_string(),
            CompilerErrorCode::MissingEqualityRightHandSide => {
                "MissingEqualityRightHandSide".to_string()
            }
            CompilerErrorCode::MissingComparisonRightHandSide => {
                "MissingComparisonRightHandSide".to_string()
            }
            CompilerErrorCode::MissingTermRightHandSide => "MissingTermRightHandSide".to_string(),
            CompilerErrorCode::MissingFactorRightHandSide => "MissingFactorRightHandSide".to_string(),
            CompilerErrorCode::MissingUnaryRightHandSide => "MissingUnaryRightHandSide".to_string(),
            CompilerErrorCode::MissingClosingParenAfterArgumentList => {
                "MissingClosingParenAfterArgumentList".to_string()
            }
            CompilerErrorCode::UnterminatedArgumentList => "UnterminatedArgumentList".to_string(),
            CompilerErrorCode::UnterminatedGroup => "UnterminatedGroup".to_string(),
            CompilerErrorCode::MissingClosingParenAfterGroup => {
                format!("expected ')' to end group{}", self.demistify_next_token())
            }
            CompilerErrorCode::UnexpectedTokenInExpression => {
                format!("{}, {}", "UnexpectedTokenInExpression", self.token)
            }
            CompilerErrorCode::FunctionCallToManyArguments => {
                "too many arguments passed to function".to_string()
            }
            CompilerErrorCode::MissingIdentifierAfterFunKeyword => {
                "MissingIdentifierAfterFunKeyword".to_string()
            }
            CompilerErrorCode::MissingOpenParenAfterFunIdentifier => {
                "MissingOpenParenAfterFunIdentifier".to_string()
            }
            CompilerErrorCode::MissingParameterNameInFunDefinition => {
                "MissingParameterNameInFunDefinition".to_string()
            }
            CompilerErrorCode::FunctionDefinitionToManyArguments => {
                "FunctionDefinitionToManyArguments".to_string()
            }
            CompilerErrorCode::MissingCommaAfterFunctionParameterName => {
                "MissingCommaAfterFunctionParameterName".to_string()
            }
            CompilerErrorCode::MissingOpenBraceAfterFunctionDefinition => {
                "MissingOpenBraceAfterFunctionDefinition".to_string()
            }
            CompilerErrorCode::MissingExpressionAfterReturnKeyword => {
                "MissingExpressionAfterReturnKeyword".to_string()
            }
            CompilerErrorCode::MissingSemicolonAfterReturnStatement => {
                "MissingSemicolonAfterReturnStatement".to_string()
            }
            CompilerErrorCode::MissingIdentifierAfterClassKeyword => {
                "MissingIdentifierAfterClassKeyword".to_string()
            }
            CompilerErrorCode::MissingOpenBraceAfterClassName => {
                "MissingOpenBraceAfterClassName".to_string()
            }
            CompilerErrorCode::MissingIdentifierAfterCallDot => format!(
                "expected property name after '.'{}",
                self.demistify_next_token()
            ),
            CompilerErrorCode::MissingSuperclassName => format!(
                "expected parent class name after '<'{}",
                self.demistify_next_token()
            ),
            CompilerErrorCode::MissingDotAfterSuperKeyword => format!(
                "expected '.' after super keyword{}",
                self.demistify_next_token()
            ),
            CompilerErrorCode::MissingIdentiferAfterSuperDot => format!(
                "expected identifier after super.{}",
                self.demistify_next_token()
            ),
            CompilerErrorCode::TooManyLocals => format!(
                "could not define local variable '{}', too many already defined",
                self.token.demistify()
            ),
            CompilerErrorCode::LocalAlreadyDefined => format!(
                "could not define local variable '{}', a variable with this name already exists in this scope",
                self.token.demistify()
            ),
            CompilerErrorCode::ReadOwnLocalBeforeInitialized => format!(
                "could not read local variable '{}', this variable is not initialized yet",
                self.token.demistify()
            ),
            CompilerErrorCode::MissingClosingParenAfterIfPredicate => format!("expected ')' after if predicate,{}", self.demistify_next_token()),
            CompilerErrorCode::JumpTooLong => "jump is too long".to_string(),
            CompilerErrorCode::TopLevelReturn => "cannot return outside a function".to_string(),
            CompilerErrorCode::MissingOpenBraceAfterClassDeclaration => format!("expected '}}' after class declaration{}", self.demistify_next_token()),
            CompilerErrorCode::ThisOutsideMethod => "cannot use 'this' outside methods".to_string(),
            CompilerErrorCode::InitializerReturnValue => "cannot return value inside initializer".to_string(),
            CompilerErrorCode::InheritFromSelf => "cannot inherit yourself".to_string(),
            CompilerErrorCode::SuperOutsideChildClass => "cannot use 'super' in class without a parent".to_string(),
            CompilerErrorCode::SuperOutsideClass => "cannot use 'super' outside a class method".to_string(), // c => format!("missing error demistifyer for {}", c as u32),
        }
    }
}

pub trait Demistify {
    fn demistify(&self) -> String;
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoxError::Scanner(err) => {
                writeln!(f, "{}", err.demistify())
            }
            LoxError::Compiler(err) => writeln!(f, "{}", err.demistify()),
            LoxError::Runtime(err) => writeln!(f, "{}", err.demistify()),
        }
    }
}
