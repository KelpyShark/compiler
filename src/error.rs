/// KelpyShark error types used throughout the compiler pipeline.

/// Represents a position in the source code for error reporting.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

/// All error types the KelpyShark compiler can produce.
#[derive(Debug, Clone)]
pub enum KelpyError {
    /// Lexer encountered an unexpected character or malformed token.
    LexerError {
        message: String,
        location: SourceLocation,
    },
    /// Parser encountered unexpected token or invalid syntax.
    ParseError {
        message: String,
        location: SourceLocation,
    },
    /// Semantic analysis found a logical error (e.g. undefined variable).
    SemanticError {
        message: String,
        location: Option<SourceLocation>,
    },
    /// Code generation failed.
    CodegenError {
        message: String,
    },
    /// Runtime error during interpretation.
    RuntimeError {
        message: String,
    },
}

impl std::fmt::Display for KelpyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KelpyError::LexerError { message, location } => {
                write!(f, "[Lexer Error at {}] {}", location, message)
            }
            KelpyError::ParseError { message, location } => {
                write!(f, "[Parse Error at {}] {}", location, message)
            }
            KelpyError::SemanticError {
                message,
                location: Some(loc),
            } => {
                write!(f, "[Semantic Error at {}] {}", loc, message)
            }
            KelpyError::SemanticError {
                message,
                location: None,
            } => {
                write!(f, "[Semantic Error] {}", message)
            }
            KelpyError::CodegenError { message } => {
                write!(f, "[Codegen Error] {}", message)
            }
            KelpyError::RuntimeError { message } => {
                write!(f, "[Runtime Error] {}", message)
            }
        }
    }
}

impl std::error::Error for KelpyError {}

pub type KelpyResult<T> = Result<T, KelpyError>;
