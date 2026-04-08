/// KelpyShark AST (Abstract Syntax Tree)
///
/// All node types that the parser can produce.

use crate::error::SourceLocation;

/// The root node of a KelpyShark program.
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

/// A statement in the language.
#[derive(Debug, Clone)]
pub enum Statement {
    /// Variable assignment: `x = expr`
    Assignment {
        name: String,
        value: Expr,
        location: SourceLocation,
    },
    /// Compound assignment: `x += expr`, `x -= expr`, etc.
    CompoundAssignment {
        name: String,
        op: CompoundOp,
        value: Expr,
        location: SourceLocation,
    },
    /// Function definition: `def name(params) { body }`
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Vec<Statement>,
        location: SourceLocation,
    },
    /// If statement: `if expr { body }` with optional elif/else
    If {
        condition: Expr,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
        location: SourceLocation,
    },
    /// While loop: `while expr { body }`
    While {
        condition: Expr,
        body: Vec<Statement>,
        location: SourceLocation,
    },
    /// For loop: `for item in expr { body }`
    For {
        variable: String,
        iterable: Expr,
        body: Vec<Statement>,
        location: SourceLocation,
    },
    /// Return statement: `return expr`
    Return {
        value: Option<Expr>,
        location: SourceLocation,
    },
    /// Break statement: `break`
    Break {
        location: SourceLocation,
    },
    /// Continue statement: `continue`
    Continue {
        location: SourceLocation,
    },
    /// Import statement: `import module_name`
    Import {
        module: String,
        location: SourceLocation,
    },
    /// Print statement: `print expr`
    Print {
        value: Expr,
        location: SourceLocation,
    },
    /// Try-catch block: `try { body } catch (err) { handler }`
    TryCatch {
        try_body: Vec<Statement>,
        catch_var: String,
        catch_body: Vec<Statement>,
        location: SourceLocation,
    },
    /// Throw statement: `throw expr`
    Throw {
        value: Expr,
        location: SourceLocation,
    },
    /// Class definition: `class Name { def method(self) { ... } }`
    ClassDef {
        name: String,
        methods: Vec<Statement>,
        location: SourceLocation,
    },
    /// Expression used as a statement (e.g. a function call).
    ExprStatement {
        expr: Expr,
        location: SourceLocation,
    },
}

/// An expression in the language.
#[derive(Debug, Clone)]
pub enum Expr {
    /// Number literal: `42`, `3.14`
    NumberLiteral {
        value: f64,
        location: SourceLocation,
    },
    /// String literal: `"hello"`
    StringLiteral {
        value: String,
        location: SourceLocation,
    },
    /// Boolean literal: `true`, `false`
    BooleanLiteral {
        value: bool,
        location: SourceLocation,
    },
    /// Variable / identifier reference: `x`
    Identifier {
        name: String,
        location: SourceLocation,
    },
    /// Binary operation: `a + b`, `x >= 5`
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOperator,
        right: Box<Expr>,
        location: SourceLocation,
    },
    /// Unary operation: `not x`, `-5`
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expr>,
        location: SourceLocation,
    },
    /// Function call: `foo(a, b)`
    FunctionCall {
        callee: Box<Expr>,
        args: Vec<Expr>,
        location: SourceLocation,
    },
    /// Index access: `list[0]`, `dict["key"]`
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        location: SourceLocation,
    },
    /// Member access: `obj.field`
    MemberAccess {
        object: Box<Expr>,
        member: String,
        location: SourceLocation,
    },
    /// List literal: `[1, 2, 3]`
    ListLiteral {
        elements: Vec<Expr>,
        location: SourceLocation,
    },
    /// Dictionary literal: `{"key": value}`
    DictLiteral {
        entries: Vec<(Expr, Expr)>,
        location: SourceLocation,
    },
    /// Null literal: `null`
    NullLiteral {
        location: SourceLocation,
    },
    /// Method call: `obj.method(args)` — distinct from MemberAccess + FunctionCall
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
        location: SourceLocation,
    },
    /// Object instantiation: `new ClassName(args)`
    New {
        class_name: String,
        args: Vec<Expr>,
        location: SourceLocation,
    },
    /// String interpolation: `"Hello {$name}!"`
    /// Stored as a list of parts — either literal strings or expressions.
    StringInterpolation {
        parts: Vec<StringPart>,
        location: SourceLocation,
    },
}

/// Part of an interpolated string.
#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(String),
    Expression(Expr),
}

/// Binary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Add,        // +
    Subtract,   // -
    Multiply,   // *
    Divide,     // /
    Modulo,     // %
    Equal,      // ==
    NotEqual,   // !=
    LessThan,   // <
    LessEqual,  // <=
    GreaterThan,// >
    GreaterEqual,// >=
    And,        // and
    Or,         // or
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterEqual => write!(f, ">="),
            BinaryOperator::And => write!(f, "and"),
            BinaryOperator::Or => write!(f, "or"),
        }
    }
}

/// Unary operators.
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Negate, // -
    Not,    // not
}

impl std::fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Negate => write!(f, "-"),
            UnaryOperator::Not => write!(f, "not"),
        }
    }
}

/// Compound assignment operators.
#[derive(Debug, Clone, PartialEq)]
pub enum CompoundOp {
    Add,      // +=
    Subtract, // -=
    Multiply, // *=
    Divide,   // /=
}
