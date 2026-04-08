/// KelpyShark Semantic Analyzer
///
/// Validates the AST for semantic correctness before code generation.
///
/// Checks performed:
///   - Undefined variable references
///   - Undefined function calls
///   - Function arity (argument count) mismatches
///   - Duplicate function definitions
///   - Return statements outside functions
///   - Unused variable warnings

use std::collections::{HashMap, HashSet};

use crate::ast::*;
use crate::error::{KelpyError, KelpyResult, SourceLocation};

/// Severity of a diagnostic.
#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

/// A diagnostic message from semantic analysis.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub location: Option<SourceLocation>,
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = match self.severity {
            Severity::Error => "Error",
            Severity::Warning => "Warning",
        };
        match &self.location {
            Some(loc) => write!(f, "[Semantic {} at {}] {}", prefix, loc, self.message),
            None => write!(f, "[Semantic {}] {}", prefix, self.message),
        }
    }
}

/// Information about a declared function.
#[derive(Debug, Clone)]
struct FunctionInfo {
    arity: usize,
    #[allow(dead_code)]
    location: SourceLocation,
}

/// The semantic analyzer walks the AST and collects diagnostics.
pub struct SemanticAnalyzer {
    /// Stack of scopes; each scope maps variable names to whether they've been read.
    scopes: Vec<HashMap<String, bool>>,
    /// Known function definitions: name → (param count, location).
    functions: HashMap<String, FunctionInfo>,
    /// Whether we're currently inside a function body.
    in_function: bool,
    /// Collected diagnostics.
    diagnostics: Vec<Diagnostic>,
    /// Built-in names that are always defined.
    builtins: HashSet<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut builtins = HashSet::new();
        for name in &["len", "type", "str", "num", "push", "print"] {
            builtins.insert(name.to_string());
        }
        SemanticAnalyzer {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            in_function: false,
            diagnostics: Vec::new(),
            builtins,
        }
    }

    /// Analyze an entire program. Returns a list of diagnostics.
    pub fn analyze(&mut self, program: &Program) -> Vec<Diagnostic> {
        // First pass: collect all top-level function declarations
        for stmt in &program.statements {
            if let Statement::FunctionDef {
                name,
                params,
                location,
                ..
            } = stmt
            {
                if self.functions.contains_key(name) {
                    self.diagnostics.push(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Function '{}' is already defined", name),
                        location: Some(location.clone()),
                    });
                } else {
                    self.functions.insert(
                        name.clone(),
                        FunctionInfo {
                            arity: params.len(),
                            location: location.clone(),
                        },
                    );
                    self.define(name);
                }
            }
        }

        // Second pass: analyze all statements
        for stmt in &program.statements {
            self.analyze_statement(stmt);
        }

        // Check for unused variables in the global scope
        if let Some(scope) = self.scopes.last() {
            for (name, used) in scope {
                if !used && !self.functions.contains_key(name) {
                    self.diagnostics.push(Diagnostic {
                        severity: Severity::Warning,
                        message: format!("Variable '{}' is defined but never used", name),
                        location: None,
                    });
                }
            }
        }

        self.diagnostics.clone()
    }

    /// Convenience: analyze and return only errors (no warnings). Returns Ok(()) or first error.
    pub fn check(program: &Program) -> KelpyResult<()> {
        let mut analyzer = SemanticAnalyzer::new();
        let diagnostics = analyzer.analyze(program);
        for d in &diagnostics {
            if d.severity == Severity::Error {
                return Err(KelpyError::SemanticError {
                    message: d.message.clone(),
                    location: d.location.clone(),
                });
            }
        }
        Ok(())
    }

    // ── Scope helpers ──

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) -> HashMap<String, bool> {
        self.scopes.pop().unwrap_or_default()
    }

    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), false);
        }
    }

    fn mark_used(&mut self, name: &str) {
        // Walk scopes from innermost to outermost
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), true);
                return;
            }
        }
    }

    fn is_defined(&self, name: &str) -> bool {
        if self.builtins.contains(name) {
            return true;
        }
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return true;
            }
        }
        false
    }

    // ── Statement analysis ──

    fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assignment {
                name,
                value,
                ..
            } => {
                self.analyze_expr(value);
                self.define(name);
            }
            Statement::FunctionDef {
                name,
                params,
                body,
                location,
                ..
            } => {
                // Function name already registered in first pass (top-level)
                // or register it now (nested)
                if !self.functions.contains_key(name) {
                    self.functions.insert(
                        name.clone(),
                        FunctionInfo {
                            arity: params.len(),
                            location: location.clone(),
                        },
                    );
                    self.define(name);
                }

                self.push_scope();
                let prev_in_func = self.in_function;
                self.in_function = true;

                // Define parameters in function scope
                for param in params {
                    self.define(param);
                    // Mark params as used (they're inputs)
                    self.mark_used(param);
                }

                for s in body {
                    self.analyze_statement(s);
                }

                let _func_scope = self.pop_scope();
                self.in_function = prev_in_func;
            }
            Statement::If {
                condition,
                then_body,
                else_body,
                ..
            } => {
                self.analyze_expr(condition);

                self.push_scope();
                for s in then_body {
                    self.analyze_statement(s);
                }
                self.pop_scope();

                if let Some(else_stmts) = else_body {
                    self.push_scope();
                    for s in else_stmts {
                        self.analyze_statement(s);
                    }
                    self.pop_scope();
                }
            }
            Statement::While {
                condition,
                body,
                ..
            } => {
                self.analyze_expr(condition);
                self.push_scope();
                for s in body {
                    self.analyze_statement(s);
                }
                self.pop_scope();
            }
            Statement::For {
                variable,
                iterable,
                body,
                ..
            } => {
                self.analyze_expr(iterable);
                self.push_scope();
                self.define(variable);
                self.mark_used(variable); // loop var is implicitly used
                for s in body {
                    self.analyze_statement(s);
                }
                self.pop_scope();
            }
            Statement::Return { value, location } => {
                if !self.in_function {
                    self.diagnostics.push(Diagnostic {
                        severity: Severity::Error,
                        message: "'return' used outside of a function".to_string(),
                        location: Some(location.clone()),
                    });
                }
                if let Some(expr) = value {
                    self.analyze_expr(expr);
                }
            }
            Statement::Import { .. } => {
                // Imports are valid anywhere; we can't resolve modules at this stage.
            }
            Statement::Print { value, .. } => {
                self.analyze_expr(value);
            }
            Statement::ExprStatement { expr, .. } => {
                self.analyze_expr(expr);
            }
            // New statement types — basic analysis (walk inner expressions)
            Statement::CompoundAssignment { value, .. } => {
                self.analyze_expr(value);
            }
            Statement::Break { .. } | Statement::Continue { .. } => {}
            Statement::Throw { value, .. } => {
                self.analyze_expr(value);
            }
            Statement::TryCatch { try_body, catch_body, .. } => {
                for s in try_body { self.analyze_statement(s); }
                for s in catch_body { self.analyze_statement(s); }
            }
            Statement::ClassDef { methods, .. } => {
                for m in methods { self.analyze_statement(m); }
            }
        }
    }

    // ── Expression analysis ──

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::NumberLiteral { .. }
            | Expr::StringLiteral { .. }
            | Expr::BooleanLiteral { .. } => {}

            Expr::Identifier { name, location } => {
                if !self.is_defined(name) {
                    self.diagnostics.push(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Undefined variable '{}'", name),
                        location: Some(location.clone()),
                    });
                } else {
                    self.mark_used(name);
                }
            }

            Expr::BinaryOp {
                left, right, ..
            } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
            }

            Expr::UnaryOp { operand, .. } => {
                self.analyze_expr(operand);
            }

            Expr::FunctionCall {
                callee,
                args,
                location,
            } => {
                // Check callee
                self.analyze_expr(callee);

                // Check argument count if callee is a known function
                if let Expr::Identifier { name, .. } = callee.as_ref() {
                    if let Some(info) = self.functions.get(name) {
                        if args.len() != info.arity {
                            self.diagnostics.push(Diagnostic {
                                severity: Severity::Error,
                                message: format!(
                                    "Function '{}' expects {} argument(s), but {} were given",
                                    name, info.arity, args.len()
                                ),
                                location: Some(location.clone()),
                            });
                        }
                    }
                }

                // Analyze each argument
                for arg in args {
                    self.analyze_expr(arg);
                }
            }

            Expr::Index {
                object, index, ..
            } => {
                self.analyze_expr(object);
                self.analyze_expr(index);
            }

            Expr::MemberAccess { object, .. } => {
                self.analyze_expr(object);
            }

            Expr::ListLiteral { elements, .. } => {
                for el in elements {
                    self.analyze_expr(el);
                }
            }

            Expr::DictLiteral { entries, .. } => {
                for (k, v) in entries {
                    self.analyze_expr(k);
                    self.analyze_expr(v);
                }
            }

            Expr::StringInterpolation { parts, .. } => {
                for part in parts {
                    if let StringPart::Expression(expr) = part {
                        self.analyze_expr(expr);
                    }
                }
            }

            Expr::NullLiteral { .. } => {}

            Expr::MethodCall { object, args, .. } => {
                self.analyze_expr(object);
                for arg in args { self.analyze_expr(arg); }
            }

            Expr::New { args, .. } => {
                for arg in args { self.analyze_expr(arg); }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn analyze_source(source: &str) -> Vec<Diagnostic> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program)
    }

    fn errors(source: &str) -> Vec<Diagnostic> {
        analyze_source(source)
            .into_iter()
            .filter(|d| d.severity == Severity::Error)
            .collect()
    }

    fn warnings(source: &str) -> Vec<Diagnostic> {
        analyze_source(source)
            .into_iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect()
    }

    #[test]
    fn test_valid_program() {
        let errs = errors("x = 42\nprint x");
        assert!(errs.is_empty(), "Expected no errors, got: {:?}", errs);
    }

    #[test]
    fn test_undefined_variable() {
        let errs = errors("print y");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("Undefined variable 'y'"));
    }

    #[test]
    fn test_variable_defined_after_use() {
        let errs = errors("print x\nx = 10");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("Undefined variable 'x'"));
    }

    #[test]
    fn test_function_arity_mismatch() {
        let errs = errors("def add(a, b) {\n  return a + b\n}\nadd(1)");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("expects 2 argument(s), but 1"));
    }

    #[test]
    fn test_function_arity_correct() {
        let errs = errors("def add(a, b) {\n  return a + b\n}\nadd(1, 2)");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_duplicate_function() {
        let errs = errors("def foo() {\n  print 1\n}\ndef foo() {\n  print 2\n}");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("already defined"));
    }

    #[test]
    fn test_return_outside_function() {
        let errs = errors("return 42");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("'return' used outside"));
    }

    #[test]
    fn test_return_inside_function_ok() {
        let errs = errors("def foo() {\n  return 42\n}");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_unused_variable_warning() {
        let warns = warnings("x = 42");
        assert_eq!(warns.len(), 1);
        assert!(warns[0].message.contains("never used"));
    }

    #[test]
    fn test_used_variable_no_warning() {
        let warns = warnings("x = 42\nprint x");
        assert!(warns.is_empty());
    }

    #[test]
    fn test_builtin_functions_are_defined() {
        let errs = errors("x = len(\"hello\")\nprint x");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_nested_scope_access() {
        let errs = errors("x = 10\nif true {\n  print x\n}");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_for_loop_variable() {
        let errs = errors("items = [1, 2, 3]\nfor item in items {\n  print item\n}");
        assert!(errs.is_empty());
    }

    #[test]
    fn test_undefined_in_binary_op() {
        let errs = errors("x = a + b");
        assert_eq!(errs.len(), 2); // both a and b undefined
    }

    #[test]
    fn test_function_call_undefined() {
        let errs = errors("foo(1, 2)");
        assert_eq!(errs.len(), 1);
        assert!(errs[0].message.contains("Undefined variable 'foo'"));
    }

    #[test]
    fn test_check_returns_first_error() {
        let mut lexer = Lexer::new("print unknown_var");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let result = SemanticAnalyzer::check(&program);
        assert!(result.is_err());
    }

    #[test]
    fn test_check_returns_ok_for_valid() {
        let mut lexer = Lexer::new("x = 1\nprint x");
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse().unwrap();
        let result = SemanticAnalyzer::check(&program);
        assert!(result.is_ok());
    }
}
