use crate::ast::{Expr, Program, Statement, StringPart};
use std::collections::HashSet;

/// Validate constructs whose current generated Java and C# runtimes can execute
/// with the same behavior as the interpreter.
pub fn validate_managed_target(program: &Program, target: &str) -> Result<(), String> {
    let functions = program.statements.iter().filter_map(|statement| match statement {
        Statement::FunctionDef { name, .. } => Some(name.clone()),
        _ => None,
    }).collect::<HashSet<_>>();
    for statement in &program.statements {
        validate_statement(statement, target, &functions, true)?;
    }
    Ok(())
}

pub fn validate_javascript_target(program: &Program) -> Result<(), String> {
    let functions = program.statements.iter().filter_map(|statement| match statement {
        Statement::FunctionDef { name, .. } => Some(name.clone()),
        _ => None,
    }).collect::<HashSet<_>>();
    for statement in &program.statements {
        validate_javascript_statement(statement, &functions)?;
    }
    Ok(())
}

fn validate_javascript_statement(statement: &Statement, functions: &HashSet<String>) -> Result<(), String> {
    match statement {
        Statement::Assignment { value, .. } | Statement::CompoundAssignment { value, .. }
        | Statement::Print { value, .. } | Statement::Throw { value, .. } => validate_javascript_expr(value, functions),
        Statement::MemberAssignment { object, value, .. } => {
            validate_javascript_expr(object, functions)?;
            validate_javascript_expr(value, functions)
        }
        Statement::IndexAssignment { object, index, value, .. } => {
            validate_javascript_expr(object, functions)?;
            validate_javascript_expr(index, functions)?;
            validate_javascript_expr(value, functions)
        }
        Statement::FunctionDef { body, .. } => validate_javascript_statements(body, functions),
        Statement::If { condition, then_body, else_body, .. } => {
            validate_javascript_expr(condition, functions)?;
            validate_javascript_statements(then_body, functions)?;
            if let Some(body) = else_body { validate_javascript_statements(body, functions)?; }
            Ok(())
        }
        Statement::While { condition, body, .. } => {
            validate_javascript_expr(condition, functions)?;
            validate_javascript_statements(body, functions)
        }
        Statement::For { iterable, body, .. } => {
            validate_javascript_expr(iterable, functions)?;
            validate_javascript_statements(body, functions)
        }
        Statement::Return { value, .. } => {
            if let Some(value) = value { validate_javascript_expr(value, functions)?; }
            Ok(())
        }
        Statement::Import { module, .. } => Err(format!(
            "JavaScript target does not yet support imports (including standard-library module '{}')",
            module
        )),
        Statement::TryCatch { try_body, catch_body, .. } => {
            validate_javascript_statements(try_body, functions)?;
            validate_javascript_statements(catch_body, functions)
        }
        Statement::ClassDef { methods, .. } => {
            for method in methods { validate_javascript_statement(method, functions)?; }
            Ok(())
        }
        Statement::ExprStatement { expr, .. } => validate_javascript_expr(expr, functions),
        Statement::Break { .. } | Statement::Continue { .. } => Ok(()),
    }
}

fn validate_javascript_statements(statements: &[Statement], functions: &HashSet<String>) -> Result<(), String> {
    for statement in statements { validate_javascript_statement(statement, functions)?; }
    Ok(())
}

fn validate_javascript_expr(expression: &Expr, functions: &HashSet<String>) -> Result<(), String> {
    match expression {
        Expr::BinaryOp { left, right, .. } => {
            validate_javascript_expr(left, functions)?;
            validate_javascript_expr(right, functions)
        }
        Expr::UnaryOp { operand, .. } => validate_javascript_expr(operand, functions),
        Expr::FunctionCall { callee, args, .. } => {
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                if !functions.contains(name) {
                    let expected = match name.as_str() {
                        "len" | "type" | "str" | "num" | "int" | "float" | "abs" | "sqrt"
                        | "floor" | "ceil" | "round" | "input" => Some(1),
                        "push" | "min" | "max" | "pow" => Some(2),
                        "print" => Some(1),
                        "range" => {
                            if !(1..=3).contains(&args.len()) { return Err(format!("range() expects 1 to 3 arguments, got {}", args.len())); }
                            None
                        }
                        _ => None,
                    };
                    if let Some(expected) = expected {
                        if args.len() != expected { return Err(format!("{}() expects {} argument(s), got {}", name, expected, args.len())); }
                    }
                }
            }
            validate_javascript_expr(callee, functions)?;
            for arg in args { validate_javascript_expr(arg, functions)?; }
            Ok(())
        }
        Expr::Index { object, index, .. } => {
            validate_javascript_expr(object, functions)?;
            validate_javascript_expr(index, functions)
        }
        Expr::MemberAccess { object, .. } => validate_javascript_expr(object, functions),
        Expr::ListLiteral { elements, .. } => {
            for element in elements { validate_javascript_expr(element, functions)?; }
            Ok(())
        }
        Expr::DictLiteral { entries, .. } => {
            for (key, value) in entries { validate_javascript_expr(key, functions)?; validate_javascript_expr(value, functions)?; }
            Ok(())
        }
        Expr::StringInterpolation { parts, .. } => {
            for part in parts { if let StringPart::Expression(expr) = part { validate_javascript_expr(expr, functions)?; } }
            Ok(())
        }
        Expr::MethodCall { object, args, .. } => {
            validate_javascript_expr(object, functions)?;
            for arg in args { validate_javascript_expr(arg, functions)?; }
            Ok(())
        }
        Expr::New { args, .. } => {
            for arg in args { validate_javascript_expr(arg, functions)?; }
            Ok(())
        }
        Expr::NumberLiteral { .. } | Expr::StringLiteral { .. } | Expr::BooleanLiteral { .. }
        | Expr::Identifier { .. } | Expr::NullLiteral { .. } => Ok(()),
    }
}

fn validate_statement(statement: &Statement, target: &str, functions: &HashSet<String>, top_level: bool) -> Result<(), String> {
    match statement {
        Statement::Assignment { value, .. }
        | Statement::CompoundAssignment { value, .. }
        | Statement::Print { value, .. }
        | Statement::Throw { value, .. } => validate_expr(value, target, functions),
        Statement::MemberAssignment { object, value, .. } => {
            validate_expr(object, target, functions)?;
            validate_expr(value, target, functions)
        }
        Statement::IndexAssignment { object, index, value, .. } => {
            validate_expr(object, target, functions)?;
            validate_expr(index, target, functions)?;
            validate_expr(value, target, functions)
        }
        Statement::FunctionDef { body, .. } => {
            if !top_level {
                return Err(format!("{} target does not yet support nested function definitions", target));
            }
            validate_statements(body, target, functions)
        }
        Statement::If { condition, then_body, else_body, .. } => {
            validate_expr(condition, target, functions)?;
            validate_statements(then_body, target, functions)?;
            if let Some(body) = else_body { validate_statements(body, target, functions)?; }
            Ok(())
        }
        Statement::While { condition, body, .. } => {
            validate_expr(condition, target, functions)?;
            validate_statements(body, target, functions)
        }
        Statement::For { iterable, body, .. } => {
            validate_expr(iterable, target, functions)?;
            validate_statements(body, target, functions)
        }
        Statement::Return { value, .. } => {
            if let Some(value) = value { validate_expr(value, target, functions)?; }
            Ok(())
        }
        Statement::Import { module, .. } => Err(format!(
            "{} target does not yet support imports (including standard-library module '{}')",
            target, module
        )),
        Statement::TryCatch { try_body, catch_body, .. } => {
            validate_statements(try_body, target, functions)?;
            validate_statements(catch_body, target, functions)
        }
        Statement::ClassDef { .. } => Err(format!(
            "{} target does not yet support class definitions",
            target
        )),
        Statement::ExprStatement { expr, .. } => validate_expr(expr, target, functions),
        Statement::Break { .. } | Statement::Continue { .. } => Ok(()),
    }
}

fn validate_statements(statements: &[Statement], target: &str, functions: &HashSet<String>) -> Result<(), String> {
    for statement in statements { validate_statement(statement, target, functions, false)?; }
    Ok(())
}

fn validate_expr(expression: &Expr, target: &str, functions: &HashSet<String>) -> Result<(), String> {
    match expression {
        Expr::BinaryOp { left, op: _, right, .. } => {
            validate_expr(left, target, functions)?;
            validate_expr(right, target, functions)
        }
        Expr::UnaryOp { op: _, operand, .. } => validate_expr(operand, target, functions),
        Expr::FunctionCall { callee, args, .. } => {
            if let Expr::Identifier { name, .. } = callee.as_ref() {
                if functions.contains(name) {
                    validate_expr(callee, target, functions)?;
                    for argument in args { validate_expr(argument, target, functions)?; }
                    return Ok(());
                }
                let arity = match name.as_str() {
                    "len" | "type" | "str" | "num" | "int" | "float" | "abs"
                    | "sqrt" | "floor" | "ceil" | "round" | "input" => Some(1),
                    "push" | "min" | "max" | "pow" => Some(2),
                    "print" => Some(1),
                    "range" => {
                        if !(1..=3).contains(&args.len()) {
                            return Err(format!("range() expects 1 to 3 arguments, got {}", args.len()));
                        }
                        None
                    }
                    _ => None,
                };
                if let Some(expected) = arity {
                    if args.len() != expected {
                        return Err(format!("{}() expects {} argument(s), got {}", name, expected, args.len()));
                    }
                }
            }
            validate_expr(callee, target, functions)?;
            for argument in args { validate_expr(argument, target, functions)?; }
            Ok(())
        }
        Expr::Index { object, index, .. } => {
            validate_expr(object, target, functions)?;
            validate_expr(index, target, functions)
        }
        Expr::MemberAccess { object, .. } => validate_expr(object, target, functions),
        Expr::ListLiteral { elements, .. } => {
            for element in elements { validate_expr(element, target, functions)?; }
            Ok(())
        }
        Expr::DictLiteral { entries, .. } => {
            for (key, value) in entries {
                validate_expr(key, target, functions)?;
                validate_expr(value, target, functions)?;
            }
            Ok(())
        }
        Expr::StringInterpolation { parts, .. } => {
            for part in parts {
                if let StringPart::Expression(expr) = part { validate_expr(expr, target, functions)?; }
            }
            Ok(())
        }
        Expr::MethodCall { .. } => Err(format!("{} target does not yet support method calls", target)),
        Expr::New { .. } => Err(format!("{} target does not yet support object construction", target)),
        Expr::NumberLiteral { .. } | Expr::StringLiteral { .. } | Expr::BooleanLiteral { .. }
        | Expr::Identifier { .. } | Expr::NullLiteral { .. } => Ok(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    fn validate(source: &str) -> Result<(), String> {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let program = Parser::new(tokens).parse().unwrap();
        validate_managed_target(&program, "test")
    }

    #[test]
    fn rejects_constructs_that_would_be_silently_miscompiled() {
        for (source, reason) in [
            ("import math", "imports"),
            ("class Box:\nend", "class definitions"),
            ("x = new Box()", "object construction"),
            ("x = values.append(1)", "method calls"),
        ] {
            assert!(validate(source).unwrap_err().contains(reason), "source: {source}");
        }
    }

    #[test]
    fn validates_builtin_arities() {
        assert!(validate("x = len()").unwrap_err().contains("expects 1 argument"));
        assert!(validate("x = range(1, 2, 3, 4)").unwrap_err().contains("range() expects"));
        assert!(validate("x = len([1])").is_ok());
    }
}
