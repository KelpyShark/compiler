/// KelpyShark Parser (Pratt / Top-Down Operator Precedence)
///
/// Parses a token stream into a KelpyShark AST.
/// Uses a Pratt parser for expression precedence and recursive descent
/// for statements and top-level constructs.

use crate::ast::*;
use crate::error::{KelpyError, KelpyResult, SourceLocation};
use crate::lexer::{Token, TokenKind};

// ──────────────────────────────────────────────
//  Precedence levels for Pratt parsing
// ──────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
enum Precedence {
    None = 0,
    Or = 1,         // or
    And = 2,        // and
    Equality = 3,   // == !=
    Comparison = 4, // < <= > >=
    Term = 5,       // + -
    Factor = 6,     // * / %
    Unary = 7,      // not -
    Call = 8,       // () [] .
}

fn token_precedence(kind: &TokenKind) -> Precedence {
    match kind {
        TokenKind::Or => Precedence::Or,
        TokenKind::And => Precedence::And,
        TokenKind::EqualEqual | TokenKind::NotEqual => Precedence::Equality,
        TokenKind::LessThan
        | TokenKind::LessEqual
        | TokenKind::GreaterThan
        | TokenKind::GreaterEqual => Precedence::Comparison,
        TokenKind::Plus | TokenKind::Minus => Precedence::Term,
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
        TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot => Precedence::Call,
        _ => Precedence::None,
    }
}

// ──────────────────────────────────────────────
//  Parser
// ──────────────────────────────────────────────

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    /// Parse the full token stream into a Program AST.
    pub fn parse(&mut self) -> KelpyResult<Program> {
        let mut statements = Vec::new();
        self.skip_newlines();

        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.skip_newlines();
        }

        Ok(Program { statements })
    }

    // ── Token helpers ──

    fn peek(&self) -> &TokenKind {
        self.tokens
            .get(self.pos)
            .map(|t| &t.kind)
            .unwrap_or(&TokenKind::Eof)
    }

    fn current_location(&self) -> SourceLocation {
        self.tokens
            .get(self.pos)
            .map(|t| t.location.clone())
            .unwrap_or(SourceLocation { line: 0, column: 0 })
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.pos];
        if self.pos < self.tokens.len() - 1 {
            self.pos += 1;
        }
        token
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek(), TokenKind::Eof)
    }

    fn skip_newlines(&mut self) {
        while matches!(self.peek(), TokenKind::Newline) {
            self.advance();
        }
    }

    fn expect(&mut self, expected: &TokenKind) -> KelpyResult<&Token> {
        if self.peek() == expected {
            Ok(self.advance())
        } else {
            Err(KelpyError::ParseError {
                message: format!("Expected {}, found {}", expected, self.peek()),
                location: self.current_location(),
            })
        }
    }

    fn error(&self, message: impl Into<String>) -> KelpyError {
        KelpyError::ParseError {
            message: message.into(),
            location: self.current_location(),
        }
    }

    // ── Statement parsing ──

    fn parse_statement(&mut self) -> KelpyResult<Statement> {
        self.skip_newlines();
        match self.peek().clone() {
            TokenKind::Def => self.parse_function_def(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            TokenKind::For => self.parse_for(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Print => self.parse_print(),
            _ => self.parse_assignment_or_expr(),
        }
    }

    fn parse_function_def(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `def`

        let name = match self.peek().clone() {
            TokenKind::Identifier(n) => {
                self.advance();
                n
            }
            _ => return Err(self.error("Expected function name after 'def'")),
        };

        self.expect(&TokenKind::LParen)?;
        let params = self.parse_param_list()?;
        self.expect(&TokenKind::RParen)?;
        let body = self.parse_block()?;

        Ok(Statement::FunctionDef {
            name,
            params,
            body,
            location,
        })
    }

    fn parse_param_list(&mut self) -> KelpyResult<Vec<String>> {
        let mut params = Vec::new();
        if matches!(self.peek(), TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            match self.peek().clone() {
                TokenKind::Identifier(name) => {
                    self.advance();
                    params.push(name);
                }
                _ => return Err(self.error("Expected parameter name")),
            }

            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_block(&mut self) -> KelpyResult<Vec<Statement>> {
        self.skip_newlines();
        self.expect(&TokenKind::LBrace)?;
        self.skip_newlines();

        let mut stmts = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.skip_newlines();
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(stmts)
    }

    fn parse_if(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `if`

        let condition = self.parse_expression(Precedence::None)?;
        let then_body = self.parse_block()?;

        self.skip_newlines();
        let else_body = if matches!(self.peek(), TokenKind::Else) {
            self.advance();
            if matches!(self.peek(), TokenKind::If) {
                // else if ...
                let elif = self.parse_if()?;
                Some(vec![elif])
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_body,
            else_body,
            location,
        })
    }

    fn parse_while(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `while`

        let condition = self.parse_expression(Precedence::None)?;
        let body = self.parse_block()?;

        Ok(Statement::While {
            condition,
            body,
            location,
        })
    }

    fn parse_for(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `for`

        let variable = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected variable name after 'for'")),
        };

        self.expect(&TokenKind::In)?;
        let iterable = self.parse_expression(Precedence::None)?;
        let body = self.parse_block()?;

        Ok(Statement::For {
            variable,
            iterable,
            body,
            location,
        })
    }

    fn parse_return(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `return`

        let value = if matches!(self.peek(), TokenKind::Newline | TokenKind::RBrace | TokenKind::Eof)
        {
            None
        } else {
            Some(self.parse_expression(Precedence::None)?)
        };

        Ok(Statement::Return { value, location })
    }

    fn parse_import(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `import`

        let mut module = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected module name after 'import'")),
        };

        // Support dotted imports: import http.server
        while matches!(self.peek(), TokenKind::Dot) {
            self.advance();
            match self.peek().clone() {
                TokenKind::Identifier(part) => {
                    self.advance();
                    module.push('.');
                    module.push_str(&part);
                }
                _ => return Err(self.error("Expected module name after '.'")),
            }
        }

        Ok(Statement::Import { module, location })
    }

    fn parse_print(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `print`

        let value = self.parse_expression(Precedence::None)?;

        Ok(Statement::Print { value, location })
    }

    fn parse_assignment_or_expr(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        let expr = self.parse_expression(Precedence::None)?;

        // Check if this is an assignment: `name = expr`
        if matches!(self.peek(), TokenKind::Equals) {
            self.advance(); // consume `=`
            if let Expr::Identifier { name, .. } = expr {
                let value = self.parse_expression(Precedence::None)?;
                return Ok(Statement::Assignment {
                    name,
                    value,
                    location,
                });
            } else {
                return Err(KelpyError::ParseError {
                    message: "Invalid assignment target".to_string(),
                    location,
                });
            }
        }

        Ok(Statement::ExprStatement {
            expr,
            location,
        })
    }

    // ── Expression parsing (Pratt) ──

    fn parse_expression(&mut self, min_prec: Precedence) -> KelpyResult<Expr> {
        let mut left = self.parse_prefix()?;

        while !self.is_at_end() {
            let prec = token_precedence(self.peek());
            if prec <= min_prec {
                break;
            }
            left = self.parse_infix(left, prec)?;
        }

        Ok(left)
    }

    /// Parse a prefix expression (literal, identifier, unary op, grouping, list, dict).
    fn parse_prefix(&mut self) -> KelpyResult<Expr> {
        match self.peek().clone() {
            TokenKind::NumberLiteral(n) => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::NumberLiteral {
                    value: n,
                    location: loc,
                })
            }
            TokenKind::StringLiteral(s) => {
                let loc = self.current_location();
                self.advance();
                // Check for string interpolation {$var}
                if s.contains("{$") {
                    self.parse_string_interpolation(&s, loc)
                } else {
                    Ok(Expr::StringLiteral {
                        value: s,
                        location: loc,
                    })
                }
            }
            TokenKind::True => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::BooleanLiteral {
                    value: true,
                    location: loc,
                })
            }
            TokenKind::False => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::BooleanLiteral {
                    value: false,
                    location: loc,
                })
            }
            TokenKind::Identifier(name) => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::Identifier {
                    name,
                    location: loc,
                })
            }
            TokenKind::Minus => {
                let loc = self.current_location();
                self.advance();
                let operand = self.parse_expression(Precedence::Unary)?;
                Ok(Expr::UnaryOp {
                    op: UnaryOperator::Negate,
                    operand: Box::new(operand),
                    location: loc,
                })
            }
            TokenKind::Not => {
                let loc = self.current_location();
                self.advance();
                let operand = self.parse_expression(Precedence::Unary)?;
                Ok(Expr::UnaryOp {
                    op: UnaryOperator::Not,
                    operand: Box::new(operand),
                    location: loc,
                })
            }
            TokenKind::LParen => {
                self.advance(); // consume (
                let expr = self.parse_expression(Precedence::None)?;
                self.expect(&TokenKind::RParen)?;
                Ok(expr)
            }
            TokenKind::LBracket => self.parse_list_literal(),
            TokenKind::LBrace => self.parse_dict_literal(),
            _ => Err(self.error(format!(
                "Unexpected token in expression: {}",
                self.peek()
            ))),
        }
    }

    /// Parse an infix expression (binary ops, function calls, index, member access).
    fn parse_infix(&mut self, left: Expr, prec: Precedence) -> KelpyResult<Expr> {
        match self.peek().clone() {
            // Binary operators
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::EqualEqual
            | TokenKind::NotEqual
            | TokenKind::LessThan
            | TokenKind::LessEqual
            | TokenKind::GreaterThan
            | TokenKind::GreaterEqual
            | TokenKind::And
            | TokenKind::Or => {
                let loc = self.current_location();
                let op = self.parse_binary_op()?;
                let right = self.parse_expression(prec)?;
                Ok(Expr::BinaryOp {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                    location: loc,
                })
            }
            // Function call
            TokenKind::LParen => {
                let loc = self.current_location();
                self.advance(); // consume (
                let args = self.parse_arg_list()?;
                self.expect(&TokenKind::RParen)?;
                Ok(Expr::FunctionCall {
                    callee: Box::new(left),
                    args,
                    location: loc,
                })
            }
            // Index access
            TokenKind::LBracket => {
                let loc = self.current_location();
                self.advance(); // consume [
                let index = self.parse_expression(Precedence::None)?;
                self.expect(&TokenKind::RBracket)?;
                Ok(Expr::Index {
                    object: Box::new(left),
                    index: Box::new(index),
                    location: loc,
                })
            }
            // Member access
            TokenKind::Dot => {
                let loc = self.current_location();
                self.advance(); // consume .
                match self.peek().clone() {
                    TokenKind::Identifier(member) => {
                        self.advance();
                        Ok(Expr::MemberAccess {
                            object: Box::new(left),
                            member,
                            location: loc,
                        })
                    }
                    _ => Err(self.error("Expected member name after '.'")),
                }
            }
            _ => Err(self.error(format!("Unexpected infix token: {}", self.peek()))),
        }
    }

    fn parse_binary_op(&mut self) -> KelpyResult<BinaryOperator> {
        let op = match self.peek() {
            TokenKind::Plus => BinaryOperator::Add,
            TokenKind::Minus => BinaryOperator::Subtract,
            TokenKind::Star => BinaryOperator::Multiply,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::Percent => BinaryOperator::Modulo,
            TokenKind::EqualEqual => BinaryOperator::Equal,
            TokenKind::NotEqual => BinaryOperator::NotEqual,
            TokenKind::LessThan => BinaryOperator::LessThan,
            TokenKind::LessEqual => BinaryOperator::LessEqual,
            TokenKind::GreaterThan => BinaryOperator::GreaterThan,
            TokenKind::GreaterEqual => BinaryOperator::GreaterEqual,
            TokenKind::And => BinaryOperator::And,
            TokenKind::Or => BinaryOperator::Or,
            _ => return Err(self.error("Expected binary operator")),
        };
        self.advance();
        Ok(op)
    }

    fn parse_arg_list(&mut self) -> KelpyResult<Vec<Expr>> {
        let mut args = Vec::new();
        if matches!(self.peek(), TokenKind::RParen) {
            return Ok(args);
        }

        loop {
            self.skip_newlines();
            let arg = self.parse_expression(Precedence::None)?;
            args.push(arg);

            self.skip_newlines();
            if matches!(self.peek(), TokenKind::Comma) {
                self.advance();
            } else {
                break;
            }
        }

        Ok(args)
    }

    fn parse_list_literal(&mut self) -> KelpyResult<Expr> {
        let loc = self.current_location();
        self.advance(); // consume [

        let mut elements = Vec::new();
        self.skip_newlines();

        if !matches!(self.peek(), TokenKind::RBracket) {
            loop {
                self.skip_newlines();
                let elem = self.parse_expression(Precedence::None)?;
                elements.push(elem);
                self.skip_newlines();

                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.skip_newlines();
        self.expect(&TokenKind::RBracket)?;

        Ok(Expr::ListLiteral {
            elements,
            location: loc,
        })
    }

    fn parse_dict_literal(&mut self) -> KelpyResult<Expr> {
        let loc = self.current_location();
        self.advance(); // consume {

        let mut entries = Vec::new();
        self.skip_newlines();

        if !matches!(self.peek(), TokenKind::RBrace) {
            loop {
                self.skip_newlines();
                let key = self.parse_expression(Precedence::None)?;
                self.expect(&TokenKind::Colon)?;
                self.skip_newlines();
                let value = self.parse_expression(Precedence::None)?;
                entries.push((key, value));
                self.skip_newlines();

                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        self.skip_newlines();
        self.expect(&TokenKind::RBrace)?;

        Ok(Expr::DictLiteral {
            entries,
            location: loc,
        })
    }

    /// Parse string interpolation: `"You have {$value} items"`
    /// Splits the string into literal and expression parts.
    fn parse_string_interpolation(
        &self,
        raw: &str,
        location: SourceLocation,
    ) -> KelpyResult<Expr> {
        let mut parts = Vec::new();
        let mut current = String::new();
        let chars: Vec<char> = raw.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            if chars[i] == '{' && i + 1 < chars.len() && chars[i + 1] == '$' {
                // Flush literal part
                if !current.is_empty() {
                    parts.push(StringPart::Literal(current.clone()));
                    current.clear();
                }
                // Skip {$
                i += 2;
                let mut var_name = String::new();
                while i < chars.len() && chars[i] != '}' {
                    var_name.push(chars[i]);
                    i += 1;
                }
                if i < chars.len() {
                    i += 1; // skip }
                }
                parts.push(StringPart::Expression(Expr::Identifier {
                    name: var_name,
                    location: location.clone(),
                }));
            } else {
                current.push(chars[i]);
                i += 1;
            }
        }

        if !current.is_empty() {
            parts.push(StringPart::Literal(current));
        }

        Ok(Expr::StringInterpolation { parts, location })
    }
}

// ──────────────────────────────────────────────
//  Tests
// ──────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    /// Helper: parse source code into a Program AST.
    fn parse(source: &str) -> KelpyResult<Program> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize()?;
        let mut parser = Parser::new(tokens);
        parser.parse()
    }

    #[test]
    fn test_simple_assignment() {
        let program = parse("x = 42").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, .. } => assert_eq!(name, "x"),
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_string_assignment() {
        let program = parse(r#"name = "KelpyShark""#).unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::Assignment { name, value, .. } => {
                assert_eq!(name, "name");
                match value {
                    Expr::StringLiteral { value: s, .. } => assert_eq!(s, "KelpyShark"),
                    other => panic!("Expected StringLiteral, got: {:?}", other),
                }
            }
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_binary_expression() {
        let program = parse("x = 1 + 2 * 3").unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => {
                // Should parse as 1 + (2 * 3) due to precedence
                match value {
                    Expr::BinaryOp { op, .. } => {
                        assert_eq!(*op, BinaryOperator::Add);
                    }
                    other => panic!("Expected BinaryOp, got: {:?}", other),
                }
            }
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_operator_precedence() {
        // 2 + 3 * 4 should be 2 + (3 * 4) = 14, not (2 + 3) * 4 = 20
        let program = parse("x = 2 + 3 * 4").unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => {
                match value {
                    Expr::BinaryOp {
                        op,
                        left,
                        right,
                        ..
                    } => {
                        assert_eq!(*op, BinaryOperator::Add);
                        // Left should be 2
                        match left.as_ref() {
                            Expr::NumberLiteral { value: n, .. } => assert_eq!(*n, 2.0),
                            other => panic!("Expected NumberLiteral(2), got: {:?}", other),
                        }
                        // Right should be 3 * 4
                        match right.as_ref() {
                            Expr::BinaryOp { op, .. } => {
                                assert_eq!(*op, BinaryOperator::Multiply);
                            }
                            other => panic!("Expected BinaryOp(*), got: {:?}", other),
                        }
                    }
                    other => panic!("Expected BinaryOp, got: {:?}", other),
                }
            }
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_function_def() {
        let program = parse("def greet(name) {\n  print name\n}").unwrap();
        assert_eq!(program.statements.len(), 1);
        match &program.statements[0] {
            Statement::FunctionDef {
                name,
                params,
                body,
                ..
            } => {
                assert_eq!(name, "greet");
                assert_eq!(params, &vec!["name".to_string()]);
                assert_eq!(body.len(), 1);
            }
            other => panic!("Expected FunctionDef, got: {:?}", other),
        }
    }

    #[test]
    fn test_function_multiple_params() {
        let program = parse("def add(a, b) {\n  return a + b\n}").unwrap();
        match &program.statements[0] {
            Statement::FunctionDef { params, body, .. } => {
                assert_eq!(params, &vec!["a".to_string(), "b".to_string()]);
                assert_eq!(body.len(), 1);
                match &body[0] {
                    Statement::Return { value: Some(_), .. } => {}
                    other => panic!("Expected Return, got: {:?}", other),
                }
            }
            other => panic!("Expected FunctionDef, got: {:?}", other),
        }
    }

    #[test]
    fn test_if_statement() {
        let program = parse("if x >= 5 {\n  print x\n}").unwrap();
        match &program.statements[0] {
            Statement::If {
                then_body,
                else_body,
                ..
            } => {
                assert_eq!(then_body.len(), 1);
                assert!(else_body.is_none());
            }
            other => panic!("Expected If, got: {:?}", other),
        }
    }

    #[test]
    fn test_if_else() {
        let program = parse("if x >= 5 {\n  print x\n} else {\n  print y\n}").unwrap();
        match &program.statements[0] {
            Statement::If {
                then_body,
                else_body,
                ..
            } => {
                assert_eq!(then_body.len(), 1);
                assert!(else_body.is_some());
                assert_eq!(else_body.as_ref().unwrap().len(), 1);
            }
            other => panic!("Expected If, got: {:?}", other),
        }
    }

    #[test]
    fn test_while_loop() {
        let program = parse("while x < 10 {\n  x = x + 1\n}").unwrap();
        match &program.statements[0] {
            Statement::While { body, .. } => {
                assert_eq!(body.len(), 1);
            }
            other => panic!("Expected While, got: {:?}", other),
        }
    }

    #[test]
    fn test_for_loop() {
        let program = parse("for item in items {\n  print item\n}").unwrap();
        match &program.statements[0] {
            Statement::For { variable, .. } => {
                assert_eq!(variable, "item");
            }
            other => panic!("Expected For, got: {:?}", other),
        }
    }

    #[test]
    fn test_list_literal() {
        let program = parse(r#"x = ["apple", "banana", "orange"]"#).unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::ListLiteral { elements, .. } => {
                    assert_eq!(elements.len(), 3);
                }
                other => panic!("Expected ListLiteral, got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_dict_literal() {
        let program = parse(r#"x = {"name": "Bob", "age": 27}"#).unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::DictLiteral { entries, .. } => {
                    assert_eq!(entries.len(), 2);
                }
                other => panic!("Expected DictLiteral, got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_function_call() {
        let program = parse("greet(name)").unwrap();
        match &program.statements[0] {
            Statement::ExprStatement { expr, .. } => match expr {
                Expr::FunctionCall { args, .. } => {
                    assert_eq!(args.len(), 1);
                }
                other => panic!("Expected FunctionCall, got: {:?}", other),
            },
            other => panic!("Expected ExprStatement, got: {:?}", other),
        }
    }

    #[test]
    fn test_index_access() {
        let program = parse(r#"x = list[0]"#).unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::Index { .. } => {}
                other => panic!("Expected Index, got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_member_access() {
        let program = parse("x = obj.field").unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::MemberAccess { member, .. } => {
                    assert_eq!(member, "field");
                }
                other => panic!("Expected MemberAccess, got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_string_interpolation() {
        let program = parse(r#"x = "Hello {$name}!""#).unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::StringInterpolation { parts, .. } => {
                    assert_eq!(parts.len(), 3); // "Hello ", name, "!"
                }
                other => panic!("Expected StringInterpolation, got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_import() {
        let program = parse("import math").unwrap();
        match &program.statements[0] {
            Statement::Import { module, .. } => {
                assert_eq!(module, "math");
            }
            other => panic!("Expected Import, got: {:?}", other),
        }
    }

    #[test]
    fn test_dotted_import() {
        let program = parse("import http.server").unwrap();
        match &program.statements[0] {
            Statement::Import { module, .. } => {
                assert_eq!(module, "http.server");
            }
            other => panic!("Expected Import, got: {:?}", other),
        }
    }

    #[test]
    fn test_unary_negate() {
        let program = parse("x = -5").unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::UnaryOp {
                    op: UnaryOperator::Negate,
                    ..
                } => {}
                other => panic!("Expected UnaryOp(Negate), got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let program = parse("x = true and false").unwrap();
        match &program.statements[0] {
            Statement::Assignment { value, .. } => match value {
                Expr::BinaryOp {
                    op: BinaryOperator::And,
                    ..
                } => {}
                other => panic!("Expected BinaryOp(And), got: {:?}", other),
            },
            other => panic!("Expected Assignment, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_error_unexpected_token() {
        let result = parse("x = = 5");
        assert!(result.is_err());
    }

    #[test]
    fn test_full_example() {
        let source = r#"
bob = {
    "age": "27 years",
    "name": "Bob Smith"
}

example_list = ["apple", "banana", "orange"]

def example_function(value, thing) {
    print "You have some items!"

    if value >= 25 {
        print "You lost."
    }
}

example_function(42, "point")
"#;
        let program = parse(source);
        assert!(program.is_ok(), "Full example should parse: {:?}", program.err());
        let program = program.unwrap();
        // Should have: bob assignment, example_list assignment, function def, function call
        assert_eq!(program.statements.len(), 4);
    }
}

