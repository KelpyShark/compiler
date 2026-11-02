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

fn is_java_modifier(name: &str) -> bool {
    matches!(
        name,
        "public"
            | "protected"
            | "private"
            | "static"
            | "final"
            | "abstract"
            | "synchronized"
            | "native"
            | "strictfp"
    )
}

fn annotation_token_text(token: &TokenKind) -> String {
    match token {
        TokenKind::Identifier(name) => name.clone(),
        TokenKind::StringLiteral(value) => format!("{:?}", value),
        TokenKind::NumberLiteral(value) => value.to_string(),
        TokenKind::BooleanLiteral(value) => value.to_string(),
        TokenKind::Class => "class".to_string(),
        TokenKind::Newline => " ".to_string(),
        _ => token.to_string(),
    }
}

// ──────────────────────────────────────────────
//  Parser
// ──────────────────────────────────────────────

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
    class_depth: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
            class_depth: 0,
        }
    }

    /// Parse the full token stream into a Program AST.
    pub fn parse(&mut self) -> KelpyResult<Program> {
        let mut statements = Vec::new();
        self.skip_separators();

        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.skip_separators();
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

    fn skip_separators(&mut self) {
        while matches!(self.peek(), TokenKind::Newline | TokenKind::Semicolon) {
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
        if matches!(self.peek(), TokenKind::At) {
            return self.parse_annotated_declaration();
        }
        if matches!(self.peek(), TokenKind::Identifier(name) if is_java_modifier(name)) {
            return self.parse_modified_declaration(Vec::new());
        }
        match self.peek().clone() {
            TokenKind::Def => self.parse_function_def(),
            TokenKind::Class => self.parse_class_def(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            TokenKind::For => self.parse_for(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Break => {
                let loc = self.current_location();
                self.advance();
                Ok(Statement::Break { location: loc })
            }
            TokenKind::Continue => {
                let loc = self.current_location();
                self.advance();
                Ok(Statement::Continue { location: loc })
            }
            TokenKind::Import => self.parse_import(),
            TokenKind::Print => self.parse_print(),
            TokenKind::Try => self.parse_try_catch(),
            TokenKind::Throw => self.parse_throw(),
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

        let param_types = vec![None; params.len()];
        Ok(Statement::FunctionDef {
            name,
            params,
            param_types,
            return_type: None,
            modifiers: Vec::new(),
            annotations: Vec::new(),
            body,
            location,
        })
    }

    fn parse_annotated_declaration(&mut self) -> KelpyResult<Statement> {
        let annotations = self.parse_annotations()?;
        self.skip_newlines();
        self.parse_modified_declaration(annotations)
    }

    fn parse_modified_declaration(
        &mut self,
        annotations: Vec<Annotation>,
    ) -> KelpyResult<Statement> {
        let modifiers = self.parse_modifiers();
        match self.peek() {
            TokenKind::Class => self.parse_java_class_def(modifiers, annotations),
            TokenKind::Def => {
                let mut function = self.parse_function_def()?;
                if let Statement::FunctionDef {
                    modifiers: target_modifiers,
                    annotations: target_annotations,
                    ..
                } = &mut function
                {
                    *target_modifiers = modifiers;
                    *target_annotations = annotations;
                }
                Ok(function)
            }
            TokenKind::Identifier(_) => {
                self.parse_java_method_def(modifiers, annotations)
            }
            _ => Err(self.error(
                "Expected a class or method declaration after annotations/modifiers",
            )),
        }
    }

    fn parse_modifiers(&mut self) -> Vec<String> {
        let mut modifiers = Vec::new();
        while let TokenKind::Identifier(name) = self.peek().clone() {
            if !is_java_modifier(&name) {
                break;
            }
            self.advance();
            modifiers.push(name);
        }
        modifiers
    }

    fn parse_annotations(&mut self) -> KelpyResult<Vec<Annotation>> {
        let mut annotations = Vec::new();
        while matches!(self.peek(), TokenKind::At) {
            self.advance();
            let name = match self.peek().clone() {
                TokenKind::Identifier(name) => {
                    self.advance();
                    name
                }
                _ => return Err(self.error("Expected annotation name after '@'")),
            };
            let mut qualified_name = name;
            while matches!(self.peek(), TokenKind::Dot) {
                self.advance();
                match self.peek().clone() {
                    TokenKind::Identifier(part) => {
                        self.advance();
                        qualified_name.push('.');
                        qualified_name.push_str(&part);
                    }
                    _ => return Err(self.error("Expected annotation name after '.'")),
                }
            }

            let mut arguments = Vec::new();
            if matches!(self.peek(), TokenKind::LParen) {
                self.advance();
                let mut argument = String::new();
                let mut nested_closers = Vec::new();
                let mut closed = false;
                while !self.is_at_end() {
                    match self.peek().clone() {
                        TokenKind::RParen if nested_closers.is_empty() => {
                            self.advance();
                            if !argument.is_empty() {
                                arguments.push(argument);
                            }
                            closed = true;
                            break;
                        }
                        TokenKind::LParen => {
                            self.advance();
                            nested_closers.push(TokenKind::RParen);
                            argument.push('(');
                        }
                        TokenKind::LBracket => {
                            self.advance();
                            nested_closers.push(TokenKind::RBracket);
                            argument.push('[');
                        }
                        TokenKind::LBrace => {
                            self.advance();
                            nested_closers.push(TokenKind::RBrace);
                            argument.push('{');
                        }
                        TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace => {
                            let closing = self.peek().clone();
                            if nested_closers.pop().as_ref() != Some(&closing) {
                                return Err(self.error("Mismatched delimiter in annotation arguments"));
                            }
                            argument.push_str(&annotation_token_text(&self.advance().kind));
                        }
                        TokenKind::Comma if nested_closers.is_empty() => {
                            self.advance();
                            arguments.push(std::mem::take(&mut argument));
                        }
                        TokenKind::Newline if nested_closers.is_empty() => {
                            return Err(self.error("Unterminated annotation arguments"));
                        }
                        _ => argument.push_str(&annotation_token_text(&self.advance().kind)),
                    }
                }
                if !closed || !nested_closers.is_empty() {
                    return Err(self.error("Unterminated annotation arguments"));
                }
            }
            annotations.push(Annotation {
                name: qualified_name,
                arguments,
            });
            self.skip_newlines();
        }
        Ok(annotations)
    }

    fn parse_java_class_def(
        &mut self,
        modifiers: Vec<String>,
        annotations: Vec<Annotation>,
    ) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.expect(&TokenKind::Class)?;
        let name = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected class name after 'class'")),
        };
        self.parse_class_body(name, modifiers, annotations, true, location)
    }

    fn parse_java_method_def(
        &mut self,
        modifiers: Vec<String>,
        annotations: Vec<Annotation>,
    ) -> KelpyResult<Statement> {
        if self.class_depth == 0 {
            return Err(self.error("Java-style method declarations must be inside a class"));
        }
        let location = self.current_location();
        let return_type = self.parse_java_type()?;
        let name = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected method name after return type")),
        };
        self.expect(&TokenKind::LParen)?;

        let mut params = Vec::new();
        let mut param_types = Vec::new();
        if !matches!(self.peek(), TokenKind::RParen) {
            loop {
                self.skip_newlines();
                let param_type = self.parse_java_type()?;
                let param_name = match self.peek().clone() {
                    TokenKind::Identifier(name) => {
                        self.advance();
                        name
                    }
                    _ => return Err(self.error("Expected parameter name after parameter type")),
                };
                param_types.push(Some(param_type));
                params.push(param_name);
                self.skip_newlines();
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(&TokenKind::RParen)?;
        if !modifiers.iter().any(|modifier| modifier == "static") {
            params.insert(0, "self".to_string());
            param_types.insert(0, None);
        }
        let body = self.parse_block()?;
        Ok(Statement::FunctionDef {
            name,
            params,
            param_types,
            return_type: Some(return_type),
            modifiers,
            annotations,
            body,
            location,
        })
    }

    fn parse_java_type(&mut self) -> KelpyResult<String> {
        let mut type_name = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected type name")),
        };

        while matches!(self.peek(), TokenKind::Dot) {
            self.advance();
            match self.peek().clone() {
                TokenKind::Identifier(part) => {
                    self.advance();
                    type_name.push('.');
                    type_name.push_str(&part);
                }
                _ => return Err(self.error("Expected type name after '.'")),
            }
        }

        if matches!(self.peek(), TokenKind::LessThan) {
            self.advance();
            type_name.push('<');
            loop {
                if matches!(self.peek(), TokenKind::GreaterThan | TokenKind::Eof) {
                    return Err(self.error("Expected a type argument before '>'"));
                }
                type_name.push_str(&self.parse_java_type()?);
                if matches!(self.peek(), TokenKind::Comma) {
                    self.advance();
                    type_name.push(',');
                } else if matches!(self.peek(), TokenKind::GreaterThan) {
                    self.advance();
                    type_name.push('>');
                    break;
                } else {
                    return Err(self.error("Expected ',' or '>' in generic type"));
                }
            }
        }

        while matches!(self.peek(), TokenKind::LBracket)
            && matches!(self.tokens.get(self.pos + 1).map(|token| &token.kind), Some(TokenKind::RBracket))
        {
            self.advance();
            self.advance();
            type_name.push_str("[]");
        }
        Ok(type_name)
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
                TokenKind::Self_ => {
                    self.advance();
                    params.push("self".to_string());
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
        match self.peek() {
            TokenKind::LBrace => self.parse_braced_block(),
            TokenKind::Colon => {
                self.advance();
                self.parse_end_block()
            }
            _ => Err(self.error("Expected '{' or ':' to start a block")),
        }
    }

    fn parse_braced_block(&mut self) -> KelpyResult<Vec<Statement>> {
        self.expect(&TokenKind::LBrace)?;
        self.skip_separators();

        let mut stmts = Vec::new();
        while !matches!(self.peek(), TokenKind::RBrace | TokenKind::Eof) {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.skip_separators();
        }

        self.expect(&TokenKind::RBrace)?;
        Ok(stmts)
    }

    fn parse_end_block(&mut self) -> KelpyResult<Vec<Statement>> {
        let stmts = self.parse_statements_until(&[TokenKind::End])?;
        self.expect(&TokenKind::End)?;
        Ok(stmts)
    }

    fn parse_statements_until(&mut self, terminators: &[TokenKind]) -> KelpyResult<Vec<Statement>> {
        self.skip_separators();
        let mut stmts = Vec::new();
        while !self.is_at_end() && !terminators.contains(self.peek()) {
            stmts.push(self.parse_statement()?);
            self.skip_separators();
        }
        Ok(stmts)
    }

    fn parse_if(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `if` or `elif`

        let condition = self.parse_expression(Precedence::None)?;

        if matches!(self.peek(), TokenKind::Colon) {
            self.advance();
            let then_body = self.parse_statements_until(&[
                TokenKind::Elif,
                TokenKind::Else,
                TokenKind::End,
            ])?;

            let mut elif_branches = Vec::new();
            while matches!(self.peek(), TokenKind::Elif) {
                let elif_location = self.current_location();
                self.advance();
                let elif_condition = self.parse_expression(Precedence::None)?;
                self.expect(&TokenKind::Colon)?;
                let elif_body = self.parse_statements_until(&[
                    TokenKind::Elif,
                    TokenKind::Else,
                    TokenKind::End,
                ])?;
                elif_branches.push((elif_condition, elif_body, elif_location));
            }

            let mut else_body = if matches!(self.peek(), TokenKind::Else) {
                self.advance();
                self.expect(&TokenKind::Colon)?;
                Some(self.parse_statements_until(&[TokenKind::End])?)
            } else {
                None
            };
            self.expect(&TokenKind::End)?;

            for (elif_condition, elif_body, elif_location) in elif_branches.into_iter().rev() {
                else_body = Some(vec![Statement::If {
                    condition: elif_condition,
                    then_body: elif_body,
                    else_body,
                    location: elif_location,
                }]);
            }

            return Ok(Statement::If {
                condition,
                then_body,
                else_body,
                location,
            });
        }

        let then_body = self.parse_block()?;

        self.skip_newlines();
        let else_body = if matches!(self.peek(), TokenKind::Elif) {
            // elif is parsed as another if in the else branch
            let elif = self.parse_if()?;
            Some(vec![elif])
        } else if matches!(self.peek(), TokenKind::Else) {
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

        let value = if matches!(
            self.peek(),
            TokenKind::Newline
                | TokenKind::RBrace
                | TokenKind::Semicolon
                | TokenKind::End
                | TokenKind::Else
                | TokenKind::Elif
                | TokenKind::Catch
                | TokenKind::Eof
        ) {
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

    fn parse_try_catch(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `try`
        let colon_delimited = matches!(self.peek(), TokenKind::Colon);
        let try_body = if colon_delimited {
            self.advance();
            self.parse_statements_until(&[TokenKind::Catch])?
        } else {
            self.parse_block()?
        };
        self.skip_newlines();
        self.expect(&TokenKind::Catch)?;
        self.expect(&TokenKind::LParen)?;
        let catch_var = match self.peek().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                name
            }
            _ => return Err(self.error("Expected variable name in catch()")),
        };
        self.expect(&TokenKind::RParen)?;
        let catch_body = if colon_delimited {
            self.expect(&TokenKind::Colon)?;
            let body = self.parse_statements_until(&[TokenKind::End])?;
            self.expect(&TokenKind::End)?;
            body
        } else {
            self.parse_block()?
        };
        Ok(Statement::TryCatch {
            try_body,
            catch_var,
            catch_body,
            location,
        })
    }

    fn parse_throw(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `throw`
        let value = self.parse_expression(Precedence::None)?;
        Ok(Statement::Throw { value, location })
    }

    fn parse_class_def(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        self.advance(); // consume `class`
        let name = match self.peek().clone() {
            TokenKind::Identifier(n) => {
                self.advance();
                n
            }
            _ => return Err(self.error("Expected class name after 'class'")),
        };
        self.parse_class_body(name, Vec::new(), Vec::new(), false, location)
    }

    fn parse_class_body(
        &mut self,
        name: String,
        modifiers: Vec<String>,
        annotations: Vec<Annotation>,
        java_style: bool,
        location: SourceLocation,
    ) -> KelpyResult<Statement> {
        self.skip_newlines();
        let colon_delimited = matches!(self.peek(), TokenKind::Colon);
        if colon_delimited {
            self.advance();
        } else {
            self.expect(&TokenKind::LBrace)?;
        }
        self.skip_separators();
        let mut methods = Vec::new();
        let close = if colon_delimited {
            TokenKind::End
        } else {
            TokenKind::RBrace
        };
        self.class_depth += 1;
        while self.peek() != &close && !self.is_at_end() {
            let method = self.parse_statement()?;
            if !matches!(method, Statement::FunctionDef { .. }) {
                return Err(self.error("Class bodies currently accept method declarations only"));
            }
            if let Statement::FunctionDef { name: method_name, .. } = &method {
                let already_defined = methods.iter().any(|existing| {
                    matches!(existing, Statement::FunctionDef { name, .. } if name == method_name)
                });
                if already_defined {
                    return Err(self.error(format!(
                        "Method '{}' is already defined in class '{}' (overloading is not supported)",
                        method_name, name
                    )));
                }
            }
            methods.push(method);
            self.skip_separators();
        }
        self.expect(&close)?;
        self.class_depth -= 1;
        let java_style = java_style
            || !modifiers.is_empty()
            || !annotations.is_empty()
            || methods.iter().any(|method| {
                matches!(method, Statement::FunctionDef { return_type: Some(_), .. })
            });
        Ok(Statement::ClassDef {
            name,
            methods,
            modifiers,
            annotations,
            java_style,
            location,
        })
    }

    fn parse_assignment_or_expr(&mut self) -> KelpyResult<Statement> {
        let location = self.current_location();
        if let Some(declaration) = self.try_parse_typed_assignment()? {
            return Ok(declaration);
        }
        let expr = self.parse_expression(Precedence::None)?;

        // Check if this is an assignment: `name = expr`
        if matches!(self.peek(), TokenKind::Equals) {
            self.advance(); // consume `=`
            let value = self.parse_expression(Precedence::None)?;
            return match expr {
                Expr::Identifier { name, .. } => Ok(Statement::Assignment {
                    name,
                    declared_type: None,
                    value,
                    location,
                }),
                Expr::MemberAccess { object, member, .. } => Ok(Statement::MemberAssignment {
                    object: *object,
                    member,
                    value,
                    location,
                }),
                Expr::Index { object, index, .. } => Ok(Statement::IndexAssignment {
                    object: *object,
                    index: *index,
                    value,
                    location,
                }),
                _ => Err(KelpyError::ParseError {
                    message: "Invalid assignment target".to_string(),
                    location,
                }),
            };
        }

        // Compound assignment: `name += expr`, etc.
        let compound_op = match self.peek() {
            TokenKind::PlusEquals => Some(CompoundOp::Add),
            TokenKind::MinusEquals => Some(CompoundOp::Subtract),
            TokenKind::StarEquals => Some(CompoundOp::Multiply),
            TokenKind::SlashEquals => Some(CompoundOp::Divide),
            _ => None,
        };
        if let Some(op) = compound_op {
            self.advance(); // consume the compound op
            if let Expr::Identifier { name, .. } = expr {
                let value = self.parse_expression(Precedence::None)?;
                return Ok(Statement::CompoundAssignment {
                    name,
                    op,
                    value,
                    location,
                });
            } else {
                return Err(KelpyError::ParseError {
                    message: "Invalid compound assignment target".to_string(),
                    location,
                });
            }
        }

        if let Expr::MethodCall {
            object,
            method,
            args,
            ..
        } = &expr
        {
            let is_system_out = matches!(
                object.as_ref(),
                Expr::MemberAccess { object, member, .. }
                    if member == "out"
                        && matches!(object.as_ref(), Expr::Identifier { name, .. } if name == "System")
            );
            if is_system_out && method == "println" {
                return match args.as_slice() {
                    [] => Ok(Statement::Print {
                        value: Expr::StringLiteral {
                            value: String::new(),
                            location: location.clone(),
                        },
                        location,
                    }),
                    [value] => Ok(Statement::Print {
                        value: value.clone(),
                        location,
                    }),
                    _ => Err(KelpyError::ParseError {
                        message: "System.out.println expects zero or one argument".to_string(),
                        location,
                    }),
                };
            }
        }

        Ok(Statement::ExprStatement { expr, location })
    }

    fn try_parse_typed_assignment(&mut self) -> KelpyResult<Option<Statement>> {
        let start = self.pos;
        let type_name = match self.parse_java_type() {
            Ok(name) => name,
            Err(_) => {
                self.pos = start;
                return Ok(None);
            }
        };
        let name = match self.peek().clone() {
            TokenKind::Identifier(name) => name,
            _ => {
                self.pos = start;
                return Ok(None);
            }
        };
        self.advance();
        if !matches!(self.peek(), TokenKind::Equals) {
            self.pos = start;
            return Ok(None);
        }
        self.advance();
        let value = self.parse_expression(Precedence::None)?;
        Ok(Some(Statement::Assignment {
            name,
            declared_type: Some(type_name),
            value,
            location: self.tokens[start].location.clone(),
        }))
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
            TokenKind::Self_ => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::Identifier {
                    name: "self".to_string(),
                    location: loc,
                })
            }
            TokenKind::Null => {
                let loc = self.current_location();
                self.advance();
                Ok(Expr::NullLiteral { location: loc })
            }
            TokenKind::New => {
                let loc = self.current_location();
                self.advance();
                let class_name = match self.peek().clone() {
                    TokenKind::Identifier(n) => {
                        self.advance();
                        n
                    }
                    _ => return Err(self.error("Expected class name after 'new'")),
                };
                self.expect(&TokenKind::LParen)?;
                let args = self.parse_arg_list()?;
                self.expect(&TokenKind::RParen)?;
                Ok(Expr::New {
                    class_name,
                    args,
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
            // Member access or method call
            TokenKind::Dot => {
                let loc = self.current_location();
                self.advance(); // consume .
                match self.peek().clone() {
                    TokenKind::Identifier(member) => {
                        self.advance();
                        // If followed by `(`, it's a method call
                        if matches!(self.peek(), TokenKind::LParen) {
                            self.advance(); // consume (
                            let args = self.parse_arg_list()?;
                            self.expect(&TokenKind::RParen)?;
                            Ok(Expr::MethodCall {
                                object: Box::new(left),
                                method: member,
                                args,
                                location: loc,
                            })
                        } else {
                            Ok(Expr::MemberAccess {
                                object: Box::new(left),
                                member,
                                location: loc,
                            })
                        }
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
    fn test_colon_end_blocks_are_indentation_independent_and_nest_correctly() {
        let source = "if true:\nprint 1\nif false:\nprint 2\nend\nelif false:\nprint 3\nelse:\nprint 4\nend";
        let program = parse(source).unwrap();
        match &program.statements[0] {
            Statement::If {
                then_body,
                else_body: Some(elif_branch),
                ..
            } => {
                assert_eq!(then_body.len(), 2);
                match &elif_branch[0] {
                    Statement::If {
                        then_body,
                        else_body: Some(else_body),
                        ..
                    } => {
                        assert_eq!(then_body.len(), 1);
                        assert_eq!(else_body.len(), 1);
                    }
                    other => panic!("Expected lowered elif branch, got: {:?}", other),
                }
            }
            other => panic!("Expected if statement, got: {:?}", other),
        }
    }

    #[test]
    fn test_colon_end_blocks_for_functions_loops_classes_and_try_catch() {
        let source = "def count(n):\nfor i in range(n):\nprint i\nend\nend\nclass Counter:\ndef value(self):\nreturn 1\nend\nend\ntry:\nthrow \"bad\"\ncatch (err):\nprint err\nend";
        let program = parse(source).unwrap();
        assert_eq!(program.statements.len(), 3);
        assert!(matches!(program.statements[0], Statement::FunctionDef { .. }));
        assert!(matches!(program.statements[1], Statement::ClassDef { .. }));
        assert!(matches!(program.statements[2], Statement::TryCatch { .. }));
    }

    #[test]
    fn test_braced_blocks_remain_supported() {
        let program = parse("if true { print 1 }").unwrap();
        assert!(matches!(program.statements[0], Statement::If { .. }));
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
    fn test_member_and_index_assignment_targets() {
        let program = parse("self.name = name\ndata[\"key\"] = 42").unwrap();
        assert!(matches!(program.statements[0], Statement::MemberAssignment { .. }));
        assert!(matches!(program.statements[1], Statement::IndexAssignment { .. }));
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

    #[test]
    fn parses_java_style_annotations_classes_methods_and_semicolons() {
        let source = r#"
@Deprecated(since = "2")
public class Main {
    @Override
    public static int addNumbers(int num1, int num2) {
        int sum = num1 + num2;
        return sum;
    }

    public static void main(String[] args) {
        int result = addNumbers(5, 10);
        System.out.println("The sum is: " + result);
    }
}
"#;
        let program = parse(source).expect("Java-style class should parse");
        let Statement::ClassDef {
            annotations,
            java_style,
            methods,
            ..
        } = &program.statements[0]
        else {
            panic!("Expected class definition");
        };
        assert!(*java_style);
        assert_eq!(annotations[0].name, "Deprecated");
        assert_eq!(annotations[0].arguments, vec!["since=\"2\""]);

        let Statement::FunctionDef {
            name,
            params,
            param_types,
            return_type,
            modifiers,
            annotations,
            body,
            ..
        } = &methods[0]
        else {
            panic!("Expected annotated method");
        };
        assert_eq!(name, "addNumbers");
        assert_eq!(params, &["num1", "num2"]);
        assert_eq!(param_types, &[Some("int".to_string()), Some("int".to_string())]);
        assert_eq!(return_type.as_deref(), Some("int"));
        assert!(modifiers.contains(&"static".to_string()));
        assert_eq!(annotations[0].name, "Override");
        assert!(matches!(
            &body[0],
            Statement::Assignment {
                declared_type: Some(type_name),
                ..
            } if type_name == "int"
        ));

        let Statement::FunctionDef { body, params, .. } = &methods[1] else {
            panic!("Expected main method");
        };
        assert_eq!(params, &["args"]);
        assert!(matches!(body[0], Statement::Assignment { .. }));
        assert!(matches!(body[1], Statement::Print { .. }));
    }

    #[test]
    fn parses_nested_generic_types_and_annotation_values() {
        let source = r#"
@Meta(tags = {"fast", "safe"}, type = String.class)
public class Types {
    public static java.util.Map<String, java.util.List<Integer>> copy(
        java.util.Map<String, Integer> values
    ) {
        return values;
    }
}
"#;
        let program = parse(source).expect("nested generic declarations should parse");
        let Statement::ClassDef { annotations, methods, .. } = &program.statements[0] else {
            panic!("Expected class definition");
        };
        assert_eq!(annotations[0].arguments, vec!["tags={\"fast\",\"safe\"}", "type=String.class"]);
        let Statement::FunctionDef { param_types, return_type, .. } = &methods[0] else {
            panic!("Expected generic method definition");
        };
        assert_eq!(return_type.as_deref(), Some("java.util.Map<String,java.util.List<Integer>>"));
        assert_eq!(param_types, &[Some("java.util.Map<String,Integer>".to_string())]);
    }

    #[test]
    fn rejects_mismatched_annotation_argument_delimiters() {
        let error = parse("@Meta(values = {1, 2])\nclass Types {}").unwrap_err();
        assert!(error.to_string().contains("Mismatched delimiter in annotation arguments"));
    }

    #[test]
    fn rejects_duplicate_class_methods_instead_of_silently_replacing_them() {
        let source = "class Math { def add(self, n) {} def add(self, n, m) {} }";
        let error = parse(source).unwrap_err();
        assert!(error.to_string().contains("overloading is not supported"));
    }

    #[test]
    fn supports_empty_java_println_and_rejects_unsupported_arity() {
        let program = parse("System.out.println();").unwrap();
        assert!(matches!(
            &program.statements[0],
            Statement::Print {
                value: Expr::StringLiteral { value, .. },
                ..
            } if value.is_empty()
        ));
        let error = parse("System.out.println(1, 2);").unwrap_err();
        assert!(error.to_string().contains("expects zero or one argument"));
    }
}

