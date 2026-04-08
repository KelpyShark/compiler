/// KelpyShark Lexer
///
/// Tokenizes KelpyShark source code into a stream of tokens.
/// Handles: identifiers, keywords, string/number/boolean literals,
/// operators, punctuation, comments (single-line # and multi-line ###).

use crate::error::{KelpyError, KelpyResult, SourceLocation};

// ──────────────────────────────────────────────
//  Token types
// ──────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // ── Literals ──
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(f64),
    BooleanLiteral(bool),

    // ── Keywords ──
    Def,
    If,
    Elif,
    Else,
    While,
    For,
    In,
    Return,
    Import,
    True,
    False,
    And,
    Or,
    Not,
    Print,
    Break,
    Continue,
    Try,
    Catch,
    Throw,
    Class,
    Null,
    New,
    Self_,

    // ── Operators ──
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Percent,      // %
    Equals,       // =
    EqualEqual,   // ==
    NotEqual,     // !=
    LessThan,     // <
    LessEqual,    // <=
    GreaterThan,  // >
    GreaterEqual, // >=
    PlusEquals,   // +=
    MinusEquals,  // -=
    StarEquals,   // *=
    SlashEquals,  // /=

    // ── Punctuation ──
    LParen,   // (
    RParen,   // )
    LBrace,   // {
    RBrace,   // }
    LBracket, // [
    RBracket, // ]
    Comma,    // ,
    Colon,    // :
    Dot,      // .

    // ── Special ──
    Newline,
    Eof,
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(s) => write!(f, "IDENTIFIER({})", s),
            TokenKind::StringLiteral(s) => write!(f, "STRING(\"{}\")", s),
            TokenKind::NumberLiteral(n) => write!(f, "NUMBER({})", n),
            TokenKind::BooleanLiteral(b) => write!(f, "BOOL({})", b),
            TokenKind::Def => write!(f, "DEF"),
            TokenKind::If => write!(f, "IF"),
            TokenKind::Elif => write!(f, "ELIF"),
            TokenKind::Else => write!(f, "ELSE"),
            TokenKind::While => write!(f, "WHILE"),
            TokenKind::For => write!(f, "FOR"),
            TokenKind::In => write!(f, "IN"),
            TokenKind::Return => write!(f, "RETURN"),
            TokenKind::Import => write!(f, "IMPORT"),
            TokenKind::True => write!(f, "TRUE"),
            TokenKind::False => write!(f, "FALSE"),
            TokenKind::And => write!(f, "AND"),
            TokenKind::Or => write!(f, "OR"),
            TokenKind::Not => write!(f, "NOT"),
            TokenKind::Print => write!(f, "PRINT"),
            TokenKind::Break => write!(f, "BREAK"),
            TokenKind::Continue => write!(f, "CONTINUE"),
            TokenKind::Try => write!(f, "TRY"),
            TokenKind::Catch => write!(f, "CATCH"),
            TokenKind::Throw => write!(f, "THROW"),
            TokenKind::Class => write!(f, "CLASS"),
            TokenKind::Null => write!(f, "NULL"),
            TokenKind::New => write!(f, "NEW"),
            TokenKind::Self_ => write!(f, "SELF"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Percent => write!(f, "%"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::PlusEquals => write!(f, "+="),
            TokenKind::MinusEquals => write!(f, "-="),
            TokenKind::StarEquals => write!(f, "*="),
            TokenKind::SlashEquals => write!(f, "/="),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Newline => write!(f, "NEWLINE"),
            TokenKind::Eof => write!(f, "EOF"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub location: SourceLocation,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Token {
            kind,
            location: SourceLocation { line, column },
        }
    }
}

// ──────────────────────────────────────────────
//  Lexer
// ──────────────────────────────────────────────

pub struct Lexer {
    source: Vec<char>,
    pos: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
            source: source.chars().collect(),
            pos: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenize the entire source into a Vec<Token>.
    pub fn tokenize(&mut self) -> KelpyResult<Vec<Token>> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }

    // ── Character helpers ──

    fn peek(&self) -> Option<char> {
        self.source.get(self.pos).copied()
    }

    fn peek_next(&self) -> Option<char> {
        self.source.get(self.pos + 1).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.source.get(self.pos).copied();
        if let Some(c) = ch {
            self.pos += 1;
            if c == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        ch
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' || ch == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn make_token(&self, kind: TokenKind, start_line: usize, start_col: usize) -> Token {
        Token::new(kind, start_line, start_col)
    }

    fn error(&self, message: impl Into<String>) -> KelpyError {
        KelpyError::LexerError {
            message: message.into(),
            location: SourceLocation {
                line: self.line,
                column: self.column,
            },
        }
    }

    // ── Main tokenizer ──

    fn next_token(&mut self) -> KelpyResult<Token> {
        self.skip_whitespace();

        let start_line = self.line;
        let start_col = self.column;

        let ch = match self.peek() {
            Some(c) => c,
            None => return Ok(self.make_token(TokenKind::Eof, start_line, start_col)),
        };

        // ── Newlines ──
        if ch == '\n' {
            self.advance();
            return Ok(self.make_token(TokenKind::Newline, start_line, start_col));
        }

        // ── Comments ──
        if ch == '#' {
            return self.lex_comment(start_line, start_col);
        }

        // ── Strings ──
        if ch == '"' {
            return self.lex_string(start_line, start_col);
        }

        // ── Numbers ──
        if ch.is_ascii_digit() {
            return self.lex_number(start_line, start_col);
        }

        // ── Identifiers & keywords ──
        if ch.is_alphabetic() || ch == '_' {
            return self.lex_identifier(start_line, start_col);
        }

        // ── Operators & punctuation ──
        self.lex_operator_or_punctuation(start_line, start_col)
    }

    // ── Comment lexing ──

    fn lex_comment(&mut self, start_line: usize, start_col: usize) -> KelpyResult<Token> {
        // Check for multi-line comment ###
        if self.peek() == Some('#')
            && self.peek_at(1) == Some('#')
            && self.peek_at(2) == Some('#')
        {
            // Skip opening ###
            self.advance();
            self.advance();
            self.advance();

            // Read until closing ###
            loop {
                match self.peek() {
                    None => {
                        return Err(KelpyError::LexerError {
                            message: "Unterminated multi-line comment".to_string(),
                            location: SourceLocation {
                                line: start_line,
                                column: start_col,
                            },
                        });
                    }
                    Some('#') if self.peek_at(1) == Some('#') && self.peek_at(2) == Some('#') => {
                        self.advance();
                        self.advance();
                        self.advance();
                        break;
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
        } else {
            // Single-line comment: skip to end of line
            while let Some(c) = self.peek() {
                if c == '\n' {
                    break;
                }
                self.advance();
            }
        }

        // After a comment, get the next real token
        self.next_token()
    }

    fn peek_at(&self, offset: usize) -> Option<char> {
        self.source.get(self.pos + offset).copied()
    }

    // ── String lexing ──

    fn lex_string(&mut self, start_line: usize, start_col: usize) -> KelpyResult<Token> {
        self.advance(); // consume opening "
        let mut value = String::new();

        loop {
            match self.peek() {
                None | Some('\n') => {
                    return Err(KelpyError::LexerError {
                        message: "Unterminated string literal".to_string(),
                        location: SourceLocation {
                            line: start_line,
                            column: start_col,
                        },
                    });
                }
                Some('"') => {
                    self.advance(); // consume closing "
                    break;
                }
                Some('\\') => {
                    self.advance(); // consume backslash
                    match self.peek() {
                        Some('n') => {
                            value.push('\n');
                            self.advance();
                        }
                        Some('t') => {
                            value.push('\t');
                            self.advance();
                        }
                        Some('\\') => {
                            value.push('\\');
                            self.advance();
                        }
                        Some('"') => {
                            value.push('"');
                            self.advance();
                        }
                        Some(c) => {
                            value.push('\\');
                            value.push(c);
                            self.advance();
                        }
                        None => {
                            return Err(KelpyError::LexerError {
                                message: "Unterminated escape sequence".to_string(),
                                location: SourceLocation {
                                    line: self.line,
                                    column: self.column,
                                },
                            });
                        }
                    }
                }
                Some(c) => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        Ok(self.make_token(
            TokenKind::StringLiteral(value),
            start_line,
            start_col,
        ))
    }

    // ── Number lexing ──

    fn lex_number(&mut self, start_line: usize, start_col: usize) -> KelpyResult<Token> {
        let mut num_str = String::new();
        let mut has_dot = false;

        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                num_str.push(ch);
                self.advance();
            } else if ch == '.' && !has_dot {
                // Check that the next char is a digit (so `5.method()` doesn't eat the dot)
                if let Some(next) = self.peek_next() {
                    if next.is_ascii_digit() {
                        has_dot = true;
                        num_str.push('.');
                        self.advance();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        let value: f64 = num_str.parse().map_err(|_| KelpyError::LexerError {
            message: format!("Invalid number literal: {}", num_str),
            location: SourceLocation {
                line: start_line,
                column: start_col,
            },
        })?;

        Ok(self.make_token(
            TokenKind::NumberLiteral(value),
            start_line,
            start_col,
        ))
    }

    // ── Identifier & keyword lexing ──

    fn lex_identifier(&mut self, start_line: usize, start_col: usize) -> KelpyResult<Token> {
        let mut ident = String::new();

        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let kind = match ident.as_str() {
            "def" => TokenKind::Def,
            "if" => TokenKind::If,
            "elif" => TokenKind::Elif,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "in" => TokenKind::In,
            "return" => TokenKind::Return,
            "import" => TokenKind::Import,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "and" => TokenKind::And,
            "or" => TokenKind::Or,
            "not" => TokenKind::Not,
            "print" => TokenKind::Print,
            "break" => TokenKind::Break,
            "continue" => TokenKind::Continue,
            "try" => TokenKind::Try,
            "catch" => TokenKind::Catch,
            "throw" => TokenKind::Throw,
            "class" => TokenKind::Class,
            "null" => TokenKind::Null,
            "new" => TokenKind::New,
            "self" => TokenKind::Self_,
            _ => TokenKind::Identifier(ident),
        };

        Ok(self.make_token(kind, start_line, start_col))
    }

    // ── Operator & punctuation lexing ──

    fn lex_operator_or_punctuation(
        &mut self,
        start_line: usize,
        start_col: usize,
    ) -> KelpyResult<Token> {
        let ch = self.advance().unwrap();

        let kind = match ch {
            '+' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::PlusEquals
                } else {
                    TokenKind::Plus
                }
            }
            '-' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::MinusEquals
                } else {
                    TokenKind::Minus
                }
            }
            '*' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::StarEquals
                } else {
                    TokenKind::Star
                }
            }
            '/' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::SlashEquals
                } else {
                    TokenKind::Slash
                }
            }
            '%' => TokenKind::Percent,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '.' => TokenKind::Dot,
            '=' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equals
                }
            }
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::NotEqual
                } else {
                    return Err(self.error(format!("Unexpected character: '!'  (did you mean '!='?)")));
                }
            }
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::LessEqual
                } else {
                    TokenKind::LessThan
                }
            }
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::GreaterThan
                }
            }
            _ => {
                return Err(self.error(format!("Unexpected character: '{}'", ch)));
            }
        };

        Ok(self.make_token(kind, start_line, start_col))
    }
}

// ──────────────────────────────────────────────
//  Tests
// ──────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: tokenize source and return just the token kinds (filtering newlines).
    fn token_kinds(source: &str) -> Vec<TokenKind> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().expect("Lexer should not fail");
        tokens
            .into_iter()
            .filter(|t| t.kind != TokenKind::Newline)
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn test_empty_source() {
        let kinds = token_kinds("");
        assert_eq!(kinds, vec![TokenKind::Eof]);
    }

    #[test]
    fn test_number_literals() {
        let kinds = token_kinds("42 3.14");
        assert_eq!(
            kinds,
            vec![
                TokenKind::NumberLiteral(42.0),
                TokenKind::NumberLiteral(3.14),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_string_literal() {
        let kinds = token_kinds(r#""hello world""#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringLiteral("hello world".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_string_escape_sequences() {
        let kinds = token_kinds(r#""line1\nline2\ttab\\backslash""#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::StringLiteral("line1\nline2\ttab\\backslash".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_keywords() {
        let kinds = token_kinds("def if else while for in return import true false and or not print");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Def,
                TokenKind::If,
                TokenKind::Else,
                TokenKind::While,
                TokenKind::For,
                TokenKind::In,
                TokenKind::Return,
                TokenKind::Import,
                TokenKind::True,
                TokenKind::False,
                TokenKind::And,
                TokenKind::Or,
                TokenKind::Not,
                TokenKind::Print,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_identifiers() {
        let kinds = token_kinds("foo bar_baz _private x1");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier("foo".to_string()),
                TokenKind::Identifier("bar_baz".to_string()),
                TokenKind::Identifier("_private".to_string()),
                TokenKind::Identifier("x1".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_operators() {
        let kinds = token_kinds("+ - * / % = == != < <= > >=");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Plus,
                TokenKind::Minus,
                TokenKind::Star,
                TokenKind::Slash,
                TokenKind::Percent,
                TokenKind::Equals,
                TokenKind::EqualEqual,
                TokenKind::NotEqual,
                TokenKind::LessThan,
                TokenKind::LessEqual,
                TokenKind::GreaterThan,
                TokenKind::GreaterEqual,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_punctuation() {
        let kinds = token_kinds("( ) { } [ ] , : .");
        assert_eq!(
            kinds,
            vec![
                TokenKind::LParen,
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::RBrace,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::Comma,
                TokenKind::Colon,
                TokenKind::Dot,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_single_line_comment() {
        let kinds = token_kinds("x = 5 # this is a comment\ny = 10");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::Equals,
                TokenKind::NumberLiteral(5.0),
                TokenKind::Identifier("y".to_string()),
                TokenKind::Equals,
                TokenKind::NumberLiteral(10.0),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_multi_line_comment() {
        let kinds = token_kinds("x = 1\n### this is\na multi-line\ncomment ###\ny = 2");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier("x".to_string()),
                TokenKind::Equals,
                TokenKind::NumberLiteral(1.0),
                TokenKind::Identifier("y".to_string()),
                TokenKind::Equals,
                TokenKind::NumberLiteral(2.0),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_assignment_expression() {
        let kinds = token_kinds(r#"name = "KelpyShark""#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::Identifier("name".to_string()),
                TokenKind::Equals,
                TokenKind::StringLiteral("KelpyShark".to_string()),
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_function_definition() {
        let kinds = token_kinds("def greet(name) {\n    print name\n}");
        assert_eq!(
            kinds,
            vec![
                TokenKind::Def,
                TokenKind::Identifier("greet".to_string()),
                TokenKind::LParen,
                TokenKind::Identifier("name".to_string()),
                TokenKind::RParen,
                TokenKind::LBrace,
                TokenKind::Print,
                TokenKind::Identifier("name".to_string()),
                TokenKind::RBrace,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_dict_literal() {
        let kinds = token_kinds(r#"{"key": "value", "num": 42}"#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::LBrace,
                TokenKind::StringLiteral("key".to_string()),
                TokenKind::Colon,
                TokenKind::StringLiteral("value".to_string()),
                TokenKind::Comma,
                TokenKind::StringLiteral("num".to_string()),
                TokenKind::Colon,
                TokenKind::NumberLiteral(42.0),
                TokenKind::RBrace,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_list_literal() {
        let kinds = token_kinds(r#"["apple", "banana", "orange"]"#);
        assert_eq!(
            kinds,
            vec![
                TokenKind::LBracket,
                TokenKind::StringLiteral("apple".to_string()),
                TokenKind::Comma,
                TokenKind::StringLiteral("banana".to_string()),
                TokenKind::Comma,
                TokenKind::StringLiteral("orange".to_string()),
                TokenKind::RBracket,
                TokenKind::Eof,
            ]
        );
    }

    #[test]
    fn test_unterminated_string() {
        let mut lexer = Lexer::new(r#""hello"#);
        let result = lexer.tokenize();
        assert!(result.is_err());
        match result.unwrap_err() {
            KelpyError::LexerError { message, .. } => {
                assert!(message.contains("Unterminated string"));
            }
            other => panic!("Expected LexerError, got: {:?}", other),
        }
    }

    #[test]
    fn test_unexpected_character() {
        let mut lexer = Lexer::new("x = @");
        let result = lexer.tokenize();
        assert!(result.is_err());
        match result.unwrap_err() {
            KelpyError::LexerError { message, .. } => {
                assert!(message.contains("Unexpected character"));
            }
            other => panic!("Expected LexerError, got: {:?}", other),
        }
    }

    #[test]
    fn test_location_tracking() {
        let mut lexer = Lexer::new("x = 5\ny = 10");
        let tokens = lexer.tokenize().unwrap();
        // x is at line 1, col 1
        assert_eq!(tokens[0].location, SourceLocation { line: 1, column: 1 });
        // = is at line 1, col 3
        assert_eq!(tokens[1].location, SourceLocation { line: 1, column: 3 });
        // 5 is at line 1, col 5
        assert_eq!(tokens[2].location, SourceLocation { line: 1, column: 5 });
        // newline at line 1, col 6
        assert_eq!(tokens[3].location, SourceLocation { line: 1, column: 6 });
        // y is at line 2, col 1
        assert_eq!(tokens[4].location, SourceLocation { line: 2, column: 1 });
    }

    #[test]
    fn test_full_example_program() {
        let source = r#"bob = {
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
"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize();
        assert!(tokens.is_ok(), "Full example should tokenize without error");
        let tokens = tokens.unwrap();
        // Should start with Identifier("bob")
        assert_eq!(tokens[0].kind, TokenKind::Identifier("bob".to_string()));
        // Should end with Eof
        assert_eq!(tokens.last().unwrap().kind, TokenKind::Eof);
    }
}
