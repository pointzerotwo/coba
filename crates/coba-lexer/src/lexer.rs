/// Lexer for the Coba programming language

use crate::token::{Token, TokenKind};

pub struct Lexer {
    source: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
        }
    }

    /// Tokenize the entire source and return a vector of tokens
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }

        tokens
    }

    /// Get the next token from the source
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenKind::Eof);
        }

        let c = self.advance();

        match c {
            // Single-character tokens
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            '[' => self.make_token(TokenKind::LeftBracket),
            ']' => self.make_token(TokenKind::RightBracket),
            ',' => self.make_token(TokenKind::Comma),
            ':' => self.make_token(TokenKind::Colon),
            '+' => self.make_token(TokenKind::Plus),
            '-' => self.make_token(TokenKind::Minus),
            '%' => self.make_token(TokenKind::Percent),

            // Two-character tokens
            '*' => {
                if self.match_char('*') {
                    self.make_token(TokenKind::StarStar)
                } else {
                    self.make_token(TokenKind::Star)
                }
            }

            '.' => {
                if self.match_char('.') {
                    self.make_token(TokenKind::DotDot)
                } else {
                    self.error_token("Unexpected character: '.'")
                }
            }

            '/' => {
                if self.match_char('/') {
                    // Single-line comment
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                    self.next_token()
                } else if self.match_char('*') {
                    // Multi-line comment
                    self.skip_block_comment();
                    self.next_token()
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }

            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::EqualEqual)
                } else {
                    self.make_token(TokenKind::Equal)
                }
            }

            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::BangEqual)
                } else {
                    self.make_token(TokenKind::Bang)
                }
            }

            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::LessEqual)
                } else {
                    self.make_token(TokenKind::Less)
                }
            }

            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenKind::GreaterEqual)
                } else {
                    self.make_token(TokenKind::Greater)
                }
            }

            '&' => {
                if self.match_char('&') {
                    self.make_token(TokenKind::AmpersandAmpersand)
                } else {
                    self.error_token("Expected '&&'")
                }
            }

            '|' => {
                if self.match_char('|') {
                    self.make_token(TokenKind::PipePipe)
                } else {
                    self.error_token("Expected '||'")
                }
            }

            // String literals
            '"' => self.string(),

            // Number literals
            c if c.is_ascii_digit() => self.number(),

            // Identifiers and keywords
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),

            _ => self.error_token(&format!("Unexpected character: {}", c)),
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.column = 0;
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn skip_block_comment(&mut self) {
        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == Some('/') {
                self.advance(); // consume '*'
                self.advance(); // consume '/'
                break;
            }
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }
    }

    fn string(&mut self) -> Token {
        while !self.is_at_end() && self.peek() != '"' {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string");
        }

        // Consume closing quote
        self.advance();

        self.make_token(TokenKind::String)
    }

    fn number(&mut self) -> Token {
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }

        // Look for decimal point
        if !self.is_at_end() && self.peek() == '.' && self.peek_next().map_or(false, |c| c.is_ascii_digit()) {
            // Consume the '.'
            self.advance();

            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(TokenKind::Number)
    }

    fn identifier(&mut self) -> Token {
        while !self.is_at_end() && (self.peek().is_ascii_alphanumeric() || self.peek() == '_') {
            self.advance();
        }

        let text: String = self.source[self.start..self.current].iter().collect();
        let kind = TokenKind::from_keyword(&text).unwrap_or(TokenKind::Identifier);

        self.make_token(kind)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let lexeme: String = self.source[self.start..self.current].iter().collect();
        Token::new(kind, lexeme, self.line, self.column - (self.current - self.start))
    }

    fn error_token(&self, message: &str) -> Token {
        Token::new(
            TokenKind::Error(message.to_string()),
            String::new(),
            self.line,
            self.column,
        )
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        c
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source[self.current]
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            None
        } else {
            Some(self.source[self.current + 1])
        }
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source[self.current] != expected {
            false
        } else {
            self.current += 1;
            self.column += 1;
            true
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_tokens() {
        let mut lexer = Lexer::new("+ - * / ( ) { }");
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Plus);
        assert_eq!(tokens[1].kind, TokenKind::Minus);
        assert_eq!(tokens[2].kind, TokenKind::Star);
        assert_eq!(tokens[3].kind, TokenKind::Slash);
        assert_eq!(tokens[4].kind, TokenKind::LeftParen);
        assert_eq!(tokens[5].kind, TokenKind::RightParen);
        assert_eq!(tokens[6].kind, TokenKind::LeftBrace);
        assert_eq!(tokens[7].kind, TokenKind::RightBrace);
        assert_eq!(tokens[8].kind, TokenKind::Eof);
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("dec text int bool function if");
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Dec);
        assert_eq!(tokens[1].kind, TokenKind::Text);
        assert_eq!(tokens[2].kind, TokenKind::Int);
        assert_eq!(tokens[3].kind, TokenKind::Bool);
        assert_eq!(tokens[4].kind, TokenKind::Function);
        assert_eq!(tokens[5].kind, TokenKind::If);
    }

    #[test]
    fn test_number_literal() {
        let mut lexer = Lexer::new("123 45.67");
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Number);
        assert_eq!(tokens[0].lexeme, "123");
        assert_eq!(tokens[1].kind, TokenKind::Number);
        assert_eq!(tokens[1].lexeme, "45.67");
    }

    #[test]
    fn test_string_literal() {
        let mut lexer = Lexer::new(r#""Hello, World!""#);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(tokens[0].lexeme, r#""Hello, World!""#);
    }

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("customerName total_price _private");
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, TokenKind::Identifier);
        assert_eq!(tokens[0].lexeme, "customerName");
        assert_eq!(tokens[1].kind, TokenKind::Identifier);
        assert_eq!(tokens[1].lexeme, "total_price");
        assert_eq!(tokens[2].kind, TokenKind::Identifier);
        assert_eq!(tokens[2].lexeme, "_private");
    }

    #[test]
    fn test_comments() {
        let mut lexer = Lexer::new("// single line\n/* multi\nline */ dec");
        let tokens = lexer.tokenize();

        // Comments should be skipped
        assert_eq!(tokens[0].kind, TokenKind::Dec);
    }
}
