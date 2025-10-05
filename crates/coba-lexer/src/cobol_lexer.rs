/// Column-aware lexer for COBOL
///
/// COBOL uses a fixed column format:
/// - Columns 1-6: Sequence numbers (ignored)
/// - Column 7: Indicator area (*, -, D, or space)
/// - Columns 8-72: Code area (Area A: 8-11, Area B: 12-72)
/// - Columns 73-80: Identification area (ignored)

use crate::cobol_token::{CobolToken, CobolTokenKind};

pub struct CobolLexer {
    lines: Vec<String>,
    current_line: usize,
    current_col: usize,
    tokens: Vec<CobolToken>,
}

impl CobolLexer {
    pub fn new(source: &str) -> Self {
        Self {
            lines: source.lines().map(|s| s.to_string()).collect(),
            current_line: 0,
            current_col: 0,
            tokens: Vec::new(),
        }
    }

    /// Tokenize the entire COBOL source
    pub fn tokenize(&mut self) -> Vec<CobolToken> {
        while self.current_line < self.lines.len() {
            self.tokenize_line();
            self.current_line += 1;
        }

        self.tokens.push(CobolToken::new(
            CobolTokenKind::Eof,
            String::new(),
            self.current_line,
            0,
        ));

        self.tokens.clone()
    }

    fn tokenize_line(&mut self) {
        if self.current_line >= self.lines.len() {
            return;
        }

        let line = &self.lines[self.current_line];

        // Handle short lines
        if line.len() < 7 {
            return; // Skip lines that are too short
        }

        // Column 7 (index 6) - Indicator area
        let indicator = line.chars().nth(6).unwrap_or(' ');

        match indicator {
            '*' | '/' => {
                // Comment line - skip
                return;
            }
            '-' => {
                // Continuation line - handle specially
                // For now, we'll treat it as a normal line
            }
            _ => {
                // Normal line or blank indicator
            }
        }

        // Extract code area (columns 8-72, indices 7-71)
        let code_area = if line.len() > 72 {
            line[7..72].to_string()
        } else if line.len() > 7 {
            line[7..].to_string()
        } else {
            return;
        };

        // Tokenize the code area
        self.current_col = 7; // Start at column 8 (0-indexed is 7)
        self.tokenize_code_area(&code_area);
    }

    fn tokenize_code_area(&mut self, code: &str) {
        let chars: Vec<char> = code.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            let start_col = self.current_col + i;

            // Skip whitespace
            if chars[i].is_whitespace() {
                i += 1;
                continue;
            }

            // String literals
            if chars[i] == '"' || chars[i] == '\'' {
                let quote = chars[i];
                let start = i;
                i += 1;
                while i < chars.len() && chars[i] != quote {
                    i += 1;
                }
                if i < chars.len() {
                    i += 1; // Include closing quote
                }
                let lexeme: String = chars[start..i].iter().collect();
                self.tokens.push(CobolToken::new(
                    CobolTokenKind::String,
                    lexeme,
                    self.current_line + 1,
                    start_col,
                ));
                continue;
            }

            // Numbers
            if chars[i].is_ascii_digit() || (chars[i] == '-' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit()) {
                let start = i;
                if chars[i] == '-' {
                    i += 1;
                }
                while i < chars.len() && (chars[i].is_ascii_digit() || chars[i] == '.') {
                    i += 1;
                }
                let lexeme: String = chars[start..i].iter().collect();
                self.tokens.push(CobolToken::new(
                    CobolTokenKind::Number,
                    lexeme,
                    self.current_line + 1,
                    start_col,
                ));
                continue;
            }

            // Period (statement terminator)
            if chars[i] == '.' {
                self.tokens.push(CobolToken::new(
                    CobolTokenKind::Period,
                    ".".to_string(),
                    self.current_line + 1,
                    start_col,
                ));
                i += 1;
                continue;
            }

            // Other single-character tokens
            match chars[i] {
                '(' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::LeftParen,
                        "(".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                ')' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::RightParen,
                        ")".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                ',' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Comma,
                        ",".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                '+' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Plus,
                        "+".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                '*' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Star,
                        "*".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                '/' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Slash,
                        "/".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                '=' => {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Equal,
                        "=".to_string(),
                        self.current_line + 1,
                        start_col,
                    ));
                    i += 1;
                    continue;
                }
                '>' => {
                    if i + 1 < chars.len() && chars[i + 1] == '=' {
                        self.tokens.push(CobolToken::new(
                            CobolTokenKind::GreaterEqual,
                            ">=".to_string(),
                            self.current_line + 1,
                            start_col,
                        ));
                        i += 2;
                    } else {
                        self.tokens.push(CobolToken::new(
                            CobolTokenKind::Greater,
                            ">".to_string(),
                            self.current_line + 1,
                            start_col,
                        ));
                        i += 1;
                    }
                    continue;
                }
                '<' => {
                    if i + 1 < chars.len() && chars[i + 1] == '=' {
                        self.tokens.push(CobolToken::new(
                            CobolTokenKind::LessEqual,
                            "<=".to_string(),
                            self.current_line + 1,
                            start_col,
                        ));
                        i += 2;
                    } else if i + 1 < chars.len() && chars[i + 1] == '>' {
                        self.tokens.push(CobolToken::new(
                            CobolTokenKind::NotEqual,
                            "<>".to_string(),
                            self.current_line + 1,
                            start_col,
                        ));
                        i += 2;
                    } else {
                        self.tokens.push(CobolToken::new(
                            CobolTokenKind::Less,
                            "<".to_string(),
                            self.current_line + 1,
                            start_col,
                        ));
                        i += 1;
                    }
                    continue;
                }
                _ => {}
            }

            // Identifiers and keywords
            if chars[i].is_alphabetic() || chars[i] == '-' {
                let start = i;
                while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '-' || chars[i] == '_') {
                    i += 1;
                }
                let lexeme: String = chars[start..i].iter().collect();

                // Check if it's a level number
                if let Some(level_kind) = CobolTokenKind::try_level(&lexeme) {
                    self.tokens.push(CobolToken::new(
                        level_kind,
                        lexeme,
                        self.current_line + 1,
                        start_col,
                    ));
                } else if let Some(keyword) = CobolTokenKind::from_keyword(&lexeme) {
                    self.tokens.push(CobolToken::new(
                        keyword,
                        lexeme,
                        self.current_line + 1,
                        start_col,
                    ));
                } else {
                    self.tokens.push(CobolToken::new(
                        CobolTokenKind::Identifier,
                        lexeme,
                        self.current_line + 1,
                        start_col,
                    ));
                }
                continue;
            }

            // Unknown character - skip it
            i += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_cobol() {
        let source = r#"       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN."#;

        let mut lexer = CobolLexer::new(source);
        let tokens = lexer.tokenize();

        // Should find IDENTIFICATION, DATA, PROCEDURE divisions
        assert!(tokens.iter().any(|t| t.kind == CobolTokenKind::IdentificationDivision));
        assert!(tokens.iter().any(|t| t.kind == CobolTokenKind::DataDivision));
        assert!(tokens.iter().any(|t| t.kind == CobolTokenKind::ProcedureDivision));
    }

    #[test]
    fn test_data_definition() {
        let source = "       01 WS-TOTAL PIC 9(7)V99.";

        let mut lexer = CobolLexer::new(source);
        let tokens = lexer.tokenize();

        assert_eq!(tokens[0].kind, CobolTokenKind::Level(1));
        assert_eq!(tokens[1].kind, CobolTokenKind::Identifier);
        assert_eq!(tokens[2].kind, CobolTokenKind::Pic);
    }

    #[test]
    fn test_comment_line() {
        let source = r#"      *This is a comment
       01 WS-VAR PIC X."#;

        let mut lexer = CobolLexer::new(source);
        let tokens = lexer.tokenize();

        // Comment line should be skipped
        assert_eq!(tokens[0].kind, CobolTokenKind::Level(1));
    }
}
