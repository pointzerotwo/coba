/// Lexer for the coba programming language and COBOL
///
/// This module provides tokenization for both coba source code
/// and COBOL source code, converting raw text into a stream of tokens for the parser.

pub mod token;
pub mod lexer;
pub mod cobol_token;
pub mod cobol_lexer;

pub use token::{Token, TokenKind};
pub use lexer::Lexer;
pub use cobol_token::{CobolToken, CobolTokenKind};
pub use cobol_lexer::CobolLexer;
