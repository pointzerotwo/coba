/// Parser for the coba programming language
///
/// This module provides parsing functionality to convert tokens
/// into an Abstract Syntax Tree (AST).

pub mod parser;
pub mod error;

pub use parser::Parser;
pub use error::ParseError;
