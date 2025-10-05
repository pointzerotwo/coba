/// Semantic analysis for coba
///
/// This module provides semantic analysis including type checking,
/// symbol table management, and validation of program semantics.

pub mod analyzer;
pub mod symbol_table;
pub mod type_checker;
pub mod scope;

pub use analyzer::SemanticAnalyzer;
pub use symbol_table::SymbolTable;
