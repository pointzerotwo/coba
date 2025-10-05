/// Code generation for coba
///
/// This module provides code generation capabilities including:
/// - coba to COBOL compilation
/// - COBOL to coba decompilation
/// - Name mangling and reserved word handling

pub mod cobol_gen;
pub mod coba_gen;
pub mod name_mangler;

pub use cobol_gen::CobolGenerator;
pub use coba_gen::CobaGenerator;
