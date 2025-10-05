# Coba Programming Language

**Coba** is a modern programming language that provides a contemporary way to write COBOL. It features clean, readable syntax inspired by JavaScript/TypeScript while maintaining full interoperability with COBOL through bidirectional compilation.

## Overview

Coba bridges the gap between modern programming practices and legacy COBOL systems by providing:

- **Modern Syntax**: Clean, readable code inspired by JavaScript/TypeScript
- **Bidirectional Compilation**: Convert seamlessly between Coba and COBOL
- **Type Safety**: Strong type system with inference
- **COBOL Compatibility**: Generate valid, idiomatic COBOL code

## Features

### Language Features

- Modern type system (decimal, text, etc.)
- Structured control flow (if/elif/else, loops)
- Procedure definitions with parameters
- Lexical scoping with proper variable management
- Reserved word handling and name mangling

### Compiler Features

- **Coba → COBOL**: Compile modern Coba code to COBOL
- **COBOL → Coba**: Decompile existing COBOL to readable Coba
- **Recursion Detection**: Build call graphs and detect recursive patterns
- **GO TO Elimination**: Convert spaghetti code to structured control flow
- **Type Inference**: Reverse-engineer types from PICTURE clauses
- **Column Rule Handling**: Proper COBOL positional syntax support

## Architecture

The project is organized as a Rust workspace with the following crates:

- **coba-lexer**: Tokenization for both Coba and COBOL
- **coba-parser**: Parsing to Abstract Syntax Tree
- **coba-ast**: AST node definitions
- **coba-semantic**: Type checking and semantic analysis
- **coba-codegen**: Code generation (Coba ↔ COBOL)
- **coba-cli**: Command-line interface

## Example

```coba
dec(9,2) totalPrice = 0.00
text(50) customerName = "John Doe"

function calculateTotal(dec(9,2) price, dec(5,0) quantity) {
    set totalPrice = price * quantity
}
```

Compiles to valid COBOL with proper PICTURE clauses, PROCEDURE DIVISION structure, and column formatting.

## Edge Cases

Coba handles various COBOL edge cases gracefully:

- **Numeric Precision**: Documents maximum values, runtime overflow errors
- **Text Truncation**: Truncates to declared length (COBOL behavior)
- **Division by Zero**: Compiles successfully, runtime error in COBOL
- **Empty Procedures**: Valid syntax, generates empty COBOL paragraphs
- **Reserved Words**: Automatic escaping/mangling of COBOL keywords
- **Deep Nesting**: Supports arbitrary nesting levels (tested to 20+)

## Development

### Building

```bash
cargo build
```

### Running

```bash
# Compile Coba to COBOL
cargo run --bin coba-cli compile input.coba output.cob

# Decompile COBOL to Coba
cargo run --bin coba-cli decompile input.cob output.coba
```

### Testing

```bash
cargo test
```

## Project Status

🚧 **In Active Development** 🚧

This project is currently in early development. Core features are being implemented.

## License

MIT

## Contributing

Contributions are welcome! Please see our contributing guidelines for more information.
