## Contributing to Coba

Thank you for your interest in contributing to the Coba programming language and compiler project!

## Development Setup

### Prerequisites

- Rust toolchain (1.70 or later)
- Cargo
- Git

### Building the Project

```bash
# Clone the repository
git clone <repository-url>
cd coba

# Build all crates
cargo build

# Run tests
cargo test

# Build the CLI tool
cargo build --release --bin coba-cli
```

### Project Structure

```
coba/
├── crates/
│   ├── coba-lexer/      # Tokenization for Coba and COBOL
│   ├── coba-parser/     # Parsing to AST
│   ├── coba-ast/        # AST definitions
│   ├── coba-semantic/   # Type checking and analysis
│   ├── coba-codegen/    # Code generation (Coba ↔ COBOL)
│   └── coba-cli/        # Command-line interface
├── examples/            # Example Coba programs
├── LANGUAGE_SPEC.md     # Language specification
└── README.md            # Project overview
```

## Development Workflow

### 1. Adding New Features

When adding a new language feature:

1. **Update the Language Spec** (`LANGUAGE_SPEC.md`)
   - Document the syntax
   - Explain the semantics
   - Add examples
   - Document COBOL mapping

2. **Add to Lexer** (`coba-lexer`)
   - Add new token types if needed
   - Update keyword recognition
   - Add tests

3. **Add to Parser** (`coba-parser`)
   - Update grammar rules
   - Add new AST node types if needed
   - Add tests

4. **Add to Semantic Analysis** (`coba-semantic`)
   - Update type checking if needed
   - Update symbol table if needed
   - Add tests

5. **Add to Code Generation** (`coba-codegen`)
   - Implement COBOL generation
   - Update name mangling if needed
   - Add tests

6. **Add Examples**
   - Create example programs demonstrating the feature
   - Add to `examples/` directory

### 2. Testing

We use several levels of testing:

**Unit Tests**
```bash
# Run tests for a specific crate
cargo test -p coba-lexer
cargo test -p coba-parser
cargo test -p coba-semantic
```

**Integration Tests**
```bash
# Run all tests
cargo test

# Run tests with verbose output
cargo test -- --nocapture
```

**Manual Testing**
```bash
# Compile an example program
cargo run --bin coba-cli compile examples/simple.coba output.cob

# Check a program for errors
cargo run --bin coba-cli check examples/simple.coba
```

### 3. Code Style

- Follow Rust standard coding conventions
- Use `rustfmt` for formatting: `cargo fmt`
- Use `clippy` for linting: `cargo clippy`
- Add documentation comments for public APIs
- Keep functions focused and under 100 lines when possible

### 4. Commit Messages

Use clear, descriptive commit messages:

```
Add support for procedure parameters

- Update parser to handle parameter lists
- Add parameter type checking
- Generate COBOL linkage section
- Add tests and examples
```

## Areas for Contribution

### High Priority

- **COBOL Decompiler**: Complete the COBOL → Coba decompiler
  - COBOL parser implementation
  - GO TO elimination algorithm
  - Control flow reconstruction
  - Type inference from PICTURE clauses

- **Recursion Detection**: Implement call graph analysis
  - Build call graph from procedure calls
  - DFS-based cycle detection
  - Error reporting for recursive calls

- **Control Flow Analysis**: Advanced control flow transformations
  - GO TO elimination patterns
  - Loop detection and reconstruction
  - Simplification of complex control flow

### Medium Priority

- **Error Messages**: Improve error reporting
  - Better error messages with suggestions
  - Error recovery in parser
  - Warnings for common issues

- **Testing**: Expand test coverage
  - More edge case tests
  - Roundtrip tests (Coba → COBOL → Coba)
  - Performance benchmarks

- **Documentation**: Expand documentation
  - Tutorial for getting started
  - Advanced topics guide
  - API documentation

### Low Priority

- **Optimizations**: Code generation improvements
  - Dead code elimination
  - Constant folding
  - Optimize COBOL output

- **Tooling**: Developer tools
  - Language server protocol (LSP)
  - Syntax highlighting
  - IDE integration

## Questions?

Feel free to open an issue for:
- Bug reports
- Feature requests
- Questions about the codebase
- Suggestions for improvements

## License

By contributing to Coba, you agree that your contributions will be licensed under the MIT License.
