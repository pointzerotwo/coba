## Getting Started with Coba

Welcome to Coba! This guide will help you get started with writing and compiling Coba programs.

## Installation

### Prerequisites

- Rust toolchain (1.70 or later)
- Cargo

### Building from Source

```bash
git clone <repository-url>
cd coba
cargo build --release
```

The compiled binary will be in `target/release/coba-cli`.

## Your First Coba Program

Create a file called `hello.coba`:

```coba
text(50) message = "Hello, Coba!"

function greet()
    // This function will be compiled to a COBOL paragraph
}
```

Compile it to COBOL:

```bash
cargo run --bin coba-cli compile hello.coba hello.cob
```

## Language Basics

### Variables and Types

Coba has a strong type system designed to map to COBOL's PICTURE clauses:

```coba
// Decimal numbers with precision and scale
dec(9,2) price = 99.99      // Up to 9999999.99
dec(5,0) quantity = 100      // Up to 99999

// Fixed-length text strings
text(50) customerName = "John Doe"
text(10) productCode = "ABC123"

// Integers (shorthand for dec(9,0))
int count = 42

// Booleans
bool isActive = true
```

### Procedures

Procedures are like functions but don't return values (matching COBOL paragraphs):

```coba
// Simple procedure
function initialize()
    set count = 0
    set total = 0.00
}

// Procedure with parameters
function calculateTotal(dec(9,2) unitPrice, int qty)
    dec(9,2) result
    set result = unitPrice * qty
}
```

### Control Flow

#### If/Elif/Else

```coba
function checkPrice(dec(9,2) price)
    if price > 100.00 {
        set discount = 0.10
    elif price > 50.00 {
        set discount = 0.05
    else
        set discount = 0.00
    }
}
```

#### While Loops

```coba
function countToTen()
    int i = 0

    while i < 10 {
        set i = i + 1
    }
}
```

#### For Loops

```coba
function sumRange()
    int i
    dec(9,2) total = 0.00

    for i = 1 to 100 step 1 {
        set total = total + i
    }
}
```

### Expressions

Coba supports standard arithmetic and logical operators:

```coba
// Arithmetic
set result = a + b
set result = a - b
set result = a * b
set result = a / b
set result = a % b

// Comparison
if x == y then }
if x != y then }
if x < y then }
if x > y then }

// Logical
if a && b then }
if a || b then }
if !a then }
```

## Command-Line Interface

### Compile

Compile a Coba program to COBOL:

```bash
cargo run --bin coba-cli compile input.coba output.cob
```

### Check

Check a Coba program for errors without generating code:

```bash
cargo run --bin coba-cli check input.coba
```

### Decompile (Coming Soon)

Decompile COBOL to Coba:

```bash
cargo run --bin coba-cli decompile input.cob output.coba
```

## How Coba Maps to COBOL

### Data Types

| Coba Type | COBOL PICTURE | Max Value |
|-----------|---------------|-----------|
| `dec(9,2)` | `PIC 9(7)V99` | 9999999.99 |
| `dec(5,0)` | `PIC 9(5)` | 99999 |
| `text(50)` | `PIC X(50)` | 50 chars |
| `integer` | `PIC 9(9)` | 999999999 |
| `boolean` | `PIC 9` | 0 or 1 |

### Variable Names

Coba uses name mangling to avoid COBOL reserved words and handle scoping:

```coba
// Coba
dec(9,2) totalPrice

// COBOL
01 GLOBAL-TOTAL-PRICE PIC 9(7)V99.
```

Local variables and parameters are also mangled:

```
dec(9,2) result  →  LOCAL-PROCEDURE-RESULT
parameter amount     →  PARAM-PROCEDURE-0
```

### Procedures

```coba
// Coba
function calculate()
    set total = 0.00
}

// COBOL
CALCULATE.
    MOVE 0.00 TO GLOBAL-TOTAL.
```

### Control Flow

```coba
// Coba
if x > 10 {
    set y = 1
else
    set y = 0
}

// COBOL
IF X > 10
    MOVE 1 TO Y
ELSE
    MOVE 0 TO Y
END-IF.
```

## Edge Cases

Coba handles several COBOL edge cases gracefully:

### Numeric Overflow

Overflow is detected at runtime, not compile time:

```coba
dec(5,2) value = 999.99
set value = value + 1.00  // Runtime overflow
```

### Text Truncation

Strings are truncated to the declared length:

```coba
text(10) msg = "Hello, World!"  // Stored as "Hello, Wor"
```

### Division by Zero

Compiles successfully, causes runtime error:

```coba
set result = x / 0.00  // Compiles, runtime error in COBOL
```

### Reserved Words

COBOL reserved words are automatically escaped:

```coba
dec(5,2) PERFORM = 10.00  // Maps to COBA-PERFORM in COBOL
```

## Examples

Check out the `examples/` directory for more complete examples:

- `simple.coba` - Basic variables and procedures
- `control_flow.coba` - If/while/for statements
- `types.coba` - All data types and type checking

## Next Steps

- Read the [Language Specification](LANGUAGE_SPEC.md) for complete details
- Check out [Contributing Guide](CONTRIBUTING.md) to help improve Coba
- Explore the codebase in `crates/` to understand the compiler architecture

## Getting Help

- Open an issue on GitHub for bugs or questions
- Check existing documentation in the repository
- Read the language spec for detailed syntax and semantics
