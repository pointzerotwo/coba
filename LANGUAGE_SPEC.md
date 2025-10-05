# Coba Language Specification

Version 0.1.0

## Table of Contents

1. [Introduction](#introduction)
2. [Lexical Structure](#lexical-structure)
3. [Type System](#type-system)
4. [Variables and Declarations](#variables-and-declarations)
5. [Expressions](#expressions)
6. [Statements](#statements)
7. [Procedures](#procedures)
8. [Control Flow](#control-flow)
9. [Scoping Rules](#scoping-rules)
10. [Reserved Words and Name Mangling](#reserved-words-and-name-mangling)
11. [COBOL Interoperability](#cobol-interoperability)
12. [Edge Cases](#edge-cases)

---

## 1. Introduction

Coba is a modern programming language designed to provide a contemporary syntax for writing COBOL-compatible programs. It features:

- Clean, readable syntax inspired by JavaScript/TypeScript
- Strong type system with explicit numeric precision
- Bidirectional compilation with COBOL
- Modern control flow constructs
- Lexical scoping

### Design Goals

1. **Readability**: Code should be self-documenting and easy to understand
2. **Safety**: Strong typing prevents common errors
3. **Interoperability**: Seamless conversion to/from COBOL
4. **Familiarity**: Syntax familiar to modern developers

---

## 2. Lexical Structure

### Comments

```coba
// Single-line comment

/*
   Multi-line comment
   spanning multiple lines
*/
```

### Keywords

Reserved keywords in Coba:

- `dec`
- `text`
- `int`
- `bool`
- `function`
- `if`
- `elif`
- `else`
- `while`
- `for`
- `return`
- `set`
- `call`
- `true`
- `false`

### Identifiers

Identifiers must:
- Start with a letter (a-z, A-Z) or underscore
- Contain only letters, digits, and underscores
- Not be a reserved keyword

```coba
// Valid identifiers
customerName
total_price
account123
_private
```

### Literals

#### Numeric Literals
```coba
42          // Integer
123.45      // Decimal
0.99        // Decimal
-17.50      // Negative decimal
```

#### String Literals
```coba
"Hello, World!"
"John Doe"
""          // Empty string
```

#### Boolean Literals
```coba
true
false
```

---

## 3. Type System

Coba has a strong, static type system designed to map cleanly to COBOL's PICTURE clauses.

### Primitive Types

#### `dec(p, s)`

Fixed-point decimal number with precision `p` and scale `s`.

- `p`: Total number of digits (1-18)
- `s`: Number of decimal places (0-p)

```coba
dec(9,2) price      // Max value: 9999999.99 (COBOL: PIC 9(7)V99)
dec(5,0) count      // Max value: 99999 (COBOL: PIC 9(5))
dec(3,3) percent    // Max value: 0.999 (COBOL: PIC V999)
```

**Edge Cases:**
- Overflow at runtime → COBOL runtime error
- Values are truncated to specified precision
- Maximum value = 10^(p-s) - 10^(-s)

#### `text(n)`

Fixed-length text string of exactly `n` characters.

- `n`: Length in characters (1-9999)

```coba
text(50) customerName   // COBOL: PIC X(50)
text(10) zipCode        // COBOL: PIC X(10)
text(1) flag            // COBOL: PIC X
```

**Edge Cases:**
- Strings longer than `n` are truncated
- Strings shorter than `n` are padded with spaces (COBOL behavior)
- No compile-time error for truncation

#### `int`

Shorthand for `dec(9,0)`.

```coba
int count           // Equivalent to dec(9,0)
```

#### `bool`

True/false value. Maps to COBOL `PIC 9` with values 0/1.

```coba
bool isActive
```

### Array Types

Arrays are declared by appending `[size]` to any primitive type. Arrays are fixed-size and map to COBOL's `OCCURS` clause.

```coba
int[10] scores                  // Array of 10 integers (COBOL: OCCURS 10 TIMES)
dec(9,2)[5] prices              // Array of 5 decimals
text(30)[100] names             // Array of 100 text strings
```

**Indexing:**
- Arrays are 0-indexed in Coba (converted to 1-indexed in COBOL)
- Index out of bounds → COBOL runtime error
- Array elements accessed with `arr[index]` syntax

```coba
int[5] numbers
set numbers[0] = 100            // First element
set numbers[4] = 500            // Last element
print numbers[2]                // Access element
```

**COBOL Mapping:**
```cobol
01 GLOBAL-NUMBERS OCCURS 5 TIMES PIC 9(9).
MOVE 100 TO GLOBAL-NUMBERS(1).  ← Note: 1-indexed in COBOL
```

---

## 4. Variables and Declarations

### Variable Declaration

Variables must be declared with an explicit type and optional initializer.

```coba
decimal(9,2) totalPrice
text(50) customerName = "Unknown"
integer itemCount = 0
boolean isProcessed = false
```

### Syntax

```
<type> <identifier> [= <expression>]
```

### Initialization

- Variables without initializers are set to default values:
  - Numeric types: `0` (or `0.00` for decimals)
  - `text`: Empty string (spaces)
  - `boolean`: `false`

---

## 5. Expressions

### Binary Operators

#### Arithmetic Operators
```coba
+   // Addition
-   // Subtraction
*   // Multiplication
/   // Division
%   // Modulo
**  // Exponentiation (right-associative: 2**3**2 = 2**(3**2) = 512)
```

#### Comparison Operators
```coba
==  // Equal
!=  // Not equal
<   // Less than
<=  // Less than or equal
>   // Greater than
>=  // Greater than or equal
```

#### Logical Operators
```coba
&&  // Logical AND
||  // Logical OR
!   // Logical NOT
```

### Unary Operators
```coba
-   // Negation
!   // Logical NOT
```

### Operator Precedence

1. `!`, `-` (unary)
2. `*`, `/`, `%`
3. `+`, `-`
4. `<`, `<=`, `>`, `>=`
5. `==`, `!=`
6. `&&`
7. `||`

---

## 6. Statements

### Assignment Statement

```coba
set variable = expression
```

Example:
```coba
set totalPrice = price * quantity
set customerName = "Jane Smith"
```

**Note:** Division by zero compiles successfully but causes COBOL runtime error.

### Procedure Call Statement

```coba
call procedureName(arg1, arg2, ...)
```

Example:
```coba
call calculateTotal(price, quantity)
call processOrder(orderId)
```

### Print Statement

Print values to the console. Maps to COBOL's `DISPLAY` statement.

```coba
print expression
print expr1, expr2, expr3
```

Examples:
```coba
print "Hello, World!"
print customerName
print "Total: ", price * quantity
print message, " - Status: ", counter
```

Any type can be printed (text, decimal, integer, boolean). Multiple values can be printed in a single statement by separating them with commas.

### Accept Statement

Accept user input or system values. Maps to COBOL's `ACCEPT` statement.

```coba
accept variable              // Accept user input
accept variable from date    // Accept system date (YYYYMMDD)
accept variable from time    // Accept system time
accept variable from day     // Accept day of year (YYYYDDD)
accept variable from dayofweek  // Accept day of week (1-7)
```

Examples:
```coba
text(50) username
int currentDate
text(10) currentTime

accept username
print "Hello, ", username

accept currentDate from date
accept currentTime from time
print "Date: ", currentDate, " Time: ", currentTime
```

**COBOL Mapping:**
```cobol
ACCEPT GLOBAL-USERNAME.
ACCEPT GLOBAL-CURRENTDATE FROM DATE YYYYMMDD.
ACCEPT GLOBAL-CURRENTTIME FROM TIME.
```

### Evaluate Statement

Multi-way conditional branching, similar to switch/case in other languages. Maps to COBOL's `EVALUATE` statement.

```coba
evaluate expression {
    when value1:
        // statements
    when value2:
        // statements
    when rangeStart..rangeEnd:
        // statements
    when other:
        // statements
}
```

Examples:
```coba
// Single value matching
evaluate grade {
    when 4:
        print "Excellent!"
    when 3:
        print "Good"
    when 2:
        print "Fair"
    when other:
        print "Needs improvement"
}

// Range matching
evaluate score {
    when 90..100:
        print "Grade: A"
    when 80..89:
        print "Grade: B"
    when 70..79:
        print "Grade: C"
    when 60..69:
        print "Grade: D"
    when other:
        print "Grade: F"
}

// Complex example
evaluate status {
    when 1:
        print "Active"
        call processActive()
    when 2..5:
        print "Pending"
        call processPending()
    when other:
        print "Inactive"
        call processInactive()
}
```

**COBOL Mapping:**
```cobol
EVALUATE GLOBAL-SCORE
    WHEN 90 THRU 100
        DISPLAY "Grade: A"
    WHEN 80 THRU 89
        DISPLAY "Grade: B"
    WHEN OTHER
        DISPLAY "Grade: F"
END-EVALUATE
```

**Features:**
- Single values: `when 5:`
- Ranges: `when 10..20:` (maps to COBOL `WHEN 10 THRU 20`)
- Default case: `when other:` (maps to COBOL `WHEN OTHER`)
- Multiple statements per branch supported

---

## 7. Functions

Functions are named blocks of code that can accept parameters and execute statements.

### Syntax

```coba
function functionName(param1Type param1Name, param2Type param2Name, ...) {
    // statements
}
```

### Examples

```coba
// Function with no parameters
function initialize() {
    set counter = 0
    set total = 0.00
}

// Function with parameters
function calculateDiscount(dec(9,2) originalPrice, dec(5,2) discountRate) {
    set discountedPrice = originalPrice * (1.00 - discountRate)
}

// Empty function (valid)
function empty() {
}
```

### Return Values

Current version: Functions do not return values (COBOL paragraph behavior).

Future version may support return values.

---

## 8. Control Flow

### If Statement

```coba
if condition {
    // statements
}

if condition {
    // statements
} elif condition2 {
    // statements
} else {
    // statements
}
```

Example:
```coba
if price > 100.00 {
    set discount = 0.10
} elif price > 50.00 {
    set discount = 0.05
} else {
    set discount = 0.00
}
```

### While Loop

```coba
while condition {
    // statements
}
```

Example:
```coba
while counter < 10 {
    set counter = counter + 1
}
```

### For Loop

```coba
for variable = start to end step increment {
    // statements
}
```

Example:
```coba
for i = 1 to 10 step 1 {
    set total = total + i
}
```

### Nesting

Coba supports arbitrary nesting depth (tested to 20+ levels).

```coba
if a {
    if b {
        if c {
            // 3 levels deep
        }
    }
}
```

---

## 9. Scoping Rules

### Lexical Scoping

Variables are scoped to the procedure in which they are declared.

### Global Variables

Variables declared outside any procedure are global.

```coba
decimal(9,2) globalTotal = 0.00

procedure addToTotal(decimal(9,2) amount)
    set globalTotal = globalTotal + amount
end
```

### Local Variables

Variables declared inside a procedure are local to that procedure.

```coba
procedure calculate()
    decimal(9,2) localResult = 0.00
    set localResult = 100.00 * 1.5
end
// localResult is not accessible here
```

### Name Mangling

To avoid COBOL name collisions and scope issues, Coba uses name mangling:

- Local variables: `LOCAL-<PROC>-<VAR>`
- Parameters: `PARAM-<PROC>-<INDEX>`
- Global variables: `GLOBAL-<VAR>`

Example:
```coba
decimal(9,2) total

procedure calculate(decimal(9,2) amount)
    decimal(9,2) result
end
```

Maps to COBOL:
```cobol
01 GLOBAL-TOTAL PIC 9(7)V99.

CALCULATE.
    01 PARAM-CALCULATE-0 PIC 9(7)V99.
    01 LOCAL-CALCULATE-RESULT PIC 9(7)V99.
```

---

## 10. Reserved Words and Name Mangling

### COBOL Reserved Words

Coba automatically escapes/mangles identifiers that conflict with COBOL reserved words.

Common COBOL reserved words:
- PERFORM, MOVE, ACCEPT, DISPLAY
- ADD, SUBTRACT, MULTIPLY, DIVIDE
- IF, ELSE, END-IF
- SECTION, PARAGRAPH
- And many more...

### Mangling Strategy

If a Coba identifier matches a COBOL reserved word, it's prefixed with `COBA-`:

```coba
decimal(5,2) PERFORM = 10.00
```

Maps to:
```cobol
01 COBA-PERFORM PIC 9(3)V99.
```

---

## 11. COBOL Interoperability

### Compilation (Coba → COBOL)

Coba code is compiled to standard COBOL with:
- IDENTIFICATION DIVISION
- ENVIRONMENT DIVISION
- DATA DIVISION with PICTURE clauses
- PROCEDURE DIVISION with paragraphs

### Decompilation (COBOL → Coba)

COBOL code is decompiled to Coba by:
- Parsing PICTURE clauses to infer types
- Converting paragraphs to procedures
- Eliminating GO TO statements via control flow analysis
- Detecting and flagging recursion
- Reverse name mangling

### Column Rules

COBOL's strict column rules are handled:
- Columns 1-6: Sequence numbers (generated)
- Column 7: Indicator area (comments, continuations)
- Columns 8-72: Code area
- Columns 73-80: Ignored

---

## 12. Edge Cases

### Numeric Precision Overflow

**Behavior:** Overflow is a COBOL runtime error, not a compiler error.

```coba
decimal(5,2) value = 999.99
set value = value + 1.00  // Runtime overflow in COBOL
```

**Documentation:** Maximum values are documented for each precision.

### Text Truncation

**Behavior:** Strings are truncated to declared length without compile-time error.

```coba
text(10) message = "Hello, World!"  // Stored as "Hello, Wor"
```

**Warning:** Optional compiler flag for truncation warnings.

### Division by Zero

**Behavior:** Compiles successfully, runtime error in COBOL.

```coba
set x = a / 0.00  // Compiles, runtime error
```

### Empty Procedures

**Behavior:** Valid Coba, generates empty COBOL paragraph.

```coba
procedure empty()
end
```

### Unusual Names (Reserved Words)

**Behavior:** Automatic escaping with `COBA-` prefix.

```coba
decimal(5,2) PERFORM = 10.00  // Maps to COBA-PERFORM
```

### Deep Nesting

**Behavior:** Supports arbitrary nesting, tested to 20+ levels.

```coba
if a then
    if b then
        if c then
            // ... up to 20+ levels
        end
    end
end
```

---

## Appendix A: Grammar

```
program         → declaration* EOF

declaration     → varDecl
                | procDecl

varDecl         → type IDENTIFIER ( "=" expression )? ";"

procDecl        → "procedure" IDENTIFIER "(" parameters? ")"
                  statement*
                  "end"

parameters      → type IDENTIFIER ( "," type IDENTIFIER )*

type            → "decimal" "(" NUMBER "," NUMBER ")"
                | "text" "(" NUMBER ")"
                | "integer"
                | "boolean"

statement       → assignStmt
                | callStmt
                | ifStmt
                | whileStmt
                | forStmt

assignStmt      → "set" IDENTIFIER "=" expression

callStmt        → "call" IDENTIFIER "(" arguments? ")"

ifStmt          → "if" expression "then"
                  statement*
                  ( "elif" expression "then" statement* )*
                  ( "else" statement* )?
                  "end"

whileStmt       → "while" expression "do"
                  statement*
                  "end"

forStmt         → "for" IDENTIFIER "=" expression "to" expression "step" expression "do"
                  statement*
                  "end"

expression      → logical_or

logical_or      → logical_and ( "||" logical_and )*

logical_and     → equality ( "&&" equality )*

equality        → comparison ( ( "==" | "!=" ) comparison )*

comparison      → term ( ( "<" | "<=" | ">" | ">=" ) term )*

term            → factor ( ( "+" | "-" ) factor )*

factor          → unary ( ( "*" | "/" | "%" ) unary )*

unary           → ( "!" | "-" ) unary
                | primary

primary         → NUMBER
                | STRING
                | "true"
                | "false"
                | IDENTIFIER
                | "(" expression ")"

arguments       → expression ( "," expression )*
```

---

## Appendix B: COBOL Mapping Reference

| Coba Type | COBOL PICTURE | Example |
|-----------|---------------|---------|
| decimal(9,2) | PIC 9(7)V99 | 9999999.99 |
| decimal(5,0) | PIC 9(5) | 99999 |
| text(50) | PIC X(50) | 50 characters |
| integer | PIC 9(9) | 999999999 |
| boolean | PIC 9 | 0 or 1 |

---

## Appendix C: Version History

- **0.1.0** (Current): Initial specification
