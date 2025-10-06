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

**Type Keywords:**
- `dec`
- `text`
- `int`
- `bool`

**Control Flow:**
- `function`
- `if`
- `elif`
- `else`
- `while`
- `for`
- `to`
- `step`
- `return`
- `evaluate`
- `when`
- `other`
- `continue`
- `exit`

**Statements:**
- `set`
- `call`
- `print`
- `accept`
- `initialize`

**File I/O:**
- `file`
- `open`
- `close`
- `read`
- `write`
- `input`
- `output`
- `extend`
- `record`
- `sequential`
- `organization`
- `access`
- `status`

**String Operations:**
- `string`
- `unstring`
- `inspect`
- `delimited`
- `by`
- `into`
- `replacing`
- `all`
- `tallying`
- `with`
- `pointer`

**Search Operations:**
- `search`
- `searchall`
- `varying`
- `at`
- `end`
- `indexed`

**Literals:**
- `true`
- `false`

**Sources:**
- `from`

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

### Initialize Statement

Reset variables to their default values. Maps to COBOL's `INITIALIZE` statement.

```coba
initialize variable1, variable2, variable3
```

Examples:
```coba
text(50) name
int counter
dec(9,2) total

initialize name, counter, total
// name = spaces, counter = 0, total = 0.00
```

**COBOL Mapping:**
```cobol
INITIALIZE GLOBAL-NAME.
INITIALIZE GLOBAL-COUNTER.
INITIALIZE GLOBAL-TOTAL.
```

### Continue Statement

No-operation placeholder. Maps to COBOL's `CONTINUE` statement.

```coba
continue
```

Example:
```coba
if someCondition {
    continue  // Placeholder for future logic
}
```

**COBOL Mapping:**
```cobol
CONTINUE.
```

### Exit Statement

Exit from the current function or paragraph. Maps to COBOL's `EXIT` statement.

```coba
exit
```

Example:
```coba
function processData() {
    if errorCondition {
        exit
    }
    // Continue processing
}
```

**COBOL Mapping:**
```cobol
EXIT.
```

### String Operations

#### String Concatenation

Concatenate multiple strings with delimiters. Maps to COBOL's `STRING` statement.

```coba
string source1 delimited by delimiter1, source2 delimited by size into destination
string source1, source2 into destination with pointer ptr
```

Examples:
```coba
text(30) firstName = "John"
text(30) lastName = "Doe"
text(60) fullName
int ptr

// Concatenate with delimiter
string firstName delimited by " ", " " delimited by size, lastName delimited by " " into fullName

// Concatenate with pointer
string firstName, " ", lastName into fullName with pointer ptr
```

**COBOL Mapping:**
```cobol
STRING GLOBAL-FIRSTNAME DELIMITED BY SPACE
       " " DELIMITED BY SIZE
       GLOBAL-LASTNAME DELIMITED BY SPACE
       INTO GLOBAL-FULLNAME.

STRING GLOBAL-FIRSTNAME DELIMITED BY SIZE
       " " DELIMITED BY SIZE
       GLOBAL-LASTNAME DELIMITED BY SIZE
       INTO GLOBAL-FULLNAME
       WITH POINTER GLOBAL-PTR.
```

**Notes:**
- `delimited by size` means use the entire string
- `delimited by expression` stops at the delimiter
- `with pointer` tracks the position in the destination

#### String Splitting

Split a string by delimiter into multiple parts. Maps to COBOL's `UNSTRING` statement.

```coba
unstring source delimited by delimiter into dest1, dest2, dest3
unstring source delimited by delimiter into dest1, dest2 with pointer ptr tallying counter
```

Examples:
```coba
text(60) fullName = "John Doe"
text(30) firstName
text(30) lastName
int ptr
int count

// Simple split
unstring fullName delimited by " " into firstName, lastName

// Split with pointer and tallying
unstring fullName delimited by " " into firstName, lastName with pointer ptr tallying count
```

**COBOL Mapping:**
```cobol
UNSTRING GLOBAL-FULLNAME
    DELIMITED BY " "
    INTO GLOBAL-FIRSTNAME, GLOBAL-LASTNAME.

UNSTRING GLOBAL-FULLNAME
    DELIMITED BY " "
    INTO GLOBAL-FIRSTNAME, GLOBAL-LASTNAME
    WITH POINTER GLOBAL-PTR
    TALLYING IN GLOBAL-COUNT.
```

#### String Inspection

Search and replace or count characters in a string. Maps to COBOL's `INSPECT` statement.

```coba
inspect target replacing pattern by replacement
inspect target replacing all pattern by replacement
inspect target tallying counter for pattern
inspect target tallying counter for all pattern
```

Examples:
```coba
text(50) message = "Hello World"
int counter

// Replace first occurrence
inspect message replacing "o" by "0"
// message = "Hell0 World"

// Replace all occurrences
inspect message replacing all "o" by "0"
// message = "Hell0 W0rld"

// Count occurrences
inspect message tallying counter for all "o"
// counter = 2
```

**COBOL Mapping:**
```cobol
INSPECT GLOBAL-MESSAGE REPLACING FIRST "o" BY "0".
INSPECT GLOBAL-MESSAGE REPLACING ALL "o" BY "0".
INSPECT GLOBAL-MESSAGE TALLYING GLOBAL-COUNTER FOR ALL "o".
```

### Search Operations

#### Linear Search

Search an array sequentially for elements matching conditions. Maps to COBOL's `SEARCH` statement.

```coba
search array varying index
    at end:
        // Not found statements
    when condition1:
        // Found statements
    when condition2:
        // Found statements
end
```

Examples:
```coba
int[10] numbers
int idx
bool found = false

// Search for value
search numbers varying idx
    at end:
        print "Not found"
    when numbers[idx] == 42:
        print "Found at index: ", idx
        set found = true
end

// Multiple conditions
search numbers varying idx
    when numbers[idx] > 50:
        print "Found large value: ", numbers[idx]
    when numbers[idx] < 10:
        print "Found small value: ", numbers[idx]
end
```

**COBOL Mapping:**
```cobol
SEARCH GLOBAL-NUMBERS VARYING GLOBAL-IDX
    AT END
        DISPLAY "Not found"
    WHEN GLOBAL-NUMBERS(GLOBAL-IDX) = 42
        DISPLAY "Found at index: " GLOBAL-IDX
        MOVE 1 TO GLOBAL-FOUND
END-SEARCH.
```

**Notes:**
- `varying index` is optional
- `at end` clause executes if no match found
- Multiple `when` clauses supported
- Index is 0-based in Coba, 1-based in COBOL

#### Binary Search

Search a sorted array using binary search. Maps to COBOL's `SEARCH ALL` statement.

```coba
searchall array varying index
    at end:
        // Not found statements
    when condition:
        // Found statements
end
```

Examples:
```coba
int[100] sortedNumbers
int idx

// Binary search requires array to be sorted
searchall sortedNumbers varying idx
    at end:
        print "Not found"
    when sortedNumbers[idx] == 42:
        print "Found at index: ", idx
end
```

**COBOL Mapping:**
```cobol
SEARCH ALL GLOBAL-SORTEDNUMBERS VARYING GLOBAL-IDX
    AT END
        DISPLAY "Not found"
    WHEN GLOBAL-SORTEDNUMBERS(GLOBAL-IDX) = 42
        DISPLAY "Found at index: " GLOBAL-IDX
END-SEARCH.
```

**Notes:**
- Array must be sorted
- Only one `when` condition allowed
- Much faster than linear search for large arrays

### Set Index Operations

Manipulate array index variables. Maps to COBOL's `SET` statement.

```coba
set index to value
set index up by increment
set index down by decrement
```

Examples:
```coba
int[10] numbers
int idx

// Set index to specific value
set idx to 1

// Increment index
set idx up by 1

// Decrement index
set idx down by 2
```

**COBOL Mapping:**
```cobol
SET GLOBAL-IDX TO 1.
SET GLOBAL-IDX UP BY 1.
SET GLOBAL-IDX DOWN BY 2.
```

---

## 9. File I/O Operations

### File Declaration

Declare files for sequential access. Maps to COBOL's FILE-CONTROL and FD sections.

```coba
file filename sequential record recordType
file filename sequential record recordType status statusVariable
```

Examples:
```coba
// File with text records
file customerFile sequential record text(100)

// File with structured records
file transactionFile sequential record text(80) status fileStatus

// Status variable tracks file operations (00 = success, 10 = end of file, etc.)
text(2) fileStatus
```

**COBOL Mapping:**
```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT CUSTOMERFILE ASSIGN TO "customerFile"
        ORGANIZATION IS SEQUENTIAL.

    SELECT TRANSACTIONFILE ASSIGN TO "transactionFile"
        ORGANIZATION IS SEQUENTIAL
        FILE STATUS IS FILESTATUS.

DATA DIVISION.
FILE SECTION.
FD CUSTOMERFILE.
01 CUSTOMERFILE-RECORD PIC X(100).

FD TRANSACTIONFILE.
01 TRANSACTIONFILE-RECORD PIC X(80).

WORKING-STORAGE SECTION.
01 FILESTATUS PIC XX.
```

### Open File

Open a file for reading, writing, or appending. Maps to COBOL's `OPEN` statement.

```coba
open file input      // Open for reading
open file output     // Open for writing (creates/overwrites)
open file extend     // Open for appending
```

Examples:
```coba
file dataFile sequential record text(80)
text(80) record

// Open for reading
open dataFile input

// Process file...

close dataFile
```

**COBOL Mapping:**
```cobol
OPEN INPUT DATAFILE.
OPEN OUTPUT DATAFILE.
OPEN EXTEND DATAFILE.
```

### Close File

Close an open file. Maps to COBOL's `CLOSE` statement.

```coba
close filename
```

Example:
```coba
open dataFile input
// ... read data ...
close dataFile
```

**COBOL Mapping:**
```cobol
CLOSE DATAFILE.
```

### Read File

Read a record from a file. Maps to COBOL's `READ` statement.

```coba
read file into record
read file into record at end:
    // End of file handling
end
```

Examples:
```coba
file inputFile sequential record text(80) status fileStatus
text(80) line
text(2) fileStatus
bool endOfFile = false

open inputFile input

// Simple read
read inputFile into line

// Read with end-of-file handling
read inputFile into line at end:
    set endOfFile = true
    print "End of file reached"
end

close inputFile
```

**COBOL Mapping:**
```cobol
READ INPUTFILE INTO LINE.

READ INPUTFILE INTO LINE
    AT END
        MOVE 1 TO ENDOFFILE
        DISPLAY "End of file reached"
END-READ.
```

### Write File

Write a record to a file. Maps to COBOL's `WRITE` statement.

```coba
write record to file
```

Examples:
```coba
file outputFile sequential record text(80)
text(80) record

set record = "Hello, World!"

open outputFile output
write record to outputFile
close outputFile
```

**COBOL Mapping:**
```cobol
WRITE OUTPUTFILE-RECORD FROM RECORD.
```

### Complete File Processing Example

```coba
// File declarations
file inputFile sequential record text(80) status inStatus
file outputFile sequential record text(80) status outStatus

// Variables
text(80) inputRecord
text(80) outputRecord
text(2) inStatus
text(2) outStatus
bool endOfFile = false

function processFiles() {
    // Open files
    open inputFile input
    open outputFile output

    // Read and process until end of file
    while !endOfFile {
        read inputFile into inputRecord at end:
            set endOfFile = true
        end

        if !endOfFile {
            // Process record
            set outputRecord = inputRecord
            write outputRecord to outputFile
        }
    }

    // Close files
    close inputFile
    close outputFile
}
```

**COBOL Mapping:**
```cobol
PROCESSFILES.
    OPEN INPUT INPUTFILE.
    OPEN OUTPUT OUTPUTFILE.

    PERFORM UNTIL ENDOFFILE = 1
        READ INPUTFILE INTO INPUTRECORD
            AT END
                MOVE 1 TO ENDOFFILE
        END-READ

        IF ENDOFFILE NOT = 1
            MOVE INPUTRECORD TO OUTPUTRECORD
            WRITE OUTPUTFILE-RECORD FROM OUTPUTRECORD
        END-IF
    END-PERFORM.

    CLOSE INPUTFILE.
    CLOSE OUTPUTFILE.
```

**Notes:**
- File status codes: `"00"` = success, `"10"` = end of file, `"30"` = permanent error, `"90"` = file not found
- Files must be closed when done to ensure data is written
- Files opened as OUTPUT will overwrite existing files
- Files opened as EXTEND will append to existing files
- Only sequential file organization is currently supported

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
