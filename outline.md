We are building a new programming language called coba. It is a modern language based on JavaScript/TypeScript and other modern languages. The purpose of this language is to provide a modern way to write COBOL.
We will also be building a compiler/decompiler that converts coba to and from COBOL

Some key solutions: 
Recursion Detection - Build call graph, use DFS to catch cycles
GO TO Elimination - Control flow analysis to convert spaghetti code
Column Rules - Structured line parser that handles COBOL's positional syntax
Variable Scoping - Name mangling scheme (LOCAL-PROC-VAR, PARAM-PROC-0)
Type Inference - Pattern match PICTURE clauses to reverse-engineer types

We are going to write the compiler in rust

Known Edge Cases to Handle:

Numeric Precision
Problem: decimal(9,2) can hold max value 999999999.99
Requirement:

Overflow should be COBOL runtime error, not compiler error
Document maximum values in language spec

Text Truncation
Problem: text(10) assigned "Hello, World!" (13 chars)
Requirement:

Truncate to 10 characters (COBOL behavior)
No compile-time error
Consider warning flag

Division by Zero
Problem: set x = a / 0.00
Requirement:

Compile successfully
COBOL runtime will error
Document this behavior

Empty Procedures
Problem:
cobaprocedure empty()
end
Requirement:

Valid coba
Generate empty COBOL paragraph

Unusual Names
Problem:
cobadecimal(5,2) PERFORM = 10.00  // PERFORM is COBOL keyword
Requirement:

Escape/mangle COBOL reserved words
Document reserved word list

Deep Nesting
Problem:
rebelif a then
  if b then
    if c then
      if d then
        // 10 levels deep
Requirement:

Support arbitrary nesting
Test at least 20 levels deep


