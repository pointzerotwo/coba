/// Statement AST nodes for Coba

use crate::expr::Expr;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub line: usize,
    pub column: usize,
}

impl Stmt {
    pub fn new(kind: StmtKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// Variable declaration: decimal(9,2) price = 0.00
    VarDecl {
        name: String,
        type_: Type,
        initializer: Option<Expr>,
    },

    /// Assignment: set x = 10, set arr[0] = 10
    Assign {
        target: Expr,
        value: Expr,
    },

    /// Procedure call: call calculateTotal(price, quantity)
    Call {
        name: String,
        arguments: Vec<Expr>,
    },

    /// If statement
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        elif_branches: Vec<(Expr, Vec<Stmt>)>,
        else_branch: Option<Vec<Stmt>>,
    },

    /// While loop
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },

    /// For loop
    For {
        variable: String,
        start: Expr,
        end: Expr,
        step: Expr,
        body: Vec<Stmt>,
    },

    /// Return statement (future use)
    Return {
        value: Option<Expr>,
    },

    /// Print statement: print value1, value2, ...
    Print {
        values: Vec<Expr>,
    },

    /// Evaluate statement (switch/case): evaluate expr { when value: ... }
    Evaluate {
        subject: Expr,
        when_branches: Vec<WhenBranch>,
        other_branch: Option<Vec<Stmt>>,
    },

    /// Accept statement: accept variable [from source]
    Accept {
        variable: String,
        source: AcceptSource,
    },

    /// Initialize statement: initialize variable
    Initialize {
        variables: Vec<String>,
    },

    /// Continue statement: no-op placeholder
    Continue,

    /// Exit statement: exit function/paragraph
    Exit,

    /// String concatenation: string src1, src2 delimited by X into dest
    StringConcat {
        sources: Vec<StringSource>,
        destination: String,
        pointer: Option<String>,
    },

    /// String split: unstring source delimited by X into dest1, dest2
    StringSplit {
        source: String,
        delimiter: Option<Expr>,
        destinations: Vec<String>,
        pointer: Option<String>,
        tallying: Option<String>,
    },

    /// String inspection: inspect string replacing/tallying
    StringInspect {
        target: String,
        operation: InspectOperation,
    },

    /// Search statement: search array when condition
    SearchLinear {
        array: String,
        index: Option<String>,
        at_end: Option<Vec<Stmt>>,
        when_clauses: Vec<(Expr, Vec<Stmt>)>,
    },

    /// Binary search: searchall array when condition
    SearchBinary {
        array: String,
        index: Option<String>,
        at_end: Option<Vec<Stmt>>,
        when_condition: Expr,
        when_body: Vec<Stmt>,
    },

    /// Set statement: set index to value / set index up by N
    SetIndex {
        index: String,
        operation: SetOperation,
    },

    /// Open file: open file input/output/extend
    OpenFile {
        file: String,
        mode: FileMode,
    },

    /// Close file: close file
    CloseFile {
        file: String,
    },

    /// Read from file: read file into record
    ReadFile {
        file: String,
        record: String,
        at_end: Option<Vec<Stmt>>,
    },

    /// Write to file: write record to file
    WriteFile {
        file: String,
        record: String,
    },

    /// Add statement: add a to b giving c
    Add {
        operands: Vec<Expr>,
        to: Option<Vec<String>>,
        giving: Option<String>,
        on_size_error: Option<Vec<Stmt>>,
    },

    /// Subtract statement: subtract a from b giving c
    Subtract {
        operands: Vec<Expr>,
        from: Expr,
        giving: Option<String>,
        on_size_error: Option<Vec<Stmt>>,
    },

    /// Multiply statement: multiply a by b giving c
    Multiply {
        operand1: Expr,
        operand2: Expr,
        giving: Option<String>,
        on_size_error: Option<Vec<Stmt>>,
    },

    /// Divide statement: divide a by b giving c remainder d
    Divide {
        dividend: Expr,
        divisor: Expr,
        giving: Option<String>,
        remainder: Option<String>,
        on_size_error: Option<Vec<Stmt>>,
    },

    /// Compute statement: compute result = expression
    Compute {
        target: String,
        expression: Expr,
        on_size_error: Option<Vec<Stmt>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum FileMode {
    Input,
    Output,
    Extend,
    // Future: InputOutput
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringSource {
    pub value: Expr,
    pub delimiter: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InspectOperation {
    Replacing { pattern: Expr, replacement: Expr },
    Tallying { counter: String, pattern: Expr },
    ReplacingAll { pattern: Expr, replacement: Expr },
    TallyingAll { counter: String, pattern: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub enum SetOperation {
    To(Expr),
    UpBy(Expr),
    DownBy(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhenBranch {
    pub condition: WhenCondition,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum WhenCondition {
    /// Single value: when 1
    Value(Expr),

    /// Range: when 10..20
    Range(Expr, Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AcceptSource {
    /// Accept from user input
    User,

    /// Accept from system date (YYMMDD)
    Date,

    /// Accept from system time
    Time,

    /// Accept from system day
    Day,

    /// Accept from day of week
    DayOfWeek,
}
