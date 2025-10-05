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
