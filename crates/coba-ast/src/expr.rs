/// Expression AST nodes for Coba

use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub line: usize,
    pub column: usize,
}

impl Expr {
    pub fn new(kind: ExprKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// Numeric literal: 123, 45.67
    Number(f64),

    /// String literal: "Hello"
    String(String),

    /// Boolean literal: true, false
    Boolean(bool),

    /// Variable reference: customerName
    Variable(String),

    /// Binary operation: a + b
    Binary {
        left: Box<Expr>,
        operator: BinaryOp,
        right: Box<Expr>,
    },

    /// Unary operation: -a, !b
    Unary {
        operator: UnaryOp,
        operand: Box<Expr>,
    },

    /// Procedure call expression (future use for functions)
    Call {
        name: String,
        arguments: Vec<Expr>,
    },

    /// Array indexing: arr[0]
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,

    // Comparison
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // Logical
    And,
    Or,
}

impl BinaryOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "+" => Some(BinaryOp::Add),
            "-" => Some(BinaryOp::Subtract),
            "*" => Some(BinaryOp::Multiply),
            "**" => Some(BinaryOp::Power),
            "/" => Some(BinaryOp::Divide),
            "%" => Some(BinaryOp::Modulo),
            "==" => Some(BinaryOp::Equal),
            "!=" => Some(BinaryOp::NotEqual),
            "<" => Some(BinaryOp::Less),
            "<=" => Some(BinaryOp::LessEqual),
            ">" => Some(BinaryOp::Greater),
            ">=" => Some(BinaryOp::GreaterEqual),
            "&&" => Some(BinaryOp::And),
            "||" => Some(BinaryOp::Or),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Subtract => "-",
            BinaryOp::Multiply => "*",
            BinaryOp::Power => "**",
            BinaryOp::Divide => "/",
            BinaryOp::Modulo => "%",
            BinaryOp::Equal => "==",
            BinaryOp::NotEqual => "!=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl UnaryOp {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "-" => Some(UnaryOp::Negate),
            "!" => Some(UnaryOp::Not),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &str {
        match self {
            UnaryOp::Negate => "-",
            UnaryOp::Not => "!",
        }
    }
}
