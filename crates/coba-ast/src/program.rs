/// Program structure for Coba AST

use crate::stmt::Stmt;
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

impl Program {
    pub fn new(declarations: Vec<Declaration>) -> Self {
        Self { declarations }
    }

    pub fn empty() -> Self {
        Self {
            declarations: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Global variable declaration
    Variable {
        name: String,
        type_: Type,
        initializer: Option<crate::expr::Expr>,
    },

    /// Procedure declaration
    Procedure {
        name: String,
        parameters: Vec<Parameter>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub type_: Type,
}

impl Parameter {
    pub fn new(name: String, type_: Type) -> Self {
        Self { name, type_ }
    }
}
