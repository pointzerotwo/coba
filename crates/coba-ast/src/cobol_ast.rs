/// COBOL AST nodes for decompilation
///
/// These structures represent parsed COBOL programs,
/// which will be transformed into Coba AST for decompilation.

use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct CobolProgram {
    pub program_id: String,
    pub data_division: Option<DataDivision>,
    pub procedure_division: Option<ProcedureDivision>,
}

impl CobolProgram {
    pub fn new(program_id: String) -> Self {
        Self {
            program_id,
            data_division: None,
            procedure_division: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataDivision {
    pub working_storage: Vec<DataItem>,
    pub local_storage: Vec<DataItem>,
    pub linkage_section: Vec<DataItem>,
}

impl DataDivision {
    pub fn new() -> Self {
        Self {
            working_storage: Vec::new(),
            local_storage: Vec::new(),
            linkage_section: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataItem {
    pub level: u8,
    pub name: String,
    pub picture: Option<String>,
    pub value: Option<String>,
    pub redefines: Option<String>,
    pub occurs: Option<u32>,
    pub children: Vec<DataItem>,
}

impl DataItem {
    pub fn new(level: u8, name: String) -> Self {
        Self {
            level,
            name,
            picture: None,
            value: None,
            redefines: None,
            occurs: None,
            children: Vec::new(),
        }
    }

    /// Infer Coba type from PICTURE clause
    pub fn infer_type(&self) -> Option<Type> {
        if let Some(pic) = &self.picture {
            Type::from_cobol_picture(pic)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDivision {
    pub paragraphs: Vec<Paragraph>,
}

impl ProcedureDivision {
    pub fn new() -> Self {
        Self {
            paragraphs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Paragraph {
    pub name: String,
    pub statements: Vec<CobolStatement>,
}

impl Paragraph {
    pub fn new(name: String) -> Self {
        Self {
            name,
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CobolStatement {
    /// MOVE source TO dest
    Move {
        source: CobolExpr,
        dest: String,
    },

    /// ADD x TO y
    Add {
        operands: Vec<CobolExpr>,
        to: String,
    },

    /// SUBTRACT x FROM y
    Subtract {
        operands: Vec<CobolExpr>,
        from: String,
    },

    /// MULTIPLY x BY y
    Multiply {
        operand: CobolExpr,
        by: String,
    },

    /// DIVIDE x BY y GIVING z
    Divide {
        dividend: CobolExpr,
        divisor: CobolExpr,
        giving: Option<String>,
    },

    /// COMPUTE x = expression
    Compute {
        target: String,
        expression: CobolExpr,
    },

    /// IF condition ... END-IF
    If {
        condition: CobolCondition,
        then_stmts: Vec<CobolStatement>,
        else_stmts: Option<Vec<CobolStatement>>,
    },

    /// PERFORM paragraph-name
    Perform {
        target: PerformTarget,
    },

    /// GO TO paragraph-name
    GoTo {
        target: String,
    },

    /// DISPLAY literal/variable
    Display {
        items: Vec<CobolExpr>,
    },

    /// ACCEPT variable
    Accept {
        variable: String,
    },

    /// CALL program-name
    Call {
        program: String,
        using: Vec<String>,
    },

    /// STOP RUN
    StopRun,

    /// EXIT
    Exit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PerformTarget {
    /// PERFORM paragraph-name
    Paragraph(String),

    /// PERFORM UNTIL condition
    Until {
        condition: CobolCondition,
        statements: Vec<CobolStatement>,
    },

    /// PERFORM VARYING ... UNTIL
    Varying {
        variable: String,
        from: CobolExpr,
        by: CobolExpr,
        until: CobolCondition,
        statements: Vec<CobolStatement>,
    },

    /// PERFORM n TIMES
    Times {
        count: CobolExpr,
        statements: Vec<CobolStatement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum CobolExpr {
    /// Numeric literal
    Number(f64),

    /// String literal
    String(String),

    /// Variable reference
    Variable(String),

    /// Binary operation
    Binary {
        left: Box<CobolExpr>,
        op: CobolOp,
        right: Box<CobolExpr>,
    },

    /// Unary minus
    Negate(Box<CobolExpr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CobolOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CobolCondition {
    /// x = y
    Equal(CobolExpr, CobolExpr),

    /// x > y
    Greater(CobolExpr, CobolExpr),

    /// x < y
    Less(CobolExpr, CobolExpr),

    /// x >= y
    GreaterEqual(CobolExpr, CobolExpr),

    /// x <= y
    LessEqual(CobolExpr, CobolExpr),

    /// x <> y
    NotEqual(CobolExpr, CobolExpr),

    /// AND condition
    And(Box<CobolCondition>, Box<CobolCondition>),

    /// OR condition
    Or(Box<CobolCondition>, Box<CobolCondition>),

    /// NOT condition
    Not(Box<CobolCondition>),
}
