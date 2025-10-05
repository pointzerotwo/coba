/// Token types for COBOL lexer

#[derive(Debug, Clone, PartialEq)]
pub struct CobolToken {
    pub kind: CobolTokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl CobolToken {
    pub fn new(kind: CobolTokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Self {
            kind,
            lexeme,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CobolTokenKind {
    // Division headers
    IdentificationDivision,
    EnvironmentDivision,
    DataDivision,
    ProcedureDivision,

    // Section headers
    WorkingStorage,
    LocalStorage,
    LinkageSection,
    FileSection,

    // Data definition
    Level(u8),      // 01, 02, 77, 88, etc.
    Pic,            // PIC or PICTURE
    Value,
    Redefines,
    Occurs,

    // Statements
    Move,
    Add,
    Subtract,
    Multiply,
    Divide,
    Compute,
    If,
    Else,
    EndIf,
    Perform,
    GoTo,
    Stop,
    Accept,
    Display,
    Call,
    Exit,

    // Clauses
    To,
    From,
    By,
    Giving,
    Rounded,
    On,
    Size,
    Varying,
    Until,
    Times,

    // Literals
    Number,
    String,

    // Identifiers
    Identifier,
    ParagraphName,

    // Special
    Period,         // .
    Comma,          // ,
    LeftParen,      // (
    RightParen,     // )

    // Operators
    Equal,          // =
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Greater,        // >
    Less,           // <
    GreaterEqual,   // >=
    LessEqual,      // <=
    NotEqual,       // <>

    // Special
    Comment,
    Eof,
    Error(String),
}

impl CobolTokenKind {
    /// Check if this is a COBOL keyword
    pub fn from_keyword(word: &str) -> Option<Self> {
        let upper = word.to_uppercase();
        match upper.as_str() {
            // Divisions
            "IDENTIFICATION" => Some(CobolTokenKind::IdentificationDivision),
            "ENVIRONMENT" => Some(CobolTokenKind::EnvironmentDivision),
            "DATA" => Some(CobolTokenKind::DataDivision),
            "PROCEDURE" => Some(CobolTokenKind::ProcedureDivision),

            // Sections
            "WORKING-STORAGE" => Some(CobolTokenKind::WorkingStorage),
            "LOCAL-STORAGE" => Some(CobolTokenKind::LocalStorage),
            "LINKAGE" => Some(CobolTokenKind::LinkageSection),
            "FILE" => Some(CobolTokenKind::FileSection),
            "FILE-SECTION" => Some(CobolTokenKind::FileSection),

            // Keywords
            "PIC" | "PICTURE" => Some(CobolTokenKind::Pic),
            "VALUE" => Some(CobolTokenKind::Value),
            "REDEFINES" => Some(CobolTokenKind::Redefines),
            "OCCURS" => Some(CobolTokenKind::Occurs),

            // Statements
            "MOVE" => Some(CobolTokenKind::Move),
            "ADD" => Some(CobolTokenKind::Add),
            "SUBTRACT" => Some(CobolTokenKind::Subtract),
            "MULTIPLY" => Some(CobolTokenKind::Multiply),
            "DIVIDE" => Some(CobolTokenKind::Divide),
            "COMPUTE" => Some(CobolTokenKind::Compute),
            "IF" => Some(CobolTokenKind::If),
            "ELSE" => Some(CobolTokenKind::Else),
            "END-IF" => Some(CobolTokenKind::EndIf),
            "PERFORM" => Some(CobolTokenKind::Perform),
            "GO" => Some(CobolTokenKind::GoTo),
            "GOTO" => Some(CobolTokenKind::GoTo),
            "STOP" => Some(CobolTokenKind::Stop),
            "ACCEPT" => Some(CobolTokenKind::Accept),
            "DISPLAY" => Some(CobolTokenKind::Display),
            "CALL" => Some(CobolTokenKind::Call),
            "EXIT" => Some(CobolTokenKind::Exit),

            // Clauses
            "TO" => Some(CobolTokenKind::To),
            "FROM" => Some(CobolTokenKind::From),
            "BY" => Some(CobolTokenKind::By),
            "GIVING" => Some(CobolTokenKind::Giving),
            "ROUNDED" => Some(CobolTokenKind::Rounded),
            "ON" => Some(CobolTokenKind::On),
            "SIZE" => Some(CobolTokenKind::Size),
            "VARYING" => Some(CobolTokenKind::Varying),
            "UNTIL" => Some(CobolTokenKind::Until),
            "TIMES" => Some(CobolTokenKind::Times),

            _ => None,
        }
    }

    /// Try to parse as a level number
    pub fn try_level(word: &str) -> Option<Self> {
        if let Ok(level) = word.parse::<u8>() {
            if (1..=49).contains(&level) || level == 66 || level == 77 || level == 88 {
                return Some(CobolTokenKind::Level(level));
            }
        }
        None
    }
}
