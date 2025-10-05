/// Token types for the Coba language lexer

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Self {
            kind,
            lexeme,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Literals
    Number,
    String,
    True,
    False,

    // Keywords
    Dec,
    Text,
    Int,
    Bool,
    Function,
    If,
    Elif,
    Else,
    While,
    For,
    To,
    Step,
    Return,
    Set,
    Call,
    Print,
    Evaluate,
    When,
    Other,
    Accept,
    From,

    // Identifiers
    Identifier,

    // Operators
    Plus,
    Minus,
    Star,
    StarStar,
    Slash,
    Percent,

    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    AmpersandAmpersand,
    PipePipe,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    DotDot,

    // Special
    Eof,
    Error(String),
}

impl TokenKind {
    /// Check if this token is a keyword
    pub fn from_keyword(word: &str) -> Option<Self> {
        match word {
            "dec" => Some(TokenKind::Dec),
            "text" => Some(TokenKind::Text),
            "int" => Some(TokenKind::Int),
            "bool" => Some(TokenKind::Bool),
            "function" => Some(TokenKind::Function),
            "if" => Some(TokenKind::If),
            "elif" => Some(TokenKind::Elif),
            "else" => Some(TokenKind::Else),
            "while" => Some(TokenKind::While),
            "for" => Some(TokenKind::For),
            "to" => Some(TokenKind::To),
            "step" => Some(TokenKind::Step),
            "return" => Some(TokenKind::Return),
            "set" => Some(TokenKind::Set),
            "call" => Some(TokenKind::Call),
            "print" => Some(TokenKind::Print),
            "evaluate" => Some(TokenKind::Evaluate),
            "when" => Some(TokenKind::When),
            "other" => Some(TokenKind::Other),
            "accept" => Some(TokenKind::Accept),
            "from" => Some(TokenKind::From),
            "true" => Some(TokenKind::True),
            "false" => Some(TokenKind::False),
            _ => None,
        }
    }

    /// Check if this token kind is a literal
    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenKind::Number | TokenKind::String | TokenKind::True | TokenKind::False
        )
    }

    /// Check if this token kind is an operator
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::StarStar
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Equal
                | TokenKind::EqualEqual
                | TokenKind::Bang
                | TokenKind::BangEqual
                | TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::AmpersandAmpersand
                | TokenKind::PipePipe
        )
    }
}
