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
    Initialize,
    Continue,
    Exit,
    StringVerb,
    Unstring,
    Inspect,
    Delimited,
    By,
    Into,
    Replacing,
    All,
    Tallying,
    With,
    Pointer,
    Search,
    SearchAll,
    Varying,
    At,
    End,
    Indexed,

    // File I/O
    File,
    Open,
    Close,
    Read,
    Write,
    Input,
    Output,
    Extend,
    Record,
    Sequential,
    Organization,
    Access,
    Status,

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
            "initialize" => Some(TokenKind::Initialize),
            "continue" => Some(TokenKind::Continue),
            "exit" => Some(TokenKind::Exit),
            "string" => Some(TokenKind::StringVerb),
            "unstring" => Some(TokenKind::Unstring),
            "inspect" => Some(TokenKind::Inspect),
            "delimited" => Some(TokenKind::Delimited),
            "by" => Some(TokenKind::By),
            "into" => Some(TokenKind::Into),
            "replacing" => Some(TokenKind::Replacing),
            "all" => Some(TokenKind::All),
            "tallying" => Some(TokenKind::Tallying),
            "with" => Some(TokenKind::With),
            "pointer" => Some(TokenKind::Pointer),
            "search" => Some(TokenKind::Search),
            "searchall" => Some(TokenKind::SearchAll),
            "varying" => Some(TokenKind::Varying),
            "at" => Some(TokenKind::At),
            "end" => Some(TokenKind::End),
            "indexed" => Some(TokenKind::Indexed),
            "file" => Some(TokenKind::File),
            "open" => Some(TokenKind::Open),
            "close" => Some(TokenKind::Close),
            "read" => Some(TokenKind::Read),
            "write" => Some(TokenKind::Write),
            "input" => Some(TokenKind::Input),
            "output" => Some(TokenKind::Output),
            "extend" => Some(TokenKind::Extend),
            "record" => Some(TokenKind::Record),
            "sequential" => Some(TokenKind::Sequential),
            "organization" => Some(TokenKind::Organization),
            "access" => Some(TokenKind::Access),
            "status" => Some(TokenKind::Status),
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
