#[derive(Debug)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String(String),
    Int(i64),
    Float(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // EOF
    Eof,
}

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    lexeme: String,
    line: u32,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: u32) -> Self {
        Self {
            kind,
            lexeme,
            line,
        }
    }
}
