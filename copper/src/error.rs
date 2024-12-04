use std::fmt;

#[derive(Debug, PartialEq, Clone, Default)]
pub struct LexerError {
    pub line: usize,
    pub column: usize,
    pub kind: LexerErrorKind,
}

// TODO: Actually implement errors
#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexerErrorKind {
    InvalidCharacter(char),
    UnterminatedString,
    UnterminatedCharLiteral,
    EmptyCharLiteral,
    InvalidEscapeSequence(String),
    UnexpectedEOF,
    #[default]
    UnknownError,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Lexer error at line {}, column {}: {}",
            self.line,
            self.column,
            match &self.kind {
                LexerErrorKind::InvalidCharacter(c) => format!("Invalid character '{}'", c),
                LexerErrorKind::UnterminatedString => "Unterminated string literal".to_string(),
                LexerErrorKind::UnterminatedCharLiteral => "Unterminated character literal".to_string(),
                LexerErrorKind::EmptyCharLiteral => "Empty character literal".to_string(),
                LexerErrorKind::InvalidEscapeSequence(seq) => format!("Invalid escape sequence '{}'", seq),
                LexerErrorKind::UnexpectedEOF => "Unexpected end of file".to_string(),
                LexerErrorKind::UnknownError => "Unknown Error".to_string(),
            }
        )
    }
}

impl std::error::Error for LexerError {}