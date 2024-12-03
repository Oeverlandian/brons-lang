use logos::Logos;
use crate::error::LexerError;

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexerError)]
pub enum TokenKind {

    // Skips
    #[regex(r"\s+", logos::skip)]
    Whitespace,
    #[regex(r"//[^\n]*", logos::skip)]
    SingleLineComment,
    #[regex(r"/\*[^*]*\*+(?:[^*/][^*]*\*+)*/", logos::skip)]
    MultiLineComment,

    // Keywords
    #[token("var")]
    Var,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("func")]
    Func,
    #[token("struct")] 
    Struct, 
    #[token("if")]
    If, 
    #[token("else")]
    Else, 
    #[token("while")]
    While, 
    #[token("for")]
    For,
    #[token("break")]
    Break,
    #[token("Return")]
    Return, 
    #[token("import")]
    Import, 
    #[token("as")]
    As, 
    #[token("()")]
    Void, 
    #[token("int")]
    Int, 
    #[token("float")]
    Float, 
    #[token("char")]
    Char,
    #[token("str")]
    Str,
    #[token("bool")]
    Bool,

    // Identifier
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),
   
    // Literals 
    #[regex(r"\d+", |lex| lex.slice().parse().ok())]
    IntLiteral(i32),
    #[regex(r"\d+\.\d+", |lex| lex.slice().parse().ok())]
    FloatLiteral(f64),
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    BoolLiteral(bool),
    #[regex(r"'[^']'", |lex| lex.slice().chars().nth(1).unwrap())]
    CharLiteral(char),
    #[regex(r#""[^"]*""#, |lex| lex.slice()[1..lex.slice().len()-1].to_string())]
    StrLiteral(String),

    // Arithmetic Operators
    #[token("+")]
    Plus,               // +
    #[token("-")]
    Minus,              // -
    #[token("*")]
    Star,               // * - Multiplication
    #[token("/")]
    Slash,              // / - Division
    #[token("%")]
    Procent,            // % - Modulo

    // Comparison Operators
    #[token("==")]
    DoubleEquals,       // ==
    #[token("!=")]
    NotEquals,          // !=
    #[token(">")]
    GreaterThan,        // >
    #[token("<")]
    LessThan,           // <
    #[token(">=")]
    GreaterThanEquals,  // >=
    #[token("<=")]
    LessThanEquals,     // <=

    // Logical Operators
    #[token("&&")]
    AndAnd,             // &&
    #[token("||")]
    OrOr,               // ||
    #[token("!")]
    Not,                // !

    // Assignment Operators
    #[token("=")]
    Equals,             // =
    #[token("+=")]
    PlusEquals,         // +=

    // Bitwise Operators
    #[token("&")]
    And,                // &
    #[token("|")]
    Or,                 // |
    #[token("^")]
    Xor,                // ^
    #[token("~")]
    BwNot,              // ~7
    #[token("<<")]
    LeftShift,          // <<
    #[token(">>")]
    RightShift,         // >>

    // Punctuation
    #[token("(")]
    LeftParenthesis,    // (
    #[token(")")]
    RightParanthesis,   // )
    #[token("{")]
    LeftBrace,          // {
    #[token("}")]
    RightBrace,         // }
    #[token("[")]
    LeftBracket,        // [
    #[token("]")]
    RightBracket,       // ]
    #[token(";")]
    Semicolon,          // ;
    #[token(",")]
    Comma,              // ,
    #[token(".")]
    Dot,                // .
}
