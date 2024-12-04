use error::{LexerError, LexerErrorKind};
use logos::Logos;
use std::fs;

mod lexer;
mod error;

fn main() {

    let source_file = "source_code.brz";
    let source_code = match fs::read_to_string(source_file) {
        Ok(content) => content,
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    let lexer = lexer::TokenKind::lexer(&source_code);
    for token in lexer {
        match token {
            Ok(token) => {
                println!("{:?}", token);
            }
            Err(err) => {
                println!("Lexer error: {}", err);
                match err {
                    LexerError { kind: LexerErrorKind::InvalidCharacter(c), .. } => {
                        println!("Invalid character: {}", c);
                    }
                    LexerError { kind: LexerErrorKind::UnterminatedString, .. } => {
                        println!("Unterminated string");
                    }
                    LexerError { kind: LexerErrorKind::UnterminatedCharLiteral, .. } => {
                        println!("Unterminated char literal");
                    }
                    LexerError { kind: LexerErrorKind::EmptyCharLiteral, .. } => {
                        println!("Empty char literal");
                    }
                    LexerError { kind: LexerErrorKind::InvalidEscapeSequence(s), .. } => {
                        println!("Invalid escape sequence: {}", s);
                    }
                    LexerError { kind: LexerErrorKind::UnexpectedEOF, .. } => {
                        println!("Unexpected End Of File");
                    } 
                    _ => {
                        println!("Unknown lexer error");
                    }
                }
            }
        }
    }
}
