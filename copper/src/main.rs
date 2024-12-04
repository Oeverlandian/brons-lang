use logos::Logos;
use std::fs;

mod lexer;

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
            Err(()) => {
                eprintln!("Error!")
            }
        }
    }
}
