use logos::Logos;
use std::fs;

mod lexer;
mod parser;

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
    let mut tokens = Vec::new();
    for token in lexer {
        println!("{:?}", token.clone().unwrap());
        tokens.push(token.expect("Lexer error"));
    }

    let mut parser = parser::Parser::new(tokens);
    match parser.parse() {
        Ok(statements) => {
            for stmt in statements {
                println!("{:#?}", stmt)
            }
        },
        Err(err) => {
            println!("Error: {:?}", err)
        }
    }
}
