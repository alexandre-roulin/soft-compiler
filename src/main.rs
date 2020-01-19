//use crate::lexer::Lexer;

use std::env::args;
use std::rc::Rc;
use crate::parser::Parser;
use bnf::Grammar;

mod tokenizer;
mod lexer;
mod parser;



fn main() {

    let argc: Vec<String> = args().collect();
    if argc.len() > 1 {
        for i in 1..argc.len() {
            let mut tokenizer = tokenizer::Tokenizer::new(Rc::new(argc[i].clone()));
            tokenizer.tokenize(&lexer::Lexer::keyword_map());
            println!("Token recognize : {:?}", tokenizer.tokens);
            let mut parser = Parser::new(tokenizer.tokens.iter()).expect("Error bnf");
            println!("File is well formatted [{}]", parser.parse());
        }
    }

}
