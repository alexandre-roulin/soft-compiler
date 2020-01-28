mod tokenizer;
mod lexer;
mod data;
mod parser;

use std::env::args;
use std::rc::Rc;
use parser::Parse;
use crate::assembly::Generator;
use std::fs::File;
use std::io::Write;

mod assembly;

fn main()  {
    let argc: Vec<String> = args().collect();
    if argc.len() > 1 {
        for index in 1..argc.len() {
            let mut tokenizer = tokenizer::Tokenizer::new(Rc::new(argc[index].clone()));
            println!("Tokenizer");
            tokenizer.tokenize(&lexer::Lexer::keyword_map());
            let mut parser = Parse::new(&tokenizer.tokens);
            println!("Parser : {:?}", tokenizer.tokens);
            match parser.parse() {
                Ok(program) => {
                    let generator = Generator::new();
                    println!("Generator : {:?}", program);
                    let asm_generate = generator.generate(&program);
                    let asm_file = argc[index].replace(".c", ".s");
                    let mut f = File::create(asm_file).expect("error create file");
                    f.write(asm_generate.as_bytes()).expect("error write file");
                },
                Err(error) => {
                    let _array: Vec<String> = tokenizer.file.split('\n').map(|s| s.to_string()).collect();

                    println!("[ERROR] in `{}` :\t{} : L:{}:{}", argc[index], error.error, error.line, error.position);
                },
            };

        }
    }
}
