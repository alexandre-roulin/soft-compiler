mod tokenizer;
mod data;
mod parser;

use std::env::args;
use std::rc::Rc;
use parser::Parse;
use crate::assembly::Generator;
use std::fs::File;
use std::io::Write;
use std::process::Command;

mod assembly;

fn main()  {
    let argc: Vec<String> = args().collect();
    if argc.len() > 1 {
        for index in 1..argc.len() {
//            println!("File{}", argc[index]);

            let mut tokenizer = tokenizer::Tokenizer::new(Rc::new(argc[index].clone()));
//            println!("Tokenizer");
            tokenizer.tokenize();
            println!("Parser : {:?}", tokenizer.tokens);
            let mut parser = Parse::new(tokenizer.tokens);
            match parser.parse() {
                Ok(program) => {
                    let mut generator = Generator::new();
                    println!("Generator : {:#?}", program);
                    let asm_generate = generator.generate(&program);
                    let asm_file = argc[index].replace(".c", ".s");
                    let mut f = File::create(&asm_file).expect("error create file");
                    f.write(asm_generate.as_bytes()).expect("error write file");
                    let exec_file = argc[index].replace(".c", "");

                    let c = Command::new("gcc")
                        .arg("-g")
                        .arg(asm_file)
                        .arg("-o")
                        .arg(&exec_file)
                        .output();
                    let b= c.unwrap().stderr;
                    if b.is_empty() {
                        println!("[{}] -> ", Command::new(exec_file).output().unwrap().status)
                    } else {

                        println!("Error -> {}", String::from_utf8(b).unwrap());
                    }

                },
                Err(error) => {
                    let _array: Vec<String> = tokenizer.file.split('\n').map(|s| s.to_string()).collect();

                    println!("[ERROR] in `{}` :\t{} : L:{}:{}", argc[index], error.error, error.line, error.position);
                },
            };

        }
    }
}
