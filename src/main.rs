//use crate::lexer::Lexer;
use regex::bytes::Regex;

mod tokenizer;
mod lexer;
fn main() {
    let r = Regex::new("int").expect("");
    let b = String::from("int main() { return 2; }");
    let by = b.as_bytes();


}
