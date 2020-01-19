use std::collections::HashMap;
use crate::tokenizer::Keyword;
use crate::tokenizer::Keyword::Data;
use crate::tokenizer::Type::Int;

pub struct Lexer;
pub type KeywordMap = HashMap<&'static str, Keyword>;


impl Lexer {

    pub fn keyword_map() -> KeywordMap {
        let mut map = HashMap::new();
        map.insert("int", Keyword::Data(Int));
        map.insert("return", Keyword::Return);
        map
    }

}