use std::collections::HashMap;
use crate::tokenizer::Keyword;

pub struct Lexer;
pub type KeywordMap = HashMap<&'static str, Keyword>;


impl Lexer {

    pub fn keyword_map() -> KeywordMap {
        let mut map = HashMap::new();
        map.insert("int", Keyword::Int);
        map.insert("return", Keyword::Return);
        map
    }

}