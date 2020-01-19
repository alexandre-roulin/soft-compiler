use std::rc::Rc;
use std::fs::File;
use std::io::Read;
use crate::lexer::KeywordMap;
use std::str::FromStr;
use std::fmt::{Display, Formatter, Error};


#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    Constant(String),
    OpenParentheses,
    CloseParentheses,
    OpenBrace,
    CloseBrace,
    Semicolon,
    Whitespace,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Keyword(Keyword::Data(Type::Int)) => String::from("int"),
            Token::Identifier(_) => { String::from("identifier") }
            Token::OpenParentheses => String::from("OpenParentheses"),
            Token::CloseParentheses => String::from("CloseParentheses"),
            Token::OpenBrace => String::from("OpenBrace"),
            Token::CloseBrace => String::from("CloseBrace"),
            Token::Semicolon => String::from("Semicolon"),
            Token::Keyword(Keyword::Return) => String::from("return"),
            Token::Constant(_) => String::from("Constant"),
            _ => String::from(";"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy, Hash, Eq)]
pub enum Keyword {
    Data(Type),
    Return,
}

#[derive(Debug, Clone, PartialEq, Copy, Hash, Eq)]
pub enum Type {
    Int,
}

#[derive(Debug)]
pub enum CharacterType {
    Whitespace,
    Alphabetic,
    Numeric,
    NewLine,
    NonAlphabetic(char),
}

#[warn(dead_code)]
#[derive(Debug)]
pub struct Tokenizer {
    ptr: Rc<Vec<char>>,
    position: usize,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(filename: Rc<String>) -> Self {
        Tokenizer {
            ptr: Self::read_file(filename),
            position: 0,
            tokens: vec![],
        }
    }

    fn read_file(filename: Rc<String>) -> Rc<Vec<char>> {
        let mut s = String::new();
        let mut file = File::open(filename.as_str()).expect("File not found");
        file.read_to_string(&mut s).expect("Error reading file");
//        println!("File:\n`{}`\n", s);
        Rc::new(s.chars().collect())
    }

    fn get_char_type(&self, advance_from_pos: usize) -> Option<CharacterType> {
        self.ptr.get(self.position + advance_from_pos).map(|c| {
            if c.is_alphabetic() || c == &'_' {
                CharacterType::Alphabetic
            } else if c.is_ascii_digit() {
                CharacterType::Numeric
            } else if c == &' ' {
                CharacterType::Whitespace
            } else if c == &'\n' {
                CharacterType::NewLine
            } else {
                CharacterType::NonAlphabetic(*c)
            }
        })
    }

    pub fn tokenize(&mut self, keyword_map: &KeywordMap) {
        while let Some(ch) = self.get_char_type(0) {
            match ch {
                CharacterType::Whitespace => self.position += 1,
                CharacterType::Alphabetic => self.get_identifier(keyword_map),
                CharacterType::Numeric => self.get_constant(),
                CharacterType::NewLine => self.position += 1,
                CharacterType::NonAlphabetic(c) => {
                    self.position += 1;
                    match c {
                        '(' => { self.tokens.push(Token::OpenParentheses) }
                        ')' => { self.tokens.push(Token::CloseParentheses) }
                        '{' => { self.tokens.push(Token::OpenBrace) }
                        '}' => { self.tokens.push(Token::CloseBrace) }
                        ';' => { self.tokens.push(Token::Semicolon) }
                        _ => {}
                    }
                }
            }
        }
    }

    fn get_identifier(&mut self, keyword_map: &KeywordMap) {
        let mut len = 1;
        while let Some(c) = self.ptr.get(self.position + len) {
            if c.is_alphabetic() || c.is_ascii_digit() || c == &'_' {
                len += 1;
                continue;
            }
            break;
        }
        let value: String = self.ptr[self.position..self.position + len].iter().collect();
        if let Some(keyword) = keyword_map.get(value.as_str()) {
            self.tokens.push(Token::Keyword(*keyword))
        } else {
            self.tokens.push(Token::Identifier(value.into()))
        }
        self.position += len;
    }

    fn get_constant(&mut self) {
        let mut len = 1;
        while let Some(c) = self.ptr.get(self.position + len) {
            if c.is_ascii_digit() {
                len += 1;
                continue;
            }
            break;
        }
        let value: String = self.ptr[self.position..self.position + len].iter().collect();
        self.tokens.push(Token::Constant(value.into()));
        self.position += len;
    }
}
