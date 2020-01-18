use lazy_static::lazy_static;
use std::collections::HashMap;
use regex::Regex;

pub type RegexMap = HashMap<TokenType, Regex>;


lazy_static! {
    pub static ref REGEX_MAP : RegexMap = {
        let mut m =  HashMap::new();
        m.insert(TokenType::Int,Regex::new(  r"int").unwrap());
        m.insert(TokenType::CharLiteral,Regex::new(  r"[a-zA-Z]{1}").unwrap());
        m.insert(TokenType::Identifier,Regex::new(  r"[a-zA-Z]*\w").unwrap());
        m.insert(TokenType::OpenParenthese,Regex::new(  r"(").unwrap());
        m.insert(TokenType::CloseParenthese,Regex::new(  r")").unwrap());
        m.insert(TokenType::OpenBrace,Regex::new(  r"[").unwrap());
        m.insert(TokenType::CloseBrace,Regex::new(  r"]").unwrap());
        m.insert(TokenType::Return,Regex::new(  r"return").unwrap());
        m.insert(TokenType::Semicolon,Regex::new(  r";").unwrap());
        m
    };
}

#[allow(dead_code)]
#[derive(Debug, Clone, Ord, PartialOrd, PartialEq, Eq, Hash)]
pub enum TokenType {
    Int,
    CharLiteral,
    Identifier,
    OpenParenthese,
    CloseParenthese,
    OpenBrace,
    CloseBrace,
    Return,
    Semicolon,
    Whitespace,
}

