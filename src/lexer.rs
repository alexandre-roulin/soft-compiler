use crate::tokenizer::{TokenType, RegexMap, REGEX_MAP};
use std::collections::HashMap;
use std::fs::File;
use lazy_static::lazy_static;
use regex::*;

lazy_static! {
    static ref PATTERN_WHITESPACE: Regex = Regex::new(r"^\s+").unwrap();
    static ref PATTERN_CONSTANT: Regex = Regex::new(r"^\d+").unwrap();
    static ref PATTERN_OPERATOR: Regex = Regex::new(r"^(\+|-|\*|/|\^)").unwrap();
    static ref PATTERN_OPEN_PAREN: Regex = Regex::new(r"^\(").unwrap();
    static ref PATTERN_CLOSE_PAREN: Regex = Regex::new(r"^\)").unwrap();
}

#[warn(dead_code)]
#[derive(Debug)]
pub struct Lexer<'a> {
    text: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Lexer { text, position: 0 }
    }

    fn escaped_whitespace(&self) {
        if let Some(range) = PATTERN_WHITESPACE.find(self.text) {
            range.end();
        }
    }
}