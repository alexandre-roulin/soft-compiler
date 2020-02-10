use std::fmt::{Error, Formatter};
use std::fs::File;
use std::io::Read;
use std::rc::Rc;

#[allow(dead_code)]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub line: usize,
    pub position: usize,
    pub token: TokenType,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{:?}", self.token)
    }
}

impl Token {
    pub fn new(token: TokenType, position: usize, line: usize) -> Token {
        Token {
            line,
            position,
            token,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Int(i32),
    Char(u8),
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Keyword(Keyword),
    Identifier(String),
    Literal(Value),
    OpenParentheses,    // (
    CloseParentheses,   // )
    OpenBrace,          // {
    CloseBrace,         // }
    Semicolon,          // ;
    Whitespace,         // ' '
    Minus,              // -
    Bitwise,            // ~
    LogicalNegation,    // !
    Addition,           // +
    Multiplication,     // *
    Division,           // /
    Modulus,            // %
    LogicalAnd,         // &&
    LogicalOr,          // ||
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    LessOrEqual,        // <=
    GreaterThan,        // >=
    GreaterOrEqual,     // >=
    Assignement,        // =
    AssignPlus,         // +=
    AssignMinus,        // -=
    AssignDivide,       // /=
    AssignMultiply,     // *=
    AssignModulus,      // %=
    AssignBitwiseLeft,  // <<=
    AssignBitwiseRight, // >>=
    AssignAND,          // &=
    AssignOR,           // |=
    AssignXOR,          // ^=
    BitwiseXOR,         // ^
    BitwiseAND,         // &
    BitwiseOR,          // |
    BitwiseShiftLeft,   // >>
    BitwiseShiftRight,  // <<
    Colon,              // :
    QuestionMark,       // ?
    Increment,          // ++
    Decrement,          // --
}

#[derive(Debug, Clone, PartialEq, Copy, Hash, Eq)]
pub enum Keyword {
    Return,
    Int,
    If,
    Else,
    Continue,
    Break,
    While,
    Do,
    For,
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
    ptr: Vec<char>,
    pub file: String,
    position: usize,
    line: usize,
    pub tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new(filename: Rc<String>) -> Self {
        let file = Self::read_file(filename);
        //        println!("{}", file);
        Tokenizer {
            ptr: file.chars().collect(),
            file,
            position: 0,
            line: 0,
            tokens: vec![],
        }
    }

    fn read_file(filename: Rc<String>) -> String {
        let mut s = String::new();
        let mut file = File::open(filename.as_str()).expect("File not found");
        file.read_to_string(&mut s).expect("Error reading file");

        s
    }

    fn add_token(&mut self, token: TokenType) {
        self.tokens
            .push(Token::new(token, self.position, self.line));
    }
}

impl Tokenizer {
    fn get_char_type(&self, advance_from_pos: usize) -> Option<CharacterType> {
        self.ptr.get(self.position + advance_from_pos).map(|c| {
            if c.is_alphabetic() || c == &'_' {
                CharacterType::Alphabetic
            } else if c.is_ascii_digit() {
                CharacterType::Numeric
            } else if c == &' ' || c == &'\t' {
                CharacterType::Whitespace
            } else if c == &'\n' || c == &'\r' {
                CharacterType::NewLine
            } else {
                CharacterType::NonAlphabetic(*c)
            }
        })
    }

    pub fn tokenize(&mut self) {
        while let Some(ch) = self.get_char_type(0) {
            match ch {
                CharacterType::Whitespace => self.position += 1,
                CharacterType::Alphabetic => self.get_identifier(),
                CharacterType::Numeric => self.get_literal(),
                CharacterType::NewLine => {
                    self.position += 1;
                    self.line += 1;
                }
                CharacterType::NonAlphabetic(c) => {
                    self.position += 1;
                    match c {
                        '(' => self.add_token(TokenType::OpenParentheses),
                        ')' => self.add_token(TokenType::CloseParentheses),
                        '{' => self.add_token(TokenType::OpenBrace),
                        '}' => self.add_token(TokenType::CloseBrace),
                        ';' => self.add_token(TokenType::Semicolon),
                        '~' => self.add_token(TokenType::Bitwise),
                        '!' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::NotEqual)
                            } else {
                                self.add_token(TokenType::LogicalNegation)
                            }
                        }
                        '-' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::AssignMinus)
                            } else if let Some(CharacterType::NonAlphabetic(_ch @ '-')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::Decrement)
                            } else {
                                self.add_token(TokenType::Minus)
                            }
                        }
                        '*' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::AssignMultiply)
                            } else {
                                self.add_token(TokenType::Multiplication)
                            }
                        }
                        '/' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::AssignDivide)
                            } else {
                                self.add_token(TokenType::Division)
                            }
                        }
                        '+' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '+')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::Increment)
                            } else if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::AssignPlus)
                            } else {
                                self.add_token(TokenType::Addition)
                            }
                        }
                        '<' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::LessOrEqual)
                            } else if let Some(CharacterType::NonAlphabetic(_ch @ '<')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::BitwiseShiftLeft)
                            } else {
                                self.add_token(TokenType::LessThan)
                            }
                        }
                        '>' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::GreaterOrEqual)
                            } else if let Some(CharacterType::NonAlphabetic(_ch @ '>')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::BitwiseShiftRight)
                            } else {
                                self.add_token(TokenType::GreaterThan)
                            }
                        }
                        '&' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '&')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::LogicalAnd)
                            } else {
                                self.add_token(TokenType::BitwiseAND)
                            }
                        }
                        '|' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '|')) =
                                self.get_char_type(0)
                            {
                                self.position += 1;
                                self.add_token(TokenType::LogicalOr)
                            } else {
                                self.add_token(TokenType::BitwiseOR)
                            }
                        }
                        '=' => {
                            if let Some(CharacterType::NonAlphabetic(_ch @ '=')) =
                                self.get_char_type(0)
                            {
                                self.add_token(TokenType::Equal);
                                self.position += 1;
                            } else {
                                self.add_token(TokenType::Assignement);
                            }
                        }
                        '^' => self.add_token(TokenType::BitwiseXOR),
                        '%' => self.add_token(TokenType::Modulus),
                        '?' => self.add_token(TokenType::QuestionMark),
                        ':' => self.add_token(TokenType::Colon),

                        _ => {}
                    }
                }
            }
        }
    }

    fn get_identifier(&mut self) {
        let mut len = 1;
        while let Some(c) = self.ptr.get(self.position + len) {
            if c.is_alphabetic() || c.is_ascii_digit() || c == &'_' {
                len += 1;
                continue;
            }
            break;
        }
        let value: String = self.ptr[self.position..self.position + len]
            .iter()
            .collect();
        match value.as_str() {
            "int" => self.add_token(TokenType::Keyword(Keyword::Int)),
            "return" => self.add_token(TokenType::Keyword(Keyword::Return)),
            "if" => self.add_token(TokenType::Keyword(Keyword::If)),
            "else" => self.add_token(TokenType::Keyword(Keyword::Else)),
            "for" => self.add_token(TokenType::Keyword(Keyword::For)),
            "do" => self.add_token(TokenType::Keyword(Keyword::Do)),
            "while" => self.add_token(TokenType::Keyword(Keyword::While)),
            "continue" => self.add_token(TokenType::Keyword(Keyword::Continue)),
            "break" => self.add_token(TokenType::Keyword(Keyword::Break)),
            name => self.add_token(TokenType::Identifier(name.into())),
        }
        self.position += len;
    }

    fn get_literal(&mut self) {
        let mut len = 1;
        while let Some(c) = self.ptr.get(self.position + len) {
            if c.is_ascii_digit() {
                len += 1;
                continue;
            }
            break;
        }
        let value: String = self.ptr[self.position..self.position + len]
            .iter()
            .collect();
        self.add_token(TokenType::Literal(Value::Int(
            value.parse().expect("Error parsing literal value"),
        )));
        self.position += len;
    }
}
