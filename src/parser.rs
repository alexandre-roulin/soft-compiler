use std::iter::Peekable;
use crate::tokenizer::{TokenType, Keyword, Value, Token};
use std::slice::Iter;
use crate::data::{Program, Function, Variable, Statement, Expression};

pub struct Parse<'a> {
    iter: Peekable<Iter<'a, Token>>
}

pub struct ParseError {
    pub error: String,
    pub position: usize,
    pub line: usize,
}

impl ParseError {
    pub fn new_with_token(error: String, token: &Token) -> ParseError {
        ParseError { error, position: token.position, line: token.line }
    }

    pub fn new_with_token_unref(error: String, token: Token) -> ParseError {
        ParseError { error, position: token.position, line: token.line }
    }

    pub fn new(error: String, position: usize, line: usize) -> ParseError {
        ParseError { error, position, line }
    }
}

impl Parse<'_> {
    pub fn new(tokens: &Vec<Token>) -> Parse {
        Parse {
            iter: tokens.into_iter().peekable()
        }
    }

    fn next_is(&mut self, token: TokenType) -> Result<bool, ParseError> {
        if let Some(peek) = self.iter.peek() {
            if peek.token == token {
                Ok(true)
            } else {
                Err(ParseError::new_with_token(format!("_!_ bad token match {:?} and shall be {:?}", peek.token, token), &peek))
            }
        } else {
            Err(ParseError::new(format!("miss token {:?}", token), 0, 0))
        }
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        if let Some(e) = self.iter.next() {
            Ok(e.clone())
        } else {
            Err(ParseError::new(format!("Missing Token"), 0, 0))
        }
    }

    fn match_keyword(&mut self, keyword: Keyword) -> Result<(), ParseError> {
        let token = self.next_token()?;
        match token.token {
            TokenType::Keyword(k) if k == keyword => Ok(()),
            token_type => Err(ParseError::new(format!("_!_ bad token match {:?} and shall be {:?}", token_type, keyword), token.position, token.line))
        }
    }

    fn match_identifier(&mut self) -> Result<String, ParseError> {
        let token = self.next_token()?;
        match token.token {
            TokenType::Identifier(s) => Ok(s),
            token_type => Err(ParseError::new(format!("_!_ bad token match {:?} and shall be an identifier", token_type), token.position, token.line))
        }
    }

    fn match_token(&mut self, token_match: TokenType) -> Result<TokenType, ParseError> {
        let token = self.next_token()?;
        match token.token {
            tok if tok == token_match => Ok(tok),
            token_type => Err(ParseError::new(format!("_!_ bad token match {:?} and shall be {:?}", token_type, token_match), token.position, token.line))
        }
    }
}


impl Parse<'_> {
    pub fn parse(&mut self) -> Result<Program, ParseError> {
        let mut functions: Vec<Function> = vec![];
        let globals: Vec<Variable> = vec![];

        while self.next_is(TokenType::Keyword(Keyword::Int))? {
            functions.push(self.parse_function()?);
        }


        Ok(Program { functions, globals })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        self.match_token(TokenType::Keyword(Keyword::Int));
        let name = self.match_identifier()?;
        self.match_token(TokenType::OpenParentheses)?;

        let variables: Vec<Variable> = vec![];

        self.match_token(TokenType::CloseParentheses)?;
        self.match_token(TokenType::OpenBrace)?;

        let mut statements: Vec<Statement> = vec![];

        while self.next_is(TokenType::CloseBrace).is_err() {
            statements.push(self.parse_statement()?);
        }
        self.match_token(TokenType::CloseBrace)?;

        Ok(Function { name, variables, statements })
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        let token = self.next_token()?;
        match token.token {
            TokenType::Keyword(Keyword::Return) => {
                let token = self.next_token()?;
                let expression = match token.token {
                    TokenType::Literal(Value::Int(nu)) => Some(Expression::Int(nu)),
                    TokenType::Literal(Value::Char(c)) => Some(Expression::Char(c)),
                    _ => { None }
                };
                self.match_token(TokenType::Semicolon)?;
                if let Some(expr) = expression {
                    Ok(Statement::Return(expr))
                } else { Err(ParseError::new_with_token(format!("_!_ a token shall be a expression"), &token)) }
            }
            invalid_token => {
                Err(ParseError::new(format!("_!_ a token is out of place {:?} ", invalid_token), token.position, token.line))
            }
        }
    }
}

