use crate::tokenizer::{TokenType, Keyword, Value, Token};
use crate::tokenizer::TokenType::*;
use crate::data::{Program, Function, Variable, Statement, Expression, BiOp};
use std::vec::IntoIter;
use std::iter::Peekable;
use crate::data::Expression::UnaryOperator;

pub struct Parse {
	iter: Peekable<IntoIter<Token>>
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

	pub fn new(error: String, position: usize, line: usize) -> ParseError {
		ParseError { error, position, line }
	}
}

impl Default for ParseError {
	fn default() -> Self {
		ParseError { error: "".to_string(), position: 0, line: 0 }
	}
}


impl Parse {
	pub fn new(tokens: Vec<Token>) -> Parse {
		Parse {
			iter: tokens.into_iter().peekable()
		}
	}

	fn peek(&mut self) -> Result<TokenType, ParseError> {
		if let Some(token) = self.iter.peek() {
			Ok(token.token.clone())
		} else { Err(ParseError::new(format!(""), 2, 4)) }
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
	#[allow(dead_code)]
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

	fn has_next(&mut self) -> bool {
		self.iter.peek().is_some()
	}
}


impl Parse {
	pub fn parse(&mut self) -> Result<Program, ParseError> {
		let mut functions: Vec<Function> = vec![];
		let globals: Vec<Variable> = vec![];

		while self.has_next() && self.next_is(TokenType::Keyword(Keyword::Int))? {
			functions.push(self.parse_function()?);
		}


		Ok(Program { functions, globals })
	}

	fn parse_function(&mut self) -> Result<Function, ParseError> {
		self.match_token(TokenType::Keyword(Keyword::Int))?;
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
				let statement = Ok(Statement::Return(self.parse_expression()?));
				self.match_token(TokenType::Semicolon)?;
				statement
			}
			invalid_token => {
				Err(ParseError::new(format!("_!_ a token is out of place {:?} ", invalid_token), token.position, token.line))
			}
		}
	}
	// 3 * 2 * 1
	// 3 + (2 * 1)
	fn parse_expression(&mut self) -> Result<Expression, ParseError> {
		//println!("parse_expression");
		self.parse_lower_operation()
	}

	fn parse_higher_operation(&mut self) -> Result<Expression, ParseError> {
		//println!("parse_higher_operation");
		self.parse_generate_expression(
			&[Multiplication, Division],
			&Parse::parse_factor,
		)
	}
	fn parse_lower_operation(&mut self) -> Result<Expression, ParseError> {
		//println!("parse_lower_operation");
		self.parse_generate_expression(
			&[Addition, Minus],
			&Parse::parse_higher_operation,
		)
	}

	fn parse_factor(&mut self) -> Result<Expression, ParseError> {
		//println!("parse_factor");
		let token = self.next_token()?;
		match (token.token, self.peek()?) {
			(Literal(Value::Int(nu)), _) => {
				Ok(Expression::Int(nu))
			}
			(OpenParentheses, _) => {
				let expression = self.parse_expression();
				self.match_token(CloseParentheses)?;
				expression
			}
			(tokentype @ TokenType::Bitwise, _) | (tokentype @ TokenType::LogicalNegation, _) | (tokentype @ TokenType::Minus, _) => {
				let factor = self.parse_factor()?;
				Ok(UnaryOperator(tokentype.into(), Box::new(factor)))
			}
			_ => { Err(ParseError::default()) }
		}
	}

	fn parse_generate_expression<F>(&mut self, tokens: &[TokenType], next: F) -> Result<Expression, ParseError>
		where F: Fn(&mut Parse) -> Result<Expression, ParseError> {
		//println!("parse_generate_expression {:?}", tokens);
		let mut term = next(self)?;
		//println!("term {:?}", term);
		loop {
			if let Ok(tokentype) = self.peek() {
				if tokens.contains(&tokentype) {
					let biop = self.next_token()?.token.into();
					let next_term = next(self)?;
					term = Expression::BinaryOperator(Box::new(term), biop, Box::new(next_term));
					//println!("term {:?}", term);
				} else { return Ok(term); }
			} else { return Ok(term); }
		}
	}
}

