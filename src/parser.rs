use std::iter::Peekable;
use crate::tokenizer::{TokenType, Keyword, Value, Token};
use crate::tokenizer::TokenType::*;
use std::slice::Iter;
use crate::data::{Program, Function, Variable, Statement, Expression, BiOp};

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


impl Parse<'_> {
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
				let statement = Ok(Statement::Return(self.parse_expression(None)?));
				self.match_token(TokenType::Semicolon)?;
				statement
			}
			invalid_token => {
				Err(ParseError::new(format!("_!_ a token is out of place {:?} ", invalid_token), token.position, token.line))
			}
		}
	}


	fn parse_expression(&mut self, expr: Option<Expression>) -> Result<Expression, ParseError> {
		match self.peek()? {
			Literal(_) => {
				let expr = self.generate_expression_from_literal()?;
				self.parse_expression(Some(expr))
			}
			TokenType::Minus => {
				if let Some(e) = expr { self.generate_binary_operator(e) } else { self.generate_unary_operator() }
			}
			tok if tok.is_binary_operator() => {
				if let Some(e) = expr { self.generate_binary_operator(e) } else { Err(ParseError::new_with_token(format!("binary error"), &self.next_token()?)) }
			}
			Semicolon => {
				Ok(expr.unwrap())
			}
			_ => {
				self.parse_single_expression()
			}
		}
	}

	fn parse_single_expression(&mut self) -> Result<Expression, ParseError> {
		match self.peek()? {
			tok if tok.is_unary_operator() => self.generate_unary_operator(),
			Literal(_) => self.generate_expression_from_literal(),
			OpenParentheses => {
				self.next_token()?;
				let expression = self.parse_expression(None);
				self.match_token(TokenType::CloseParentheses)?;
				expression
			}
			_ => {
				let t = self.next_token()?;
				Err(ParseError::new_with_token(format!("expression error {:?}", t.token), &t))
			}
		}
	}

	fn generate_binary_operator(&mut self, expression: Expression) -> Result<Expression, ParseError> {
		let biop = self.next_token()?.token.to_binary_operator();
		match biop {
			BiOp::Minus | BiOp::Addition => {
				Ok(Expression::BinaryOperator(Box::new(expression), biop, Box::new(self.parse_expression(None)?)))
			}
			BiOp::Multiplication | BiOp::Division => {
				let expr = Box::new(self.parse_single_expression()?);
				let bin = Expression::BinaryOperator(Box::new(expression), biop, expr);
				println!("{:?}", bin);
				self.parse_expression(Some(bin))
			}
		}
	}

	fn generate_unary_operator(&mut self) -> Result<Expression, ParseError> {
		let unop = self.next_token()?.token.to_unary_operator();
		let expr = Box::new(self.parse_single_expression()?);
		Ok(Expression::UnaryOperator(unop, expr))
	}

	fn generate_expression_from_literal(&mut self) -> Result<Expression, ParseError> {
		let t = self.next_token()?;
		match t.token {
			TokenType::Literal(Value::Int(nu)) => { Ok(Expression::Int(nu)) }
			TokenType::Literal(Value::Char(_)) => { Ok(Expression::Int(42)) }
			_ => Err(ParseError::new(format!("t{:?}", t), 2, 4))
		}
	}
}

