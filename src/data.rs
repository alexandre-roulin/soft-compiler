use crate::tokenizer::{Token, TokenType};

#[allow(dead_code)]
#[derive(Debug)]
pub enum Size {
	Int,
	Byte,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Program {
	pub functions: Vec<Function>,
	pub globals: Vec<Variable>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Function {
	pub name: String,
	pub variables: Vec<Variable>,
	pub statements: Vec<Statement>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct Variable {
	pub name: String,
	pub size: usize,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Statement {
	Return(Expression),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum UnOp {
	Negation,
	Bitwise,
	LogicalNegation,
}

impl From<TokenType> for UnOp {
	fn from(token: TokenType) -> Self {
		match token {
			TokenType::Minus => UnOp::Negation,
			TokenType::Bitwise => UnOp::Bitwise,
			TokenType::LogicalNegation => UnOp::LogicalNegation,
			_ => panic!("critical error")
		}
	}
}

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq)]
pub enum BiOp {
	Minus,
	Addition,
	Multiplication,
	Division,
}

impl From<TokenType> for BiOp {
	fn from(token: TokenType) -> Self {
		match token {
			TokenType::Minus => BiOp::Minus,
			TokenType::Addition => BiOp::Addition,
			TokenType::Multiplication => BiOp::Multiplication,
			TokenType::Division => BiOp::Division,
			_ => panic!("critical error")
		}
	}
}


#[allow(dead_code)]
#[derive(Debug)]
pub enum Expression {
	Int(i32),
	UnaryOperator(UnOp, Box<Expression>),
	BinaryOperator(Box<Expression>, BiOp, Box<Expression>),
}