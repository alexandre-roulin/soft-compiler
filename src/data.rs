use crate::tokenizer::{Keyword, TokenType};

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Size {
    Int,
    Byte,
}

impl From<Keyword> for Size {
    fn from(keyword: Keyword) -> Self {
        match keyword {
            Keyword::Int => Size::Int,
            _ => panic!("critical error for type `{:?}`", keyword),
        }
    }
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
    pub compounds: Vec<Compound>,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub name: String,
    pub size: Size,
}

impl From<Size> for i32 {
    fn from(s: Size) -> Self {
        match s {
            Size::Int => 8,
            Size::Byte => 1,
        }
    }
}

impl Variable {
    pub fn new(name: String, size: Size) -> Variable {
        Variable { name, size }
    }
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Compound {
    Statement(Statement),
    Declare(Declare),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Declare {
    Declare(Variable, Option<Expression>),
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Option<Expression>),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Compound(Vec<Compound>), // { .. }
    For(
        Option<Expression>,
        Expression,
        Option<Expression>,
        Box<Statement>,
    ),
    ForDecl(Declare, Expression, Option<Expression>, Box<Statement>),
    While(Expression, Box<Statement>),
    Do(Vec<Compound>, Expression),
    Break,
    Continue,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
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
            _ => panic!("critical error"),
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
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assign,
    BitwiseAND,
    BitwiseOR,
    BitwiseXOR,
    BitwiseShiftLeft,
    BitwiseShiftRight,
    Modulus,
    QuestionMark,
    Colon,
    Increment,
    Decrement,
}

impl From<TokenType> for BiOp {
    fn from(token: TokenType) -> Self {
        match token {
            TokenType::Minus => BiOp::Minus,
            TokenType::Addition => BiOp::Addition,
            TokenType::Multiplication => BiOp::Multiplication,
            TokenType::Division => BiOp::Division,
            TokenType::LogicalAnd => BiOp::LogicalAnd,
            TokenType::LogicalOr => BiOp::LogicalOr,
            TokenType::Equal => BiOp::Equal,
            TokenType::NotEqual => BiOp::NotEqual,
            TokenType::LessThan => BiOp::LessThan,
            TokenType::LessOrEqual => BiOp::LessOrEqual,
            TokenType::GreaterThan => BiOp::GreaterThan,
            TokenType::GreaterOrEqual => BiOp::GreaterOrEqual,
            TokenType::Assignement => BiOp::Assign,
            TokenType::AssignPlus => BiOp::Addition,
            TokenType::AssignMinus => BiOp::Minus,
            TokenType::AssignDivide => BiOp::Division,
            TokenType::AssignMultiply => BiOp::Multiplication,
            TokenType::BitwiseAND => BiOp::BitwiseAND,
            TokenType::BitwiseOR => BiOp::BitwiseOR,
            TokenType::BitwiseXOR => BiOp::BitwiseXOR,
            TokenType::BitwiseShiftLeft => BiOp::BitwiseShiftLeft,
            TokenType::BitwiseShiftRight => BiOp::BitwiseShiftRight,
            TokenType::Modulus => BiOp::Modulus,
            TokenType::QuestionMark => BiOp::QuestionMark,
            TokenType::Colon => BiOp::Colon,
            TokenType::Increment => BiOp::Addition,
            TokenType::Decrement => BiOp::Minus,
            _ => panic!("critical error"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    UnaryOperator(UnOp, Box<Expression>),
    BinaryOperator(Box<Expression>, BiOp, Box<Expression>),
    Assign(String, Box<Expression>),
    AssignPost(String, Box<Expression>),
    Variable(String),
    CondExp(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub enum Pair<F, S> {
    First(F),
    Second(S),
}
