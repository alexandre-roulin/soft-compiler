use crate::data::Declare::Declare;
use crate::data::Expression::{BinaryOperator, UnaryOperator};
use crate::data::Pair::{First, Second};
use crate::data::{BiOp, Compound, Expression, Function, Program, Statement, Variable};
use crate::tokenizer::TokenType::*;
use crate::tokenizer::{Keyword, Token, TokenType, Value};
use std::iter::Peekable;
use std::vec::IntoIter;

pub struct Parse {
    iter: Peekable<IntoIter<Token>>,
    stack: Vec<Token>,
}

#[derive(Debug)]
pub struct ParseError {
    pub error: String,
    pub position: usize,
    pub line: usize,
}

impl ParseError {
    pub fn new_with_token(error: String, token: &Token) -> ParseError {
        ParseError {
            error,
            position: token.position,
            line: token.line,
        }
    }

    pub fn new(error: String, position: usize, line: usize) -> ParseError {
        ParseError {
            error,
            position,
            line,
        }
    }
}

impl Default for ParseError {
    fn default() -> Self {
        ParseError {
            error: "".to_string(),
            position: 0,
            line: 0,
        }
    }
}

impl Parse {
    pub fn new(tokens: Vec<Token>) -> Parse {
        Parse {
            iter: tokens.into_iter().peekable(),
            stack: vec![],
        }
    }

    fn peek(&mut self) -> Result<TokenType, ParseError> {
        if let Some(token) = self.stack.last() {
            Ok(token.token.clone())
        } else if let Some(token) = self.iter.peek() {
            Ok(token.token.clone())
        } else {
            Err(ParseError::new(format!(""), 2, 4))
        }
    }

    fn next_is(&mut self, token: TokenType) -> Result<bool, ParseError> {
        if let Some(peek) = self.iter.peek() {
            if peek.token == token {
                Ok(true)
            } else {
                Err(ParseError::new_with_token(
                    format!(
                        "_!_ bad token match {:?} and shall be {:?}",
                        peek.token, token
                    ),
                    &peek,
                ))
            }
        } else {
            Err(ParseError::new(format!("miss token {:?}", token), 0, 0))
        }
    }

    fn next_token(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.stack.pop() {
            Ok(token)
        } else if let Some(e) = self.iter.next() {
            Ok(e.clone())
        } else {
            Err(ParseError::new(format!("Missing Token"), 0, 0))
        }
    }

    fn push(&mut self, token: Token) {
        self.stack.push(token)
    }

    #[allow(dead_code)]
    fn match_keyword(&mut self, keyword: Keyword) -> Result<(), ParseError> {
        let token = self.next_token()?;
        match token.token {
            TokenType::Keyword(k) if k == keyword => Ok(()),
            token_type => Err(ParseError::new(
                format!(
                    "_!_ bad token match {:?} and shall be {:?}",
                    token_type, keyword
                ),
                token.position,
                token.line,
            )),
        }
    }

    fn match_identifier(&mut self) -> Result<String, ParseError> {
        let token = self.next_token()?;
        match token.token {
            TokenType::Identifier(s) => Ok(s),
            token_type => Err(ParseError::new(
                format!(
                    "_!_ bad token match {:?} and shall be an identifier",
                    token_type
                ),
                token.position,
                token.line,
            )),
        }
    }

    fn match_token(&mut self, token_match: TokenType) -> Result<TokenType, ParseError> {
        let token = self.next_token()?;
        match token.token {
            tok if tok == token_match => Ok(tok),
            token_type => Err(ParseError::new(
                format!(
                    "_!_ bad token match {:?} and shall be {:?}",
                    token_type, token_match
                ),
                token.position,
                token.line,
            )),
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
        //println!("parse_function");
        self.match_token(TokenType::Keyword(Keyword::Int))?;
        let name = self.match_identifier()?;
        self.match_token(TokenType::OpenParentheses)?;

        let variables: Vec<Variable> = vec![];

        self.match_token(TokenType::CloseParentheses)?;
        self.match_token(TokenType::OpenBrace)?;

        let mut compounds: Vec<Compound> = vec![];

        while self.next_is(TokenType::CloseBrace).is_err() {
            compounds.push(self.parse_compound()?);
        }
        self.match_token(TokenType::CloseBrace)?;

        Ok(Function {
            name,
            variables,
            compounds,
        })
    }

    fn parse_compound(&mut self) -> Result<Compound, ParseError> {
        //                println!("parse_compound {:?}", self.peek()?);
        match self.peek()? {
            TokenType::Keyword(size @ Keyword::Int) => {
                self.next_token()?;
                let name = self.match_identifier()?;
                match self.peek()? {
                    Assignement => {
                        //println!("Assignement");
                        self.next_token()?;
                        let expr = self.parse_expression()?;
                        self.match_token(TokenType::Semicolon)?;
                        Ok(Compound::Declare(Declare(
                            Variable::new(name, size.into()),
                            Some(expr),
                        )))
                    }
                    Semicolon => {
                        //println!("Semicolon");
                        self.match_token(TokenType::Semicolon)?;
                        Ok(Compound::Declare(Declare(
                            Variable::new(name, size.into()),
                            None,
                        )))
                    }
                    t => Err(ParseError::new_with_token(
                        format!("Error assigment in statement `{:?}`", t),
                        &self.next_token()?,
                    )),
                }
            }
            _ => Ok(Compound::Statement(self.parse_statement()?)),
        }
    }

    fn
    parse_brace(&mut self) -> Result<Statement, ParseError> {
        if self.peek()? == OpenBrace {
            self.next_token()?;
            let mut compounds = vec![];
            while self.next_is(TokenType::CloseBrace).is_err() {
                compounds.push(self.parse_compound()?);
            }

            self.next_token()?;
            Ok(Statement::Compound(compounds))
        } else {
            Ok(self.parse_statement()?)
        }
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        println!("{:?}", self.peek());
        match self.peek()? {
            TokenType::Keyword(Keyword::Return) => {
                self.next_token()?;
                let statement = Ok(Statement::Return(self.parse_expression()?));
                self.match_token(TokenType::Semicolon)?;
                statement
            }

            TokenType::Identifier(_) => {
                let statement = Ok(Statement::Expression(Some(self.parse_expression()?)));
                self.match_token(Semicolon)?;
                statement
            }
            TokenType::Literal(_) => {
                let statement = Ok(Statement::Expression(Some(self.parse_expression()?)));
                self.match_token(Semicolon)?;
                statement
            }
            TokenType::OpenBrace => {
                self.next_token()?;
                let mut compounds = vec![];
                while self.next_is(TokenType::CloseBrace).is_err() {
                    compounds.push(self.parse_compound()?);
                }

                let statement = Statement::Compound(compounds);
                self.next_token()?;
                Ok(statement)
            }
            TokenType::Keyword(Keyword::If) => {
                self.next_token()?;
                self.match_token(OpenParentheses)?;

                let condition = self.parse_expression()?;

                self.match_token(CloseParentheses)?;

                let statement = self.parse_brace()?;

                let mut else_statement = None;
                if self.peek()? == TokenType::Keyword(Keyword::Else) {
                    self.next_token()?;
                    else_statement = if self.peek()? == TokenType::Keyword(Keyword::If) {
                        Some(Box::new(self.parse_statement()?))
                    } else {
                        Some(Box::new(self.parse_brace()?))
                    }
                }

                Ok(Statement::If(
                    condition,
                    Box::new(statement),
                    else_statement,
                ))
            }
            TokenType::Keyword(Keyword::Break) => Ok(Statement::Break),
            TokenType::Keyword(Keyword::Continue) => Ok(Statement::Continue),
            TokenType::Keyword(Keyword::Do) => {
                self.next_token()?;

                self.match_token(OpenBrace)?;

                let mut compounds = vec![];

                while self.peek()? != CloseBrace {
                    compounds.push(self.parse_compound()?);
                }

                self.match_token(CloseBrace)?;

                self.match_token(TokenType::Keyword(Keyword::While))?;
                self.match_token(OpenParentheses)?;
                let expression = self.parse_expression()?;
                self.match_token(CloseParentheses)?;
                self.match_token(Semicolon)?;
                Ok(Statement::Do(compounds, expression))
            }
            TokenType::Keyword(Keyword::While) => {
                self.next_token()?;

                self.match_token(OpenParentheses)?;
                let expression = self.parse_expression()?;
                self.match_token(CloseParentheses)?;

                let statement = Box::new(self.parse_brace()?);

                Ok(Statement::While(expression, statement))
            }
            TokenType::Keyword(Keyword::For) => {
                self.next_token()?;
                self.match_token(OpenParentheses)?;

                // Looking for declaration or assignation
                let initial;

                if self.peek()? == TokenType::Keyword(Keyword::Int) {
                    if let Compound::Declare(declare) = self.parse_compound()? {
                        initial = First(declare);
                    } else {
                        panic!("Declare")
                    }
                } else if self.peek()? == Semicolon {
                    initial = Second(None);
                    self.next_token()?;
                } else {
                    initial = Second(Some(self.parse_expression()?));
                    self.next_token()?;
                }
                // middle of the for
                let condition = if self.peek()? == Semicolon {
                    self.parse_expression()?
                } else {
                    let expression = self.parse_expression()?;
                    self.match_token(Semicolon)?;
                    expression
                };

                let mut post_expression = None;
                if self.peek()? != CloseParentheses {
                    post_expression = Some(self.parse_expression()?);
                }
                self.match_token(CloseParentheses)?;

                let statements = Box::new(self.parse_brace()?);

                match initial {
                    First(initial) => Ok(Statement::ForDecl(
                        initial,
                        condition,
                        post_expression,
                        statements,
                    )),
                    Second(initial) => Ok(Statement::For(
                        initial,
                        condition,
                        post_expression,
                        statements,
                    )),
                }
            }
            TokenType::Semicolon => Ok(Statement::Expression(Some(self.parse_expression()?))),
            _ => {
                let expression = self.parse_expression()?;

                self.match_token(Semicolon)?;

                Ok(Statement::Expression(Some(expression)))
            }
        }
    }

    // 3 * 2 * 1
    // 3 + (2 * 1)
    fn parse_expression(&mut self) -> Result<Expression, ParseError> {
        //        println!("parse_expression");
        self.generate_semicolon()
    }

    fn generate_semicolon(&mut self) -> Result<Expression, ParseError> {
        if self.peek()? == Semicolon {
            self.next_token()?;
            Ok(Expression::Int(1))
        } else {
            self.parse_assignement()
        }
    }

    fn generate_assignement(
        &mut self,
        name: String,
        token: BiOp,
    ) -> Result<Expression, ParseError> {
        let bin_operator = BinaryOperator(
            Box::new(Expression::Variable(name.clone())),
            token,
            Box::new(self.parse_expression()?),
        );
        Ok(Expression::Assign(name, Box::new(bin_operator)))
    }

    fn parse_assignement(&mut self) -> Result<Expression, ParseError> {
        let (a, b) = (self.next_token()?, self.next_token()?);
        let (a_c, b_c) = (a.clone(), b.clone());
        match (a.token, b.token) {
            (Identifier(name), token @ Assignement) => {
                self.generate_assignement(name, token.into())
            }
            (Identifier(name), token @ AssignPlus) => self.generate_assignement(name, token.into()),
            (Identifier(name), token @ AssignMultiply) => {
                self.generate_assignement(name, token.into())
            }
            (Identifier(name), token @ AssignMinus) => {
                self.generate_assignement(name, token.into())
            }
            (Identifier(name), token @ AssignDivide) => {
                self.generate_assignement(name, token.into())
            }
            (_, _) => {
                self.push(b_c);
                self.push(a_c);
                self.parse_ternary_condition()
            }
        }
    }

    fn parse_ternary_condition(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.parse_logical_or_operator()?;
        while self.peek()? == QuestionMark {
            self.next_token()?;
            let body = self.parse_expression()?;
            self.match_token(Colon)?;
            let else_expr = self.parse_ternary_condition()?;
            expr = Expression::CondExp(Box::new(expr), Box::new(body), Box::new(else_expr))
        }
        Ok(expr)
    }

    fn parse_logical_or_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(&[LogicalOr], &Parse::parse_logical_and_operator)
    }

    fn parse_logical_and_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(&[LogicalAnd], &Parse::parse_bitwise_operator)
    }

    fn parse_bitwise_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(
            &[BitwiseXOR, BitwiseAND, BitwiseOR],
            &Parse::parse_equality_operator,
        )
    }

    fn parse_equality_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(&[Equal, NotEqual], &Parse::parse_relational_operator)
    }

    fn parse_relational_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(
            &[LessThan, LessOrEqual, GreaterThan, GreaterOrEqual],
            &Parse::parse_bitwise_shift,
        )
    }

    fn parse_bitwise_shift(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(
            &[BitwiseShiftRight, BitwiseShiftLeft],
            &Parse::parse_lower_operator,
        )
    }

    fn parse_lower_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(&[Addition, Minus], &Parse::parse_higher_operator)
    }

    fn parse_higher_operator(&mut self) -> Result<Expression, ParseError> {
        self.parse_generate_expression(&[Multiplication, Division, Modulus], &Parse::parse_factor)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParseError> {
        let token = self.next_token()?;
        //println!("parse_factor {:?} {:?}", token.token, self.peek()?);
        match (token.token, self.peek()?) {
            (Identifier(name), token @ Increment) => {
                self.generate_prefix_operator(token, name, true)
            }
            (Identifier(name), token @ Decrement) => {
                self.generate_prefix_operator(token, name, true)
            }
            (token @ Decrement, Identifier(name)) => {
                self.generate_prefix_operator(token, name, false)
            }
            (token @ Increment, Identifier(name)) => {
                self.generate_prefix_operator(token, name, false)
            }
            (Literal(Value::Int(nu)), _) => Ok(Expression::Int(nu)),
            (OpenParentheses, _) => {
                let expression = self.parse_expression();
                self.match_token(CloseParentheses)?;
                expression
            }
            (tokentype @ TokenType::Bitwise, _)
            | (tokentype @ TokenType::LogicalNegation, _)
            | (tokentype @ TokenType::Minus, _) => {
                let factor = self.parse_factor()?;
                Ok(UnaryOperator(tokentype.into(), Box::new(factor)))
            }
            (Identifier(name), _) => Ok(Expression::Variable(name)),
            (token, _) => Err(ParseError::new(format!("error factor {:?} ", token), 3, 4)),
        }
    }

    fn generate_prefix_operator(
        &mut self,
        token: TokenType,
        name: String,
        prefix: bool,
    ) -> Result<Expression, ParseError> {
        self.next_token()?;

        let expression = Box::new(Expression::BinaryOperator(
            Box::new(Expression::Variable(name.clone())),
            token.into(),
            Box::new(Expression::Int(1)),
        ));

        match prefix {
            true => Ok(Expression::Assign(name, expression)),
            false => Ok(Expression::AssignPost(name, expression)),
        }
    }

    fn parse_generate_expression<F>(
        &mut self,
        tokens: &[TokenType],
        next: F,
    ) -> Result<Expression, ParseError>
    where
        F: Fn(&mut Parse) -> Result<Expression, ParseError>,
    {
        let mut term = next(self)?;
        loop {
            if let Ok(tokentype) = self.peek() {
                if tokens.contains(&tokentype) {
                    let biop = self.next_token()?.token.into();
                    let next_term = next(self)?;
                    term = Expression::BinaryOperator(Box::new(term), biop, Box::new(next_term));
                } else {
                    return Ok(term);
                }
            } else {
                return Ok(term);
            }
        }
    }
}
