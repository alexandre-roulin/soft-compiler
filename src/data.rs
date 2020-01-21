
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
pub enum Expression {
    Int(i32),
    Char(u8),
}