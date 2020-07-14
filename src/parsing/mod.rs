pub mod parser;

use crate::stream;

#[derive(Debug, PartialEq)]
pub enum Statement {
    If {
        condition: Expression,
        if_block: Block,
        else_block: Option<Block>
    },

    FunctionDefinition {
        identifier: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Block
    },

    VariableDeclaration {
        var_type: Type,
        identifier: String,
        value: Option<Expression>
    },

    VariableAssignment {
        identifier: String,
        assign_to: Expression
    }
}

#[derive(Debug, PartialEq)]
pub struct Block ( Vec<Statement> );

#[derive(Debug, PartialEq)]
pub struct Parameter ( Type, String, stream::Position );

#[derive(Debug, PartialEq)]
pub enum Type {
    Identifier {pos: stream::Position, identifier: String },
    Array(Box<Type>)
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Equal(Box<Expression>, Box<Expression>),
    //NotEqual(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    BooleanNot(Box<Expression>),
    UnaryMinus(Box<Expression>),
    Array(Vec<Expression>),

    NumberLiteral { pos: stream::Position, value: f64 },
    StringLiteral { pos: stream::Position, value: String },
    BooleanLiteral { pos: stream::Position, value: bool },
    Variable { pos: stream::Position, identifier: String },
    FunctionCall { pos: stream::Position, identifier: String, args: Vec<Expression> }
}