pub mod parser;

use std::fmt;
use crate::lexing::lexer;
use crate::stream;

/// Represents the two types of syntax errors: the encountering of an unexpected
/// token, and the encountering of the end of the token stream when it is not
/// expected.
#[derive(Debug, PartialEq)]
pub enum Failure {
    UnexpectedToken(lexer::Token, &'static str),
    UnexpectedStreamEnd(&'static str),
    UnexpectedIndent { expected_indent: usize, encountered_indent: usize }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedToken(tok, expected) => write!(f, "Expected {} yet encountered unexpected {}", expected, tok),
            Failure::UnexpectedStreamEnd(expected) => write!(f, "Encountered the end of the token stream yet expected {}", expected),
            Failure::UnexpectedIndent { expected_indent, encountered_indent } =>
                write!(f, "Encountered an unexpected change in indentation from the expected level of {} to an indentation level of {} tabs", expected_indent, encountered_indent)
        }
    }
}

type Result<T> = std::result::Result<T, Failure>;

#[derive(Debug, PartialEq)]
pub enum Statement {
    If {
        condition: Expression,
        block: Block
    },

    While {
        condition: Expression,
        block: Block
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
    },

    Return(Option<Expression>)
}

pub type Block = Vec<Statement>;

#[derive(Debug, PartialEq)]
pub struct Parameter ( Type, String, stream::Position );

#[derive(Debug, PartialEq)]
pub enum Type {
    Identifier { pos: stream::Position, identifier: String },
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
    CharLiteral { pos: stream::Position, value: char },
    BooleanLiteral { pos: stream::Position, value: bool },
    Variable { pos: stream::Position, identifier: String },
    FunctionCall { pos: stream::Position, identifier: String, args: Vec<Expression> }
}