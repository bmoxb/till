//! Contains structures and enumerations that allow for the representation of
//! a till abstract syntax tree. For the actual till parsing code, see submodule
//! `parser`.

pub mod parser;

use crate::lexing::lexer;
use crate::stream;
use std::fmt;

/// Represents the two types of syntax errors: the encountering of an unexpected
/// token, and the encountering of the end of the token stream when it is not
/// expected.
#[derive(Debug, PartialEq)]
pub enum Failure {
    UnexpectedToken(lexer::Token, &'static str),
    UnexpectedStreamEnd(&'static str),
    UnexpectedIndent { expected_indent: usize, encountered_indent: usize, pos: stream::Position }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::UnexpectedToken(tok, expected) => write!(f, "Expected {} yet encountered unexpected {}", expected, tok),
            Failure::UnexpectedStreamEnd(expected) => write!(f, "Encountered the end of the token stream yet expected {}", expected),
            Failure::UnexpectedIndent { expected_indent, encountered_indent, pos } =>
                write!(f, "Encountered an unexpected change in indentation from the expected level of {} to an indentation level of {} tabs at {}", expected_indent, encountered_indent, pos)
        }
    }
}

type Result<T> = std::result::Result<T, Failure>;

/// Represents a parsed till statement. An AST is comprised of a collection of
/// `Statement` instances.
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
        return_type: Option<String>,
        body: Block
    },

    VariableDeclaration {
        var_type: String,
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

/// Parameter for a function definition.
#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub param_type: String,
    pub identifier: String,
    pub pos: stream::Position
}

/// Represents a till expression.
#[derive(Debug, PartialEq)]
pub enum Expression {
    Equal(Box<Expression>, Box<Expression>),
    GreaterThan(Box<Expression>, Box<Expression>),
    LessThan(Box<Expression>, Box<Expression>),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    BooleanNot(Box<Expression>),
    UnaryMinus(Box<Expression>),

    NumberLiteral { pos: stream::Position, value: f64 },
    CharLiteral { pos: stream::Position, value: char },
    BooleanLiteral { pos: stream::Position, value: bool },
    Variable { pos: stream::Position, identifier: String },
    FunctionCall { pos: stream::Position, identifier: String, args: Vec<Expression> }
}