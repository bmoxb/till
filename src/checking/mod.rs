pub mod checker;
use std::fmt;
use crate::parsing;

#[derive(Debug, PartialEq)]
pub enum Failure { // TODO: Show stream position in error messages.
    VariableNotInScope(String),
    FunctionNotInScope(String, Vec<Type>),
    VoidFunctionInExpr(String, Vec<Type>),
    NonexistentPrimitiveType(String),
    UnexpectedType { expected: Type, encountered: Type }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::VariableNotInScope(ident) => write!(f, "Reference made to variable with identifier `{}` which is either undefined and inaccessible from the current scope", ident),
            Failure::FunctionNotInScope(ident, params) => write!(f, "Call made to a function '{}' with parameters {:?} which is either undefined or inaccessible from the current scope", ident, params),
            Failure::VoidFunctionInExpr(ident, params) => write!(f, "Function '{}' with parameters {:?} has no return value and so cannot be used in an expression", ident, params),
            Failure::NonexistentPrimitiveType(ident) => write!(f, "The primitive type '{}' does not exist - please use either 'Num', 'Char' or 'Bool'", ident),
            Failure::UnexpectedType { expected, encountered } => write!(f, "Expected type {} yet enountered {}", expected, encountered)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Array(Box<Type>),
    Char, Num, Bool
}

impl Type {
    fn from_parsing_type(ptype: &parsing::Type) -> Result<Type, Failure> {
        match ptype {
            parsing::Type::Identifier { pos: _, identifier } => {
                match identifier.as_str() {
                    "Char" => Ok(Type::Char),
                    "Num" => Ok(Type::Num),
                    "Bool" => Ok(Type::Bool),
                    _ => Err(Failure::NonexistentPrimitiveType(identifier.clone()))
                }
            }

            parsing::Type::Array(contained) => Ok(Type::Array(Box::new(Type::from_parsing_type(contained)?)))
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Array(contained) => write!(f, "[{}]", contained),
            other => write!(f, "{:?}", other)
        }
    }
}