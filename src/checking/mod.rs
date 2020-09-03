pub mod checker;
use std::{ fmt, cmp, mem };
use crate::parsing;

#[derive(Debug, PartialEq)]
pub enum Failure { // TODO: Show stream position in error messages.
    VariableNotInScope(String),
    FunctionNotInScope(String, Vec<Type>),
    VoidFunctionInExpr(String, Vec<Type>),
    RedefinedExistingFunction(String, Vec<Type>),
    VoidFunctionReturnsValue(String, Vec<Type>, Type),
    FunctionDoesNotReturn(String, Vec<Type>, Type),
    NonexistentPrimitiveType(String),
    RedeclaredToDifferentType { identifier: String, expected: Type, encountered: Type },
    UnexpectedType { expected: Type, encountered: Type }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::VariableNotInScope(ident) => write!(f, "Reference made to variable '{}' which is either undefined or inaccessible from the current scope", ident),
            Failure::FunctionNotInScope(ident, params) => write!(f, "Call made to function '{}' with parameter types {:?} which is either undefined or inaccessible from the current scope", ident, params),
            Failure::VoidFunctionInExpr(ident, params) => write!(f, "Function '{}' with parameter types {:?} has no return value and so cannot be used in an expression", ident, params),
            Failure::RedefinedExistingFunction(ident, params) => write!(f, "Function '{}' with parameter types {:?} has already been defined", ident, params),
            Failure::VoidFunctionReturnsValue(ident, params, ret_type) => write!(f, "Function '{}' with parameter types {:?} defined without return type yet has a block that returns a value of type {}", ident, params, ret_type),
            Failure::FunctionDoesNotReturn(ident, params, ret_type) => write!(f, "Function '{}' with parameter types {:?} expected to return a value of type {}", ident, params, ret_type),
            Failure::NonexistentPrimitiveType(ident) => write!(f, "The primitive type '{}' does not exist - please use either 'Num', 'Char' or 'Bool'", ident),
            Failure::RedeclaredToDifferentType { identifier, expected, encountered } => write!(f, "Attempt made to redeclare variable '{}' of type {} to different type {} in the same scope", identifier, expected, encountered),
            Failure::UnexpectedType { expected, encountered } => write!(f, "Expected type {} yet enountered {}", expected, encountered)
        }
    }
}

type Result<T> = std::result::Result<T, Failure>;

#[derive(Clone, Debug)]
pub enum Type {
    Array(Box<Type>),
    Char, Num, Bool,
    Any
}

impl Type {
    fn from_parsing_type(ptype: &parsing::Type) -> Result<Type> {
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
            Type::Any => write!(f, "???"),
            other => write!(f, "{:?}", other)
        }
    }
}

impl cmp::PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Array(left), Type::Array(right)) => *left == *right,
            (Type::Array(_), _) => false,
            (_, Type::Array(_)) => false,
            (Type::Any, _) => true,
            (_, Type::Any) => true,
            _ => mem::discriminant(self) == mem::discriminant(other)
        }
    }
}