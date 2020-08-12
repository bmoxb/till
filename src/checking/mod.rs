pub mod checker;
use std::fmt;

pub struct FinalIR { /* ... */ }

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Array(Box<Type>),
    Char, Num, Bool
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Array(contained) => write!(f, "[{}]", contained),
            other => write!(f, "{:?}", other)
        }
    }
}