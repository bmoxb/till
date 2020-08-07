pub mod checker;
use std::fmt;

pub struct FinalIR { /* ... */ }

#[derive(Debug, PartialEq)]
enum Type {
    Simple(SimpleType),
    Array(Box<Type>)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Simple(simp) => write!(f, "{:?}", simp),
            Type::Array(contained) => write!(f, "[{}]", contained)
        }
    }
}

#[derive(Debug, PartialEq)]
enum SimpleType { Char, Num, Bool }