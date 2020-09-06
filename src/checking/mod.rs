//! Contains structures and enumerations used during the checking of scoping and
//! typing rules, as well as ones for representing the final immediate representation
//! of a till program. For the actual checking code, see submodule `checker`.

pub mod checker;

use crate::parsing;
use std::{ fmt, cmp, mem };

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

/// Represents the types available in till. `Char`, `Num`, and `Bool` are
/// self-explanatory primitive types, `Array` is a type indiciating a collection
/// of elements of a given type, `Any` will match with any primitive type but
/// not with an array type.
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

/// Represents a scope within a till program. A new scope is created in the body
/// of a function definition, if statement, or while statement. Any variables
/// or functions declared in a given scope will only be accessible from within
/// that scope or from a scope nested in it.
#[derive(Debug)]
struct Scope {
    variable_defs: Vec<VariableDef>,
    function_defs: Vec<FunctionDef>
}

impl Scope {
    fn find_variable_def(&self, ident: &str) -> Option<&VariableDef> {
        for def in &self.variable_defs {
            if def.identifier == ident { return Some(def) }
        }
        None
    }

    fn find_function_def(&self, ident: &str, params: &[Type]) -> Option<&FunctionDef> {
        for def in &self.function_defs {
            if def.identifier == ident && def.parameter_types.as_slice() == params {
                return Some(def)
            }
        }
        None
    }
}

type VarId = usize;

/// Definition of a variable with a given identifier and type.
#[derive(Debug, PartialEq)]
struct VariableDef {
    identifier: String,
    var_type: Type,
    id: VarId
}

/// Definition of a function with an identifier, set of parameters, and a return
/// type.
#[derive(Debug, PartialEq)]
struct FunctionDef {
    identifier: String,
    parameter_types: Vec<Type>,
    return_type: Option<Type>
}

#[derive(Debug, PartialEq)]
pub enum Value {
    /// Value is determined by that of the variable with the specified ID.
    Variable(VarId),
    Num(f64),
    Char(char),
    Bool(bool),
    /// The argument indicates the size of the array - that number of items at
    /// the top of the stack are considered the elements of this array.
    Array(usize)
}

/// Represents the simple, assembly-like instructions that make up the final
/// immediate representation of a till program.
#[derive(Debug)]
pub enum Instruction {
    Allocate(VarId),
    Deallocate(VarId),
    /// Push the specified value onto the stack.
    Push(Value),
    /// Pop a value off the stack and store it in the specified location.
    Store(VarId),
    Equals,
    GreaterThan,
    LessThan,
    Add, // Pop top of stack, add to specified, push result
    Subtract, // Pop top of stack, subtract from specified, push result
    Multiply, // Pop top of stack, multiply with specified, push result
    Divide, // Pop top of stack, divide by specified, push result
    Not, // Pop top of stack, perform boolean not and push result
    Call(Value), // Jump to specified, return here when Instruction::Return encountered
    /// Return from call, returning value on top of stack.
    ReturnValue,
    /// Return from call without including a value.
    ReturnVoid
}