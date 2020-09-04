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

/// The final immediate representation of a till program before it is converted
/// into machine code.
#[derive(Debug)]
pub struct ProgramRepresentation {
    /// The primitive, assembly-like instructions that make up the input program.
    instructions: Vec<Instruction>,
    /// Contains all scopes of the program. This vector should only ever be added
    /// to. The relationship between scopes is only considered during the checking
    /// stage and so is not included as part of the final immediate representation.
    scopes: Vec<Scope>
}

impl ProgramRepresentation {
    fn new() -> Self {
        ProgramRepresentation {
            instructions: Vec::new(), scopes: Vec::new()
        }
    }

    fn create_scope(&mut self) -> usize {
        self.scopes.push(Scope { 
            variable_defs: Vec::new(),
            function_defs: Vec::new()
        });
        self.scopes.len() - 1
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

/// Definition of a variable with a given identifier and type.
#[derive(Debug, PartialEq)]
struct VariableDef {
    identifier: String,
    var_type: Type
}

impl VariableDef {
    fn new(ident: &str, var_type: Type) -> Self {
        VariableDef {
            identifier: ident.to_string(),
            var_type
        }
    }
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
enum Value {
    Variable { scope: usize, identifier: String },
    Constant(ConstValue)
}

#[derive(Debug, PartialEq)]
enum ConstValue {
    Num(f64), Char(char), Bool(bool), Array(Vec<ConstValue>)
}

/// Represents the simple, assembly-like instructions that make up the final
/// immediate representation of a till program.
#[derive(Debug)]
enum Instruction {
    Push(Value), // Push value at specified
    Pop(Value), // Pop value into specified
    Equals(Value), // Compare top of stack with specified
    Add(Value), // Pop top of stack, add to specified, push result
    Subtract(Value), // Pop top of stack, subtract from specified, push result
    Multiply(Value), // Pop top of stack, multiply with specified, push result
    Divide(Value), // Pop top of stack, divide by specified, push result
    Not, // Pop top of stack, if 0 then push 1, else push 0
    Call(Value), // Jump to specified, return here when Instruction::Return encountered
    Return // Return from call
}