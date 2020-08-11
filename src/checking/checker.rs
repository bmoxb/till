use crate::parsing;
use std::fmt;

pub fn input<T: Iterator<Item=parsing::Statement>>(stmts: T) -> Checker<T> {
    let mut checker = Checker { stmts: stmts, scope_stack: Vec::new() };
    checker.begin_new_scope();
    checker
}

#[derive(Debug)]
pub enum Failure { // TODO: Show stream position in error messages.
    VariableNotInScope(String),
    TypeMismatch { expected: super::Type, encountered: super::Type }
}

impl fmt::Display for Failure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Failure::VariableNotInScope(identifier) => write!(f, "Reference made to variable with identifier `{}` which is either undefined and inaccessible from the current scope", identifier),
            Failure::TypeMismatch { expected, encountered } => write!(f, "Type mismatch encountered - expected type {} yet enountered {}", expected, encountered)
        }
    }
}

pub struct Checker<T: Iterator<Item=parsing::Statement>> {
    stmts: T,
    scope_stack: Vec<Scope>
}

impl<T: Iterator<Item=parsing::Statement>> Iterator for Checker<T> {
    type Item = Result<parsing::Statement, Failure>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.stmts.next() {
            Some(stmt) => {
                match self.check_stmt(&stmt) {
                    Err(e) => Some(Err(e)),
                    Ok(_) => Some(Ok(stmt))
                }
            }

            None => {
                log::trace!("Reached end of statement stream - ending program scope");
                self.end_scope();
                None
            }
        }
    }
}

impl<T: Iterator<Item=parsing::Statement>> Checker<T> {
    fn check_stmt(&mut self, stmt: &parsing::Statement) -> Result<(), Failure> {
        match stmt {
            parsing::Statement::If { condition, block } => {
                self.expect_expr_type(condition, super::Type::Simple(super::SimpleType::Bool))?;
                // handle block...
            }
            _ => unimplemented!()
        }
        Ok(()) // TODO: temp
    }

    fn begin_new_scope(&mut self) {
        self.scope_stack.push(Scope { 
            variable_defs: Vec::new(),
            function_defs: Vec::new()
        });
    }

    fn end_scope(&mut self) {
        self.scope_stack.pop();
    }

    /// Search the current accessible scopes for the variable definition with
    /// the given identifier.
    fn variable_lookup(&self, identifier: &str) -> Result<&VariableDef, Failure> {
        Err(Failure::VariableNotInScope(identifier.to_string())) // TODO: temp
    }

    fn identify_expr_type(&self, expr: &parsing::Expression) -> Result<super::Type, Failure> {
        match expr {
            parsing::Expression::Add(left, right) |
            parsing::Expression::Subtract(left, right) |
            parsing::Expression::Multiply(left, right) |
            parsing::Expression::Divide(left, right) => {
                log::trace!("Verifying types of arithmetic expression (Num type on both sides of operator expected)");

                self.expect_expr_type(left, super::Type::Simple(super::SimpleType::Num))?;
                self.expect_expr_type(right, super::Type::Simple(super::SimpleType::Num))?;

                Ok(super::Type::Simple(super::SimpleType::Num))
            }

            parsing::Expression::Equal(left, right) => {
                log::trace!("Verifying types of equality expression (types on both sides of the operator should be the same)");

                let left_type = self.identify_expr_type(left)?;
                let right_type = self.identify_expr_type(right)?;

                if left_type == right_type { Ok(left_type) }
                else {
                    Err(Failure::TypeMismatch {
                        expected: left_type,
                        encountered: right_type
                    })
                }
            }

            _ => unimplemented!()
        }
    }

    fn expect_expr_type(&self, expr: &parsing::Expression, expected: super::Type) -> Result<(), Failure> {
        let expr_type = self.identify_expr_type(expr)?;
        
        if expr_type == expected { Ok(()) }
        else { Err(Failure::TypeMismatch { expected, encountered: expr_type }) }
    }
}

struct Scope {
    variable_defs: Vec<VariableDef>,
    function_defs: Vec<FunctionDef>
}

struct VariableDef {
    identifier: String,
    var_type: parsing::Type
}

struct FunctionDef {
    identifier: String,
    return_type: Option<parsing::Type>
}

#[cfg(test)]
mod tests {}