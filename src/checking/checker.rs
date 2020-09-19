//! Contains code for the semantic analysis of a till AST and its conversion to
//! a final immediate representation of the input program.

use crate::{ stream, parsing };

pub fn input<T: Iterator<Item=parsing::Statement>>(stmts: T) -> super::Result<Vec<super::Instruction>> {
    Checker::new(stmts).execute()
}

/// Performs scoping and type checking on a stream of parsed statements. Yields
/// a final lower-level immediate representation of the input program.
pub struct Checker<T: Iterator<Item=parsing::Statement>> {
    /// Iterator of statements to be checked.
    stmts: T,
    /// The scope stack. The scope at the end of this vector is the inner most
    /// scope at a given point.
    scopes: Vec<super::Scope>,
    /// Holds the primitive instructions that will make up the final immediate
    /// representation of the input program.
    final_ir: Vec<super::Instruction>,
    /// When a scope ends, all the variable IDs it was previously using are stored
    /// in this vector so that then may be reused in a new scope.
    available_var_ids: Vec<super::Id>,
    /// Counter for creating unique IDs. Incremented each time a ID is required.
    id_counter: super::Id
}

impl<T: Iterator<Item=parsing::Statement>> Checker<T> {
    fn new(stmts: T) -> Self {
        Checker {
            stmts,
            scopes: Vec::new(),
            final_ir: Vec::new(),
            available_var_ids: Vec::new(),
            id_counter: 0
        }
    }

    /// Perform scoping and type checking before yielding the final immediate
    /// representation of the input program. This will consume the `Checker`
    /// instance.
    fn execute(mut self) -> super::Result<Vec<super::Instruction>> {
        self.begin_new_scope();
    
        while let Some(stmt) = self.stmts.next() {
            self.check_stmt(stmt)?;
        }

        log::info!("Reached end of statement stream - ending program scope");

        self.end_scope();
        assert!(self.scopes.is_empty());

        Ok(self.final_ir)
    }

    /// Check the validity of a given statement. May return a type in the case of
    /// the statement being a return statement or a while or if statement with a
    /// block containing a return statement.
    fn check_stmt(&mut self, stmt: parsing::Statement) -> super::Result<Option<super::Type>> {
        match stmt {
            parsing::Statement::Return(Some(expr)) => {
                let (value, _) = self.check_expr(expr)?;
                self.final_ir.push(super::Instruction::ReturnValue);
                Ok(Some(value))
            }
            parsing::Statement::Return(None) => {
                self.final_ir.push(super::Instruction::ReturnVoid);
                Ok(None)
            }

            parsing::Statement::While { condition, block } => {
                let block_end_id = self.new_id();
                self.final_ir.push(super::Instruction::Jump(block_end_id));

                let start_id = self.new_id();
                self.final_ir.push(super::Instruction::Label(start_id));

                let (block_ret_type, _) = self.check_block(block, vec![])?;
                self.final_ir.push(super::Instruction::Label(block_end_id));

                self.expect_expr_type(condition, super::Type::Bool)?;
                self.final_ir.push(super::Instruction::JumpIfTrue(start_id));

                Ok(block_ret_type)
            }

            parsing::Statement::If { condition, block } => {
                let skip_block_id = self.new_id();

                self.expect_expr_type(condition, super::Type::Bool)?;
                self.final_ir.push(super::Instruction::JumpIfFalse(skip_block_id));

                let (block_ret_type, _) = self.check_block(block, vec![])?;

                self.final_ir.push(super::Instruction::Label(skip_block_id));

                Ok(block_ret_type)
            }

            parsing::Statement::VariableDeclaration { var_type, identifier, value } => {
                let checking_type = super::Type::from_parsing_type(&var_type)?;

                let var_id = { 
                    // If variable is already defined then ensure it is being redeclared
                    // to the same type:
                    if let Some(existing_def) = self.get_inner_scope().find_variable_def(&identifier) {
                        log::trace!("Redeclaring variable '{}' in same scope", identifier);

                        if checking_type != existing_def.var_type {
                            return Err(super::Failure::RedeclaredToDifferentType {
                                identifier: identifier.to_string(),
                                expected: existing_def.var_type.clone(),
                                encountered: checking_type
                            });
                        }

                        existing_def.id
                    }
                    else {
                        log::trace!("Introducing variable '{}' to current scope", identifier);

                        self.introduce_variable_to_inner_scope(&identifier, checking_type.clone())
                    }
                };

                // Ensure initial value expression is of correct type:
                if let Some(initial_value) = value {
                    self.expect_expr_type(initial_value, checking_type)?;

                    // Store the initial value instruction:
                    self.final_ir.push(super::Instruction::Store(var_id));
                }

                Ok(None)
            }

            parsing::Statement::VariableAssignment { identifier, assign_to } => {
                let var_id = {
                    let (assign_to_type, strm_pos) = self.check_expr(assign_to)?;
                    
                    let var_def = self.variable_lookup(&identifier, &strm_pos)?;

                    if var_def.var_type != assign_to_type {
                        return Err(super::Failure::UnexpectedType {
                            encountered: assign_to_type,
                            expected: var_def.var_type.clone()
                        });
                    }

                    var_def.id
                };

                self.final_ir.push(super::Instruction::Store(var_id));

                Ok(None)
            }

            parsing::Statement::FunctionDefinition { pos, identifier, parameters, return_type, body } => {
                // Function body should only run when function is called so skip
                // over the body:
                let end_id = self.new_id();
                self.final_ir.push(super::Instruction::Jump(end_id));

                // Add the function instruction before the function body:
                let start_id = self.new_id();
                self.final_ir.push(super::Instruction::Function(start_id));

                let (optional_body_return_type, param_types) = self.check_block(body, parameters)?;

                // Introduce the end label after the function body:
                self.final_ir.push(super::Instruction::Label(end_id));

                // Is a function with the same identifier and type signature
                // defined and accessible from this scope?
                if self.function_lookup(&identifier, param_types.as_slice(), &pos).is_ok() {
                    Err(super::Failure::RedefinedExistingFunction(identifier.to_string(), param_types.to_vec()))
                }
                else {
                    // Return type specified in function signature:
                    if let Some(parsing_return_type) = return_type {
                        let expected_return_type = super::Type::from_parsing_type(&parsing_return_type)?;

                        // Function body should return something if a return type
                        // has been specified in the signature:
                        if let Some(body_return_type) = optional_body_return_type {
                            // Are those types the same?
                            if body_return_type == expected_return_type {
                                self.introduce_function(identifier, param_types, Some(body_return_type), start_id);
                                Ok(None)
                            }
                            else {
                                Err(super::Failure::UnexpectedType {
                                    encountered: body_return_type,
                                    expected: expected_return_type
                                })
                            }
                        }
                        else {
                            return Err(super::Failure::FunctionDoesNotReturn(
                                identifier, param_types.to_vec(),
                                expected_return_type
                            ));
                        }
                    } // No return type specified in signature:
                    else {
                        // Does function body return something?
                        if let Some(body_return_type) = optional_body_return_type {
                            Err(super::Failure::VoidFunctionReturnsValue(
                                identifier, param_types.to_vec(),
                                body_return_type
                            ))
                        }
                        else {
                            self.introduce_function(identifier, param_types, None, start_id);
                            Ok(None)
                        }
                    }
                }
            }
        }
    }

    /// Iterate over the statements contained in a block, checking each. Should
    /// a return statement be encountered, the type of the returned expression
    /// is returned within `Ok(Some(...), ...)`. If there are multiple return
    /// statements then it will be ensured that they are all returning the same type.
    fn check_block(&mut self, block: parsing::Block, params: Vec<parsing::Parameter>) -> super::Result<(Option<super::Type>, Vec<super::Type>)> {
        let mut ret_type = None;

        self.begin_new_scope();

        let mut param_types = Vec::new();
        for (i, param) in params.iter().enumerate() {
            let converted_type = super::Type::from_parsing_type(&param.param_type)?;

            let var_id = self.introduce_variable_to_inner_scope(&param.identifier, converted_type.clone());

            // Function arguments are assumed to be placed on the stack before
            // the function is called, so store those values in the parameter
            // variables:
            self.final_ir.push(super::Instruction::Parameter {
                store_in: var_id,
                param_number: (params.len() - 1) - i // Added in reverse.
            });

            param_types.push(converted_type);
        }

        for stmt in block {
            if let Some(new) = self.check_stmt(stmt)? {
                // Has a return type already been established for this block?
                if let Some(current) = &ret_type {
                    if new != *current { // Can't have return statements with different types!
                        return Err(super::Failure::UnexpectedType {
                            expected: current.clone(),
                            encountered: new
                        })
                    }
                }
                else { ret_type.replace(new); }
            }
        }

        self.end_scope();

        Ok((ret_type, param_types))
    }

    /// Introduce a new, inner-most scope which is added to the end of the scope
    /// stack.
    fn begin_new_scope(&mut self) {
        self.scopes.push(super::Scope {
            variable_defs: Vec::new(),
            function_defs: Vec::new()
        });
    }

    /// Remove the inner-most scope from the scopes stack. Will also insert an
    /// end scope instruction into the final IR.
    fn end_scope(&mut self) {
        if let Some(old_scope) = self.scopes.pop() {
            let newly_available_ids = old_scope.variable_defs.iter().map(|x| x.id);
            self.available_var_ids.extend(newly_available_ids);
        }
    }

    /// Get a mutable reference to the current inner-most scope. Will panic if
    /// the scope stack is empty.
    fn get_inner_scope(&mut self) -> &mut super::Scope {
        self.scopes.last_mut().unwrap()
    }

    /// Search the current accessible scopes for the variable definition with
    /// the given identifier.
    fn variable_lookup(&self, ident: &str, strm_pos: &stream::Position) -> super::Result<&super::VariableDef> {
        // Reverse the iterator so that the inner most scope has priority (i.e.
        // automatically handle shadowing).
        for scope in self.scopes.iter().rev() {
            if let Some(var_def) = scope.find_variable_def(ident) {
                return Ok(var_def)
            }
        }
        Err(super::Failure::VariableNotInScope(strm_pos.clone(), ident.to_string()))
    }

    /// Introduce a new variable into the current inner most scope. Will also
    /// insert final IR instruction to allocate space for this new variable.
    fn introduce_variable_to_inner_scope(&mut self, ident: &str, var_type: super::Type) -> super::Id {
        let id = {
            if let Some(unused_id) = self.available_var_ids.pop() { unused_id }
            else {
                let new_id = self.new_id();
                self.final_ir.push(super::Instruction::Allocate(new_id));
                new_id
            }
        };
        
        self.get_inner_scope().variable_defs.push(super::VariableDef {
            var_type, identifier: ident.to_string(), id
        });

        id
    }

    fn function_lookup(&self, ident: &str, params: &[super::Type], strm_pos: &stream::Position) -> super::Result<&super::FunctionDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(func_def) = scope.find_function_def(ident, params) {
                return Ok(func_def)
            }
        }
        Err(super::Failure::FunctionNotInScope(strm_pos.clone(), ident.to_string(), params.to_vec()))
    }

    fn introduce_function(&mut self, ident: String, params: Vec<super::Type>, return_type: Option<super::Type>, start_id: super::Id) {
        self.get_inner_scope().function_defs.push(super::FunctionDef {
            identifier: ident, parameter_types: params,
            id: start_id, return_type
        });
    }

    fn new_id(&mut self) -> super::Id {
        let id = self.id_counter;
        self.id_counter += 1;

        id
    }

    /// Check the validity of a given expression as well as insert the appropriate
    /// instructions into the final IR.
    fn check_expr(&mut self, expr: parsing::Expression) -> super::Result<(super::Type, stream::Position)> {
        match expr {
            parsing::Expression::Variable { pos, identifier } => {
                log::trace!("Searching scope for the type of referenced variable with identifier '{}'", identifier);

                let (var_type, id) = {
                    let def = self.variable_lookup(&identifier, &pos)?;
                    (def.var_type.clone(), def.id)
                };

                self.final_ir.push(super::Instruction::Push(super::Value::Variable(id)));

                Ok((var_type, pos))
            }

            parsing::Expression::FunctionCall {pos, identifier, args } => {
                log::trace!("Searching scope for the return type of referenced function '{}' given arguments {:?}", identifier, args);

                let mut arg_types = Vec::new();
                for arg in args {
                    let (arg_type, _) = self.check_expr(arg)?;
                    arg_types.push(arg_type); 
                }

                let (ident, option_ret_type, id) = {
                    let def = self.function_lookup(&identifier, arg_types.as_slice(), &pos)?;
                    (def.identifier.clone(), def.return_type.clone(), def.id)
                };

                self.final_ir.push(
                    if option_ret_type.is_some() { super::Instruction::CallExpectingValue(id) }
                    else { super::Instruction::CallExpectingVoid(id) }
                );

                match option_ret_type {
                    Some(ret_type) => Ok((ret_type.clone(), pos)),
                    None => Err(super::Failure::VoidFunctionInExpr(pos, ident, arg_types))
                }
            }

            parsing::Expression::Add(l, r) =>
                Ok((super::Type::Num, self.verify_arithmetic_expr(*l, *r, super::Instruction::Add, "addition")?)),

            parsing::Expression::Subtract(l, r) =>
                Ok((super::Type::Num, self.verify_arithmetic_expr(*l, *r, super::Instruction::Subtract, "subtraction")?)),

            parsing::Expression::Multiply(l, r) =>
                Ok((super::Type::Num, self.verify_arithmetic_expr(*l, *r, super::Instruction::Multiply, "multiplication")?)),

            parsing::Expression::Divide(l, r) =>
                Ok((super::Type::Num, self.verify_arithmetic_expr(*l, *r, super::Instruction::Divide, "divide")?)),

            parsing::Expression::GreaterThan(l, r) =>
                Ok((super::Type::Bool, self.verify_arithmetic_expr(*l, *r, super::Instruction::GreaterThan, "greater than")?)),

            parsing::Expression::LessThan(l, r) =>
                Ok((super::Type::Bool, self.verify_arithmetic_expr(*l, *r, super::Instruction::LessThan, "less than")?)),

            parsing::Expression::Equal(left, right) => {
                log::trace!("Verifying types of equality expression - types on both sides of the operator should be the same");

                let (left_type, strm_pos) = self.check_expr(*left)?;
                let (right_type, _) = self.check_expr(*right)?;

                if left_type == right_type {
                    self.final_ir.push(super::Instruction::Equals);

                    Ok((super::Type::Bool, strm_pos))
                }
                else {
                    Err(super::Failure::UnexpectedType {
                        expected: left_type,
                        encountered: right_type
                    })
                }
            }

            parsing::Expression::BooleanNot(expr) => {
                log::trace!("Verifying type of expression to which boolean NOT operator is being applied - expecting Bool expression to right of operator");

                let strm_pos = self.expect_expr_type(*expr, super::Type::Bool)?;
                self.final_ir.push(super::Instruction::Not);

                Ok((super::Type::Bool, strm_pos))
            }

            parsing::Expression::UnaryMinus(expr) => {
                log::trace!("Verify type of expression to which unary minus is being applied - expecting Num");

                self.final_ir.push(super::Instruction::Push(super::Value::Num(0.0)));
                let strm_pos = self.expect_expr_type(*expr, super::Type::Num)?;
                self.final_ir.push(super::Instruction::Subtract);

                Ok((super::Type::Num, strm_pos))
            }

            parsing::Expression::NumberLiteral {pos, value } => {
                self.final_ir.push(super::Instruction::Push(super::Value::Num(value)));

                Ok((super::Type::Num, pos))
            }
            parsing::Expression::BooleanLiteral { pos, value } => {
                self.final_ir.push(super::Instruction::Push(super::Value::Bool(value)));

                Ok((super::Type::Bool, pos))
            }
            parsing::Expression::CharLiteral { pos, value } => {
                self.final_ir.push(super::Instruction::Push(super::Value::Char(value)));

                Ok((super::Type::Char, pos))
            }
        }
    }

    /// Ensure the two sub-expressions of an arithmetic expression are both of
    /// Num type. Insert the relevant final IR instruction also.
    fn verify_arithmetic_expr(&mut self, left: parsing::Expression, right: parsing::Expression, instruction: super::Instruction, expr_type: &str) -> super::Result<stream::Position> {
        log::trace!("Verifying types of {} expression - Num type on both sides of operator expected", expr_type);

        let strm_pos = self.expect_expr_type(left, super::Type::Num)?;
        self.expect_expr_type(right, super::Type::Num)?;

        self.final_ir.push(instruction);

        Ok(strm_pos)
    }

    fn expect_expr_type(&mut self, expr: parsing::Expression, expected: super::Type) -> super::Result<stream::Position> {
        let (expr_type, strm_pos) = self.check_expr(expr)?;
        
        if expr_type == expected { Ok(strm_pos) }
        else { Err(super::Failure::UnexpectedType { expected, encountered: expr_type }) }
    }
}



#[cfg(test)]
mod tests {
    use std::iter;
    use crate::{ assert_pattern, parsing, checking, stream::Position };

    fn new_empty_checker() -> super::Checker<iter::Empty<parsing::Statement>> {
        let mut chkr = super::Checker::new(iter::empty());
        chkr.begin_new_scope();
        chkr
    }

    #[test]
    fn scoping() {
        let mut chkr = new_empty_checker();

        let pos = Position::new();

        chkr.introduce_variable_to_inner_scope("outer", checking::Type::Num);
        assert_eq!(chkr.variable_lookup("outer", &pos), Ok(&checking::VariableDef {
            identifier: "outer".to_string(),
            var_type: checking::Type::Num,
            id: 0
        }));

        chkr.begin_new_scope();

        chkr.introduce_variable_to_inner_scope("inner", checking::Type::Bool);

        assert!(chkr.variable_lookup("inner", &pos).is_ok());
        assert!(chkr.variable_lookup("outer", &pos).is_ok());

        chkr.end_scope();

        assert!(chkr.variable_lookup("inner", &pos).is_err());
        assert!(chkr.variable_lookup("outer", &pos).is_ok());
        assert!(chkr.variable_lookup("undefined", &pos).is_err());

        chkr.introduce_function("xyz".to_string(), vec![checking::Type::Char], Some(checking::Type::Num), 0);
        
        assert_eq!(chkr.function_lookup("xyz", &[checking::Type::Char], &pos), Ok(&checking::FunctionDef {
            identifier: "xyz".to_string(),
            parameter_types: vec![checking::Type::Char],
            return_type: Some(checking::Type::Num), id: 0
        }));

        assert!(chkr.function_lookup("xyz", &[checking::Type::Num], &pos).is_err());
    }

    #[test]
    fn check_exprs() {
        let mut chkr = new_empty_checker();

        assert_pattern!(
            chkr.check_expr(parsing::Expression::NumberLiteral { pos: Position::new(), value: 10.5 }),
            Ok((checking::Type::Num, _))
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::BooleanLiteral { pos: Position::new(), value: true }),
            Ok((checking::Type::Bool, _))
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::CharLiteral { pos: Position::new(), value: '話' }),
            Ok((checking::Type::Char, _))
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::Equal(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }),
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'y' })
            )),
            Ok((checking::Type::Bool, _))
        );

        assert_eq!(
            chkr.check_expr(parsing::Expression::Equal(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.5 }),
                Box::new(parsing::Expression::BooleanLiteral { pos: Position::new(), value: false })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Bool,
                expected: checking::Type::Num
            })
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::GreaterThan(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.34 }),
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 0.95 })
            )),
            Ok((checking::Type::Bool, _))
        );

        assert_eq!(
            chkr.check_expr(parsing::Expression::LessThan(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'b' }),
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'a' })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Char,
                expected: checking::Type::Num
            })
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::Add(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 10.0 }),
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 11.2 })
            )),
            Ok((checking::Type::Num, _))
        );

        assert_eq!(
            chkr.check_expr(parsing::Expression::Divide(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }),
                Box::new(parsing::Expression::BooleanLiteral { pos: Position::new(), value: false })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Char,
                expected: checking::Type::Num
            })
        );

        assert_pattern!(
            chkr.check_expr(parsing::Expression::Variable {
                pos: Position::new(),
                identifier: "undefined".to_string()
            }),
            Err(checking::Failure::VariableNotInScope(_, _))
        );

        chkr.introduce_variable_to_inner_scope("var", checking::Type::Num);

        chkr.begin_new_scope();
        assert_pattern!(
            chkr.check_expr(parsing::Expression::Variable {
                pos: Position::new(),
                identifier: "var".to_string()
            }),
            Ok((checking::Type::Num, _))
        );
        chkr.end_scope();

        chkr.introduce_function("func".to_string(), vec![], Some(checking::Type::Num), 0);

        assert_pattern!(
            chkr.check_expr(parsing::Expression::FunctionCall {
                pos: Position::new(),
                identifier: "func".to_string(),
                args: vec![]
            }),
            Ok((checking::Type::Num, _))
        );

        match chkr.check_expr(parsing::Expression::FunctionCall {
            pos: Position::new(),
            identifier: "func".to_string(),
            args: vec![
                parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.5 }
            ]
        }) {
            Err(checking::Failure::FunctionNotInScope(_, ident, args)) => {
                assert_eq!(ident, "func".to_string());
                assert_eq!(args, vec![checking::Type::Num]);
            }
            _ => panic!()
        }

        chkr.introduce_function("abc".to_string(), vec![checking::Type::Char], None, 1);

        assert_pattern!(
            chkr.check_expr(parsing::Expression::FunctionCall {
                pos: Position::new(),
                identifier: "abc".to_string(),
                args: vec![
                    parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }
                ]
            }),
            Err(checking::Failure::VoidFunctionInExpr(_, _, _))
        );
    }

    #[test]
    fn check_stmts() -> checking::Result<()> {
        let mut chkr = new_empty_checker();

        assert_eq!(
            chkr.check_stmt(parsing::Statement::Return(None)),
            Ok(None)
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::Return(Some(
                parsing::Expression::Add(
                    Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.2 }),
                    Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 2.8 })
                )
            ))),
            Ok(Some(checking::Type::Num))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::If {
                condition: parsing::Expression::BooleanLiteral { pos: Position::new(), value: true },
                block: vec![
                    parsing::Statement::Return(Some(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }))
                ]
            }),
            Ok(Some(checking::Type::Char))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::VariableDeclaration {
                identifier: "pi".to_string(),
                var_type: "Num".to_string(),
                value: Some(parsing::Expression::NumberLiteral { pos: Position::new(), value: 3.14 })
            }),
            Ok(None)
        );
        assert!(chkr.variable_lookup("pi", &Position::new()).is_ok());

        assert_eq!(
            chkr.check_stmt(parsing::Statement::VariableDeclaration {
                identifier: "xyz".to_string(),
                var_type: "Oops".to_string(),
                value: None
            }),
            Err(checking::Failure::NonexistentPrimitiveType("Oops".to_string()))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::VariableAssignment {
                identifier: "pi".to_string(),
                assign_to: parsing::Expression::NumberLiteral { pos: Position::new(), value: 3.1 }
            }),
            Ok(None)
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::VariableAssignment {
                identifier: "pi".to_string(),
                assign_to: parsing::Expression::BooleanLiteral { pos: Position::new(), value: true }
            }),
            Err(checking::Failure::UnexpectedType {
                expected: checking::Type::Num,
                encountered: checking::Type::Bool
            })
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::FunctionDefinition {
                identifier: "func".to_string(),
                parameters: vec![],
                return_type: None,
                body: vec![],
                pos: Position::new()
            }),
            Ok(None)
        );
        assert!(chkr.function_lookup("func", &[], &Position::new())?.return_type.is_none());

        assert_eq!(
            chkr.check_stmt(parsing::Statement::FunctionDefinition {
                identifier: "func".to_string(),
                parameters: vec![],
                return_type: Some("Num".to_string()),
                body: vec![
                    parsing::Statement::Return(Some(parsing::Expression::NumberLiteral {
                        pos: Position::new(), value: 1.5
                    }))
                ],
                pos: Position::new()
            }),
            Err(checking::Failure::RedefinedExistingFunction(
                "func".to_string(), vec![]
            ))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::FunctionDefinition {
                identifier: "func".to_string(),
                parameters: vec![
                    parsing::Parameter {
                        pos: Position::new(), identifier: "x".to_string(),
                        param_type: "Char".to_string()
                    }
                ],
                return_type: Some("Num".to_string()),
                body: vec![],
                pos: Position::new()
            }),
            Err(checking::Failure::FunctionDoesNotReturn(
                "func".to_string(), vec![checking::Type::Char],
                checking::Type::Num
            ))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::FunctionDefinition {
                identifier: "xyz".to_string(),
                parameters: vec![],
                return_type: None,
                body: vec![
                    parsing::Statement::Return(Some(parsing::Expression::BooleanLiteral {
                        pos: Position::new(), value: true
                    }))
                ],
                pos: Position::new()
            }),
            Err(checking::Failure::VoidFunctionReturnsValue(
                "xyz".to_string(), vec![], checking::Type::Bool
            ))
        );

        assert_eq!(
            chkr.check_stmt(parsing::Statement::FunctionDefinition {
                identifier: "useless_function".to_string(),
                parameters: vec![
                    parsing::Parameter {
                        pos: Position::new(), identifier: "x".to_string(),
                        param_type: "Num".to_string()
                    }
                ],
                return_type: Some("Num".to_string()),
                body: vec![
                    parsing::Statement::Return(Some(parsing::Expression::Variable {
                        pos: Position::new(), identifier: "x".to_string()
                    }))
                ],
                pos: Position::new()
            }),
            Ok(None)
        );

        Ok(())
    }

    #[test]
    fn variable_shadowing() -> checking::Result<()> {
        let mut chkr = new_empty_checker();

        let pos = Position::new();

        chkr.check_stmt(parsing::Statement::VariableDeclaration {
            identifier: "x".to_string(),
            var_type: "Num".to_string(),
            value: None
        })?;

        chkr.begin_new_scope();

        // Shadow variable 'x' by declaring a variable in the inner scope of the
        // same name but a different type:
        chkr.check_stmt(parsing::Statement::VariableDeclaration {
            identifier: "x".to_string(),
            var_type: "Bool".to_string(),
            value: None
        })?;

        assert_eq!(chkr.variable_lookup("x", &pos)?.var_type, checking::Type::Bool);

        chkr.end_scope();

        assert_eq!(chkr.variable_lookup("x", &pos)?.var_type, checking::Type::Num);

        assert_eq!(
            chkr.check_stmt(parsing::Statement::VariableDeclaration {
                identifier: "x".to_string(),
                var_type: "Char".to_string(),
                value: None
            }),
            Err(checking::Failure::RedeclaredToDifferentType {
                identifier: "x".to_string(),
                expected: checking::Type::Num,
                encountered: checking::Type::Char
            })
        );

        Ok(())
    }
}