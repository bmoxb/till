//! Contains code for the semantic analysis of a till AST and its conversion to
//! a final immediate representation of the input program.

use crate::{ stream, parsing };
//use std::collections::HashMap;

pub fn input<T: Iterator<Item=parsing::Statement>>(stmts: T) -> super::Result<Vec<super::Instruction>> {
    Checker::new(stmts).execute()
}

/// Performs scoping and type checking on a stream of parsed statements. Yields
/// a final lower-level immediate representation of the input program.
pub struct Checker<T: Iterator<Item=parsing::Statement>> {
    /// Iterator of statements to be checked.
    stmts: T,
    /// Contains all function definitions.
    functions: Vec<super::FunctionDef>,
    /// The scope stack. The scope at the end of this vector is the inner most
    /// scope at a given point.
    scopes: Vec<super::Scope>,
    /// Counter for creating unique IDs.
    id_counter: super::Id,
    /// IDs of local variables that are no longer used (i.e. went out of scope).
    available_local_variable_ids: Vec<super::Id>,
    /// Has the main function been defined?
    main_defined: bool
}

impl<T: Iterator<Item=parsing::Statement>> Checker<T> {
    fn new(stmts: T) -> Self {
        Checker {
            stmts,
            //global_variables: HashMap::new(),
            functions: Vec::new(),
            scopes: Vec::new(),
            id_counter: 0,
            available_local_variable_ids: Vec::new(),
            main_defined: false
        }
    }

    /// Perform scoping and type checking before yielding the final immediate
    /// representation of the input program. This will consume the `Checker`
    /// instance.
    fn execute(mut self) -> super::Result<Vec<super::Instruction>> {
        // Holds the primitive instructions that will make up the final immediate
        // representation of the input program.
        let mut final_ir = Vec::new();

        // Evaluate top-level statements:
        while let Some(stmt) = self.stmts.next() {
            let new_instructions = self.eval_top_level_stmt(stmt)?;
            final_ir.extend(new_instructions);
        }

        assert!(self.scopes.is_empty());

        if self.main_defined { Ok(final_ir) }
        else { Err(super::Failure::MainUndefined) }
    }

    /// Ensure the validity and evaluate a top-level statement (function or global
    /// variable definitions expected).
    fn eval_top_level_stmt(&mut self, stmt: parsing::Statement) -> super::Result<Vec<super::Instruction>> {
        match stmt {
            parsing::Statement::FunctionDefinition { pos, identifier, parameters, return_type, body } => {
                // Create a label for this function ("main" if the main function,
                // "func" followed by a new ID otherwise):
                let label = {
                    if identifier == "main" && parameters.is_empty() {
                        self.main_defined = true;
                        identifier.clone()
                    }
                    else { format!("func{}", self.new_id()) }
                };

                // Check the declared return type is actually a real type:
                let checked_return_type = return_type.map(|x| super::Type::from_identifier(&x)).transpose()?;

                let mut param_types = Vec::new();
                for param in parameters.iter() {
                    param_types.push(super::Type::from_identifier(&param.param_type)?);
                }
                let checked_parameters = parameters.into_iter().map(|x| x.identifier).zip(param_types.clone().into_iter()).collect();

                // Check if the function already exists:
                if self.function_lookup(&identifier, param_types.as_slice(), &pos).is_ok() {
                    return Err(super::Failure::RedefinedExistingFunction(identifier.to_string(), param_types.to_vec()))
                }
                else {
                    // Create the function definition before evaluating the body
                    // so as to allow recursion:
                    self.add_function_def(identifier.clone(), param_types.clone(), checked_return_type.clone(), label.clone());
                }

                // Evaluate the function body:
                let (body_instructions, local_variable_count, optional_body_return_type) = self.eval_block(body, checked_parameters)?;

                let mut instructions = vec![super::Instruction::Function { label, local_variable_count }];
                instructions.extend(body_instructions);

                // Return type specified in function signature:
                if let Some(expected_return_type) = checked_return_type {
                    // Function body should return something if a return type
                    // has been specified in the signature:
                    if let Some(body_return_type) = optional_body_return_type {
                        // Are those types the same?
                        if body_return_type == expected_return_type { Ok(instructions) }
                        else {
                            Err(super::Failure::FunctionUnexpectedReturnType {
                                pos, identifier, params: param_types.to_vec(),
                                expected: expected_return_type,
                                encountered: Some(body_return_type)
                            })
                        }
                    } // Function body doesn't return anything:
                    else {
                        return Err(super::Failure::FunctionUnexpectedReturnType {
                            pos, identifier, params: param_types.to_vec(),
                            expected: expected_return_type, encountered: None
                        });
                    }
                } // No return type specified in signature:
                else {
                    // Does function body return something?
                    if let Some(body_return_type) = optional_body_return_type {
                        Err(super::Failure::VoidFunctionReturnsValue(
                            pos, identifier, param_types.to_vec(),
                            body_return_type
                        ))
                    }
                    else {
                        // Ensure function has final return statement:
                        if instructions.last() != Some(&super::Instruction::ReturnVoid) {
                            instructions.push(super::Instruction::ReturnVoid);
                        }

                        Ok(instructions)
                    }
                }
            }

            _ => Err(super::Failure::InvalidTopLevelStatement)
        }
    }

    /// Check the validity of a given statement within a function. Returns a
    /// `Result` containing a tuple. The first item in this tuple will be a
    /// vector containing final IR instructions. The second item will be the
    /// number of local variables declared by the given statement. The final
    /// item in the tuple will ve a return type and stream position should
    /// the statement be a return statement or an if or while statement with a
    /// block containing a return statement.
    fn eval_inner_stmt(&mut self, stmt: parsing::Statement) -> super::Result<(Vec<super::Instruction>, usize, Option<(super::Type, stream::Position)>)> {
        match stmt {
            parsing::Statement::Return(Some(expr)) => {
                let (mut instructions, ret_type, pos) = self.eval_expr(expr)?;
                instructions.push(super::Instruction::ReturnValue);
                Ok((instructions, 0, Some((ret_type, pos))))
            }
            parsing::Statement::Return(None) =>
                Ok((vec![super::Instruction::ReturnVoid], 0, None)),

            parsing::Statement::Display(expr) => {
                let (mut instructions, value_type, pos) = self.eval_expr(expr)?;
                instructions.push(super::Instruction::Display {
                    value_type, line_number: pos.line_number
                });
                Ok((instructions, 0, None))
            }

            parsing::Statement::While { condition, block } => {
                let block_end_id = self.new_id();
                let start_id = self.new_id();
                
                let mut instructions = vec![
                    super::Instruction::Jump(block_end_id),
                    super::Instruction::Label(start_id)
                ];

                let (block_instructions, block_locals_count, block_ret_type) = self.eval_block(block, vec![])?;
                instructions.extend(block_instructions);
                instructions.push(super::Instruction::Label(block_end_id));

                let (condition_instructions, pos) = self.expect_expr_type(condition, super::Type::Bool)?;
                instructions.extend(condition_instructions);
                instructions.push(super::Instruction::JumpIfTrue(start_id));

                Ok((
                    instructions, block_locals_count,
                    if let Some(ret_type) = block_ret_type { Some((ret_type, pos)) }
                    else { None }
                ))
            }

            parsing::Statement::If { condition, block } => {
                let skip_block_id = self.new_id();

                let (mut instructions, pos) = self.expect_expr_type(condition, super::Type::Bool)?;
                instructions.push(super::Instruction::JumpIfFalse(skip_block_id));

                let (block_instructions, block_locals_count, block_ret_type) = self.eval_block(block, vec![])?;
                instructions.extend(block_instructions);

                instructions.push(super::Instruction::Label(skip_block_id));

                Ok(
                    if let Some(ret_type) = block_ret_type { (instructions, block_locals_count, Some((ret_type, pos))) }
                    else { (instructions, block_locals_count, None) }
                )
            }

            parsing::Statement::VariableDeclaration { var_type, identifier, value } => {
                let checked_type = super::Type::from_identifier(&var_type)?;
                let mut local_variable_count = 0;
                let mut instructions = Vec::new();

                let var_id = {
                    // If variable is already defined in this same scope then
                    // ensure it is being redeclared to the same type:
                    if let Some(existing_def) = self.get_inner_scope().find_variable_def(&identifier) {
                        log::trace!("Redeclaring variable '{}' in same scope", identifier);

                        if checked_type != existing_def.var_type {
                            return Err(
                                super::Failure::VariableRedeclaredToDifferentType {
                                    identifier: identifier.to_string(),
                                    expected: existing_def.var_type.clone(),
                                    encountered: checked_type
                                }
                            );
                        }

                        existing_def.id
                    }
                    else {
                        log::trace!("Introducing variable '{}' to current scope", identifier);

                        let id = self.add_variable_def_to_inner_scope(identifier, checked_type.clone());
                        
                        instructions.push(super::Instruction::Local(id));
                        local_variable_count = 1;

                        id
                    }
                };

                // Ensure initial value expression is of correct type:
                if let Some(initial_value) = value {
                    let (value_instructions, _) = self.expect_expr_type(initial_value, checked_type)?;
                    instructions.extend(value_instructions);

                    // Store the initial value in the variable:
                    instructions.push(super::Instruction::Store(var_id));
                }

                Ok((instructions, local_variable_count, None))
            }

            parsing::Statement::VariableAssignment { identifier, assign_to } => {
                let mut instructions = Vec::new();

                let var_id = {
                    let (expr_instructions, assign_to_type, strm_pos) = self.eval_expr(assign_to)?;
                    instructions.extend(expr_instructions);
                    
                    let var_def = self.variable_lookup(&identifier, &strm_pos)?;

                    if var_def.var_type != assign_to_type {
                        return Err(super::Failure::UnexpectedType {
                            pos: strm_pos,
                            encountered: assign_to_type,
                            expected: var_def.var_type.clone()
                        });
                    }

                    var_def.id
                };

                instructions.push(super::Instruction::Store(var_id));

                // A variable assignment modifies a previously declared local
                // variable so does not increase the local variable count:
                Ok((instructions, 0, None))
            }

            parsing::Statement::FunctionDefinition { pos, identifier, parameters: _, return_type: _, body: _ } =>
                Err(super::Failure::NestedFunctions(pos, identifier))
        }
    }

    /// Iterate over the statements contained in a block, checking each. Should
    /// a return statement be encountered, the type of the returned expression
    /// is returned within `Ok((..., Some(...)))`. If there are multiple return
    /// statements then it will be ensured that they are all returning the same
    /// type. Also returns the number of local variables created (excluding
    /// parameters) within the block as the first part of the returned tuple.
    fn eval_block(&mut self, block: parsing::Block, params: Vec<(String, super::Type)>) -> super::Result<(Vec<super::Instruction>, usize, Option<super::Type>)> {
        let mut instructions = Vec::new();

        self.begin_new_scope();

        for (identifier, param_type) in params.into_iter().rev() {
            let var_id = self.add_variable_def_to_inner_scope(identifier, param_type);
            instructions.push(super::Instruction::Parameter(var_id));
        }

        let mut ret_type = None;
        let mut local_variable_count = 0;

        for stmt in block {
            let (inner_instructions, inner_locals_count, optional_ret_info) = self.eval_inner_stmt(stmt)?;
            instructions.extend(inner_instructions);
            local_variable_count += inner_locals_count;

            if let Some((new, pos)) = optional_ret_info {
                // Has a return type already been established for this block?
                if let Some(current) = &ret_type {
                    if new != *current { // Can't have return statements with different types!
                        return Err(super::Failure::UnexpectedType {
                            pos, expected: current.clone(),
                            encountered: new
                        })
                    }
                }
                else { ret_type.replace(new); }
            }
        }

        self.end_scope();

        Ok((instructions, local_variable_count, ret_type))
    }

    /// Introduce a new, inner-most scope which is added to the end of the scope
    /// stack.
    fn begin_new_scope(&mut self) {
        self.scopes.push(super::Scope { variables: Vec::new() });
    }

    /// Remove the inner-most scope from the scopes stack and deallocate all the
    /// variables from said scope.
    fn end_scope(&mut self) {
        if let Some(previous_scope) = self.scopes.pop() {
            for def in previous_scope.variables {
                self.available_local_variable_ids.push(def.id);
                //self.final_ir.push(super::Instruction::Free(def.id));
            }
        }
    }

    /// Get a mutable reference to the current inner-most scope. Will panic if
    /// the scope stack is empty.
    fn get_inner_scope(&mut self) -> &mut super::Scope {
        self.scopes.last_mut().unwrap()
    }

    /// Search for a definition for a function with a given identifier and set
    /// of parameter types.
    fn function_lookup(&self, ident: &str, params: &[super::Type], strm_pos: &stream::Position) -> super::Result<&super::FunctionDef> {
        for def in self.functions.iter() {
            if def.identifier == ident && def.parameter_types == params {
                return Ok(def);
            }
        }
        Err(super::Failure::FunctionUndefined(strm_pos.clone(), ident.to_string(), params.to_vec()))
    }

    fn add_function_def(&mut self, identifier: String, parameter_types: Vec<super::Type>, return_type: Option<super::Type>, label: String) {
        self.functions.push(super::FunctionDef {
            identifier, parameter_types, return_type, label
        });
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

    fn add_variable_def_to_inner_scope(&mut self, identifier: String, var_type: super::Type) -> super::Id {
        let id = {
            if let Some(available_id) = self.available_local_variable_ids.pop() { available_id }
            else { self.new_id() }
        };
        
        self.get_inner_scope().variables.push(super::VariableDef {
            identifier, var_type, id
        });
        
        id
    }

    /// Check the validity of a given expression as well as return the appropriate
    /// instructions to be inserted into the final IR.
    fn eval_expr(&self, expr: parsing::Expression) -> super::Result<(Vec<super::Instruction>, super::Type, stream::Position)> {
        match expr {
            parsing::Expression::Variable { pos, identifier } => {
                log::trace!("Searching scope for the type of referenced variable with identifier '{}'", identifier);

                let (var_type, id) = { // TODO: Check if variable is initialised before use!
                    let def = self.variable_lookup(&identifier, &pos)?;
                    (def.var_type.clone(), def.id)
                };

                Ok((
                    vec![super::Instruction::Push(super::Value::Variable(id))],
                    var_type, pos
                ))
            }

            parsing::Expression::FunctionCall {pos, identifier, args } => {
                log::trace!("Searching scope for the return type of referenced function '{}' given arguments {:?}", identifier, args);

                let mut instructions = Vec::new();

                let mut arg_types = Vec::new();
                for arg in args {
                    let (arg_instructions, arg_type, _) = self.eval_expr(arg)?;

                    instructions.extend(arg_instructions);
                    arg_types.push(arg_type); 
                }

                let (ident, option_ret_type, label) = {
                    let def = self.function_lookup(&identifier, arg_types.as_slice(), &pos)?;
                    (def.identifier.clone(), def.return_type.clone(), def.label.clone())
                };

                instructions.push(
                    if option_ret_type.is_some() { super::Instruction::CallExpectingValue(label) }
                    else { super::Instruction::CallExpectingVoid(label) }
                );

                match option_ret_type {
                    Some(ret_type) => Ok((instructions, ret_type.clone(), pos)),
                    None => Err(super::Failure::VoidFunctionInExpr(pos, ident, arg_types))
                }
            }

            parsing::Expression::Add(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::Add, "addition")?;
                Ok((instructions, super::Type::Num, pos))
            }

            parsing::Expression::Subtract(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::Subtract, "subtraction")?;
                Ok((instructions, super::Type::Num, pos))
            }

            parsing::Expression::Multiply(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::Multiply, "multiplication")?;
                Ok((instructions, super::Type::Num, pos))
            }

            parsing::Expression::Divide(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::Divide, "divide")?;
                Ok((instructions, super::Type::Num, pos))
            }

            parsing::Expression::GreaterThan(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::GreaterThan, "greater than")?;
                Ok((instructions, super::Type::Bool, pos))
            }

            parsing::Expression::LessThan(l, r) => {
                let (instructions, pos) = self.eval_arithmetic_expr(*l, *r, super::Instruction::LessThan, "less than")?;
                Ok((instructions, super::Type::Bool, pos))
            }

            parsing::Expression::Equal(left, right) => {
                log::trace!("Verifying types of equality expression - types on both sides of the operator should be the same");

                let (mut instructions, left_type, strm_pos) = self.eval_expr(*left)?;
                let (right_instructions, right_type, _) = self.eval_expr(*right)?;

                if left_type == right_type {
                    instructions.extend(right_instructions);
                    instructions.push(super::Instruction::Equals);

                    Ok((instructions, super::Type::Bool, strm_pos))
                }
                else {
                    Err(super::Failure::UnexpectedType {
                        pos: strm_pos,
                        expected: left_type,
                        encountered: right_type
                    })
                }
            }

            parsing::Expression::BooleanNot(expr) => {
                log::trace!("Verifying type of expression to which boolean NOT operator is being applied - expecting Bool expression to right of operator");

                let (mut instructions, strm_pos) = self.expect_expr_type(*expr, super::Type::Bool)?;
                instructions.push(super::Instruction::Not);

                Ok((instructions, super::Type::Bool, strm_pos))
            }

            parsing::Expression::UnaryMinus(expr) => {
                log::trace!("Verify type of expression to which unary minus is being applied - expecting Num");

                let mut instructions = vec![super::Instruction::Push(super::Value::Num(0.0))];
                
                let (contained_instructions, strm_pos) = self.expect_expr_type(*expr, super::Type::Num)?;
                instructions.extend(contained_instructions);
                
                instructions.push(super::Instruction::Subtract);

                Ok((instructions, super::Type::Num, strm_pos))
            }

            parsing::Expression::NumberLiteral {pos, value } => 
                Ok((vec![super::Instruction::Push(super::Value::Num(value))], super::Type::Num, pos)),

            parsing::Expression::BooleanLiteral { pos, value } =>
                Ok((vec![super::Instruction::Push(super::Value::Bool(value))], super::Type::Bool, pos)),

            parsing::Expression::CharLiteral { pos, value } =>
                Ok((vec![super::Instruction::Push(super::Value::Char(value))], super::Type::Char, pos))
        }
    }

    /// Ensure the two sub-expressions of an arithmetic expression are both of
    /// Num type. Insert the relevant final IR instruction also.
    fn eval_arithmetic_expr(&self, left: parsing::Expression, right: parsing::Expression, operation_instruction: super::Instruction, expr_type: &str) -> super::Result<(Vec<super::Instruction>, stream::Position)> {
        log::trace!("Verifying types of {} expression - Num type on both sides of operator expected", expr_type);

        let (mut instructions, strm_pos) = self.expect_expr_type(left, super::Type::Num)?;
        let (right_instructions, _) = self.expect_expr_type(right, super::Type::Num)?;

        instructions.extend(right_instructions);
        instructions.push(operation_instruction);

        Ok((instructions, strm_pos))
    }

    fn expect_expr_type(&self, expr: parsing::Expression, expected: super::Type) -> super::Result<(Vec<super::Instruction>, stream::Position)> {
        let (instructions, expr_type, strm_pos) = self.eval_expr(expr)?;
        
        if expr_type == expected { Ok((instructions, strm_pos)) }
        else {
            Err(super::Failure::UnexpectedType {
                pos: strm_pos, expected, encountered: expr_type
            }) 
        }
    }

    fn new_id(&mut self) -> super::Id {
        let id = self.id_counter;
        self.id_counter += 1;
        id
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

        chkr.add_variable_def_to_inner_scope("outer".to_string(), checking::Type::Num);
        assert_eq!(chkr.variable_lookup("outer", &pos), Ok(&checking::VariableDef {
            identifier: "outer".to_string(),
            var_type: checking::Type::Num,
            id: 0
        }));

        chkr.begin_new_scope();

        chkr.add_variable_def_to_inner_scope("inner".to_string(), checking::Type::Bool);

        assert!(chkr.variable_lookup("inner", &pos).is_ok());
        assert!(chkr.variable_lookup("outer", &pos).is_ok());

        chkr.end_scope();

        assert!(chkr.variable_lookup("inner", &pos).is_err());
        assert!(chkr.variable_lookup("outer", &pos).is_ok());
        assert!(chkr.variable_lookup("undefined", &pos).is_err());
    }

    #[test]
    fn eval_exprs() {
        let mut chkr = new_empty_checker();

        assert_eq!(
            chkr.eval_expr(parsing::Expression::NumberLiteral { pos: Position::new(), value: 10.5 }),
            Ok((
                vec![checking::Instruction::Push(checking::Value::Num(10.5))],
                checking::Type::Num, Position::new()
            ))
        );

        assert_eq!(
            chkr.eval_expr(parsing::Expression::BooleanLiteral { pos: Position::new(), value: true }),
            Ok((
                vec![checking::Instruction::Push(checking::Value::Bool(true))],
                checking::Type::Bool, Position::new()
            ))
        );

        assert_eq!(
            chkr.eval_expr(parsing::Expression::CharLiteral { pos: Position::new(), value: '話' }),
            Ok((
                vec![checking::Instruction::Push(checking::Value::Char('話'))],
                checking::Type::Char, Position::new()
            ))
        );

        assert_eq!(
            chkr.eval_expr(parsing::Expression::Equal(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }),
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'y' })
            )),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Char('x')),
                    checking::Instruction::Push(checking::Value::Char('y')),
                    checking::Instruction::Equals
                ],
                checking::Type::Bool, Position::new()
            ))
        );

        assert_pattern!(
            chkr.eval_expr(parsing::Expression::Equal(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.5 }),
                Box::new(parsing::Expression::BooleanLiteral { pos: Position::new(), value: false })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Bool,
                expected: checking::Type::Num, pos: _
            })
        );

        assert_eq!(
            chkr.eval_expr(parsing::Expression::GreaterThan(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.34 }),
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 0.95 })
            )),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Num(1.34)),
                    checking::Instruction::Push(checking::Value::Num(0.95)),
                    checking::Instruction::GreaterThan
                ],
                checking::Type::Bool, Position::new()
            ))
        );

        assert_pattern!(
            chkr.eval_expr(parsing::Expression::LessThan(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'b' }),
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'a' })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Char,
                expected: checking::Type::Num, pos: _
            })
        );

        assert_eq!(
            chkr.eval_expr(parsing::Expression::Add(
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 10.0 }),
                Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 11.2 })
            )),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Num(10.0)),
                    checking::Instruction::Push(checking::Value::Num(11.2)),
                    checking::Instruction::Add
                ],
                checking::Type::Num, Position::new()
            ))
        );

        assert_pattern!(
            chkr.eval_expr(parsing::Expression::Divide(
                Box::new(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }),
                Box::new(parsing::Expression::BooleanLiteral { pos: Position::new(), value: false })
            )),
            Err(checking::Failure::UnexpectedType {
                encountered: checking::Type::Char,
                expected: checking::Type::Num, pos: _
            })
        );

        assert_pattern!(
            chkr.eval_expr(parsing::Expression::Variable {
                pos: Position::new(),
                identifier: "undefined".to_string()
            }),
            Err(checking::Failure::VariableNotInScope(_, _))
        );

        let var_id = chkr.add_variable_def_to_inner_scope("var".to_string(), checking::Type::Num);

        chkr.begin_new_scope();
        assert_eq!(
            chkr.eval_expr(parsing::Expression::Variable {
                pos: Position::new(),
                identifier: "var".to_string()
            }),
            Ok((
                vec![checking::Instruction::Push(checking::Value::Variable(var_id))],
                checking::Type::Num, Position::new()
            ))
        );
        chkr.end_scope();

        chkr.add_function_def("func".to_string(), vec![checking::Type::Char], Some(checking::Type::Num), "func0".to_string());

        assert_eq!(
            chkr.eval_expr(parsing::Expression::FunctionCall {
                pos: Position::new(),
                identifier: "func".to_string(),
                args: vec![parsing::Expression::CharLiteral { pos: Position::new(), value: 'a' }]
            }),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Char('a')),
                    checking::Instruction::CallExpectingValue("func0".to_string())
                ],
                checking::Type::Num, Position::new()
            ))
        );

        match chkr.eval_expr(parsing::Expression::FunctionCall {
            pos: Position::new(),
            identifier: "func".to_string(),
            args: vec![
                parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.5 }
            ]
        }) {
            Err(checking::Failure::FunctionUndefined(_, ident, args)) => {
                assert_eq!(ident, "func".to_string());
                assert_eq!(args, vec![checking::Type::Num]);
            }
            _ => panic!()
        }

        chkr.add_function_def("abc".to_string(), vec![checking::Type::Char], None, "func1".to_string());

        assert_pattern!(
            chkr.eval_expr(parsing::Expression::FunctionCall {
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
    fn eval_inner_stmts() {
        let mut chkr = new_empty_checker();

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::Return(None)),
            Ok((vec![checking::Instruction::ReturnVoid], 0, None))
        );

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::Return(Some(
                parsing::Expression::Add(
                    Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 1.2 }),
                    Box::new(parsing::Expression::NumberLiteral { pos: Position::new(), value: 2.8 })
                )
            ))),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Num(1.2)),
                    checking::Instruction::Push(checking::Value::Num(2.8)),
                    checking::Instruction::Add,
                    checking::Instruction::ReturnValue
                ],
                0, Some((checking::Type::Num, Position::new()))
            ))
        );

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::If {
                condition: parsing::Expression::BooleanLiteral { pos: Position::new(), value: true },
                block: vec![
                    parsing::Statement::Return(Some(parsing::Expression::CharLiteral { pos: Position::new(), value: 'x' }))
                ]
            }),
            Ok((
                vec![
                    checking::Instruction::Push(checking::Value::Bool(true)),
                    checking::Instruction::JumpIfFalse(0),
                    checking::Instruction::Push(checking::Value::Char('x')),
                    checking::Instruction::ReturnValue,
                    checking::Instruction::Label(0)
                ],
                0, Some((checking::Type::Char, Position::new()))
            ))
        );

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::VariableDeclaration {
                identifier: "pi".to_string(),
                var_type: "Num".to_string(),
                value: Some(parsing::Expression::NumberLiteral { pos: Position::new(), value: 3.14 })
            }),
            Ok((
                vec![
                    checking::Instruction::Local(1),
                    checking::Instruction::Push(checking::Value::Num(3.14)),
                    checking::Instruction::Store(1)
                ],
                1, None
            ))
        );
        assert!(chkr.variable_lookup("pi", &Position::new()).is_ok());

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::VariableDeclaration {
                identifier: "xyz".to_string(),
                var_type: "Oops".to_string(),
                value: None
            }),
            Err(checking::Failure::NonexistentPrimitiveType("Oops".to_string()))
        );

        assert_pattern!(
            chkr.eval_inner_stmt(parsing::Statement::VariableAssignment {
                identifier: "pi".to_string(),
                assign_to: parsing::Expression::NumberLiteral { pos: Position::new(), value: 3.1 }
            }),
            Ok((_, _, None))
        );

        assert_pattern!(
            chkr.eval_inner_stmt(parsing::Statement::VariableAssignment {
                identifier: "pi".to_string(),
                assign_to: parsing::Expression::BooleanLiteral { pos: Position::new(), value: true }
            }),
            Err(checking::Failure::UnexpectedType {
                expected: checking::Type::Num,
                encountered: checking::Type::Bool, pos: _
            })
        );

        assert_pattern!(
            chkr.eval_inner_stmt(parsing::Statement::FunctionDefinition {
                identifier: "nested".to_string(),
                parameters: vec![],
                return_type: None,
                body: vec![],
                pos: Position::new()
            }),
            Err(checking::Failure::NestedFunctions(_, _))
        );
    }

    #[test]
    fn eval_top_level_stmts() -> checking::Result<()> {
        let mut chkr = new_empty_checker();

        pretty_env_logger::init();
        assert_eq!(
            chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
                identifier: "func".to_string(),
                parameters: vec![],
                return_type: None,
                body: vec![
                    parsing::Statement::VariableDeclaration {
                        identifier: "var".to_string(), var_type: "Num".to_string(),
                        value: None
                    }
                ],
                pos: Position::new()
            }),
            Ok(vec![
                checking::Instruction::Function { label: "func0".to_string(), local_variable_count: 1 },
                checking::Instruction::Local(1),
                checking::Instruction::ReturnVoid
            ])
        );
        assert!(chkr.function_lookup("func", &[], &Position::new())?.return_type.is_none());

        assert_eq!(
            chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
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

        assert_pattern!(
            chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
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
            Err(checking::Failure::FunctionUnexpectedReturnType {
                pos: _, identifier: _, params: _,
                expected: checking::Type::Num, encountered: None
            })
        );

        assert_pattern!(
            chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
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
                _, _, _, checking::Type::Bool
            ))
        );

        chkr.id_counter = 0;

        assert_eq!(
            chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
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
            Ok(vec![
                checking::Instruction::Function { label: "func0".to_string(), local_variable_count: 0 },
                checking::Instruction::Parameter(1),
                checking::Instruction::Push(checking::Value::Variable(1)),
                checking::Instruction::ReturnValue
            ])
        );

        let main_func = chkr.eval_top_level_stmt(parsing::Statement::FunctionDefinition {
            identifier: "main".to_string(),
            parameters: vec![],
            return_type: None,
            body: vec![],
            pos: Position::new()
        })?;
        assert_eq!(main_func[0], checking::Instruction::Function { label: "main".to_string(), local_variable_count: 0 });

        Ok(())
    }

    #[test]
    fn variable_shadowing() -> checking::Result<()> {
        let mut chkr = new_empty_checker();

        let pos = Position::new();

        chkr.eval_inner_stmt(parsing::Statement::VariableDeclaration {
            identifier: "x".to_string(),
            var_type: "Num".to_string(),
            value: None
        })?;

        chkr.begin_new_scope();

        // Shadow variable 'x' by declaring a variable in the inner scope of the
        // same name but a different type:
        chkr.eval_inner_stmt(parsing::Statement::VariableDeclaration {
            identifier: "x".to_string(),
            var_type: "Bool".to_string(),
            value: None
        })?;

        assert_eq!(chkr.variable_lookup("x", &pos)?.var_type, checking::Type::Bool);

        chkr.end_scope();

        assert_eq!(chkr.variable_lookup("x", &pos)?.var_type, checking::Type::Num);

        assert_eq!(
            chkr.eval_inner_stmt(parsing::Statement::VariableDeclaration {
                identifier: "x".to_string(),
                var_type: "Char".to_string(),
                value: None
            }),
            Err(checking::Failure::VariableRedeclaredToDifferentType {
                identifier: "x".to_string(),
                expected: checking::Type::Num,
                encountered: checking::Type::Char
            })
        );

        Ok(())
    }
}