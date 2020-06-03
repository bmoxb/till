pub mod parser;

pub enum Statement {
    If {
        condition: Expression,
        if_block: Vec<Statement>,
        else_block: Option<Vec<Statement>>
    },

    FunctionDefinition {
        identifier: String,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
        body: Vec<Statement>
    },

    VariableDeclaration {
        identifier: String,
        var_type: Type,
        value: Option<Expression>
    },

    VariableAssignment {
        identifier: String,
        value: Expression
    }
}

pub struct Parameter (String, Type);

pub enum Type {
    Identifier(String),
    Array(Box<Type>)
}

pub enum Expression {
    // ...
}