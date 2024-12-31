use std::collections::HashMap;
use crate::parser::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    String,
    Array(Box<Type>),
    Struct(String, HashMap<String, Type>),
    Function {
        parameters: Vec<Type>,
        return_types: Vec<Type>,
    },
    Void,
}

#[derive(Debug)]
pub enum SemanticError {
    TypeMismatch {
        expected: Type,
        found: Type,
        context: String,
    },
    UndefinedVariable(String),
    UndefinedFunction(String),
    UndefinedType(String),
    UndefinedStruct(String),
    InvalidFieldAccess {
        struct_name: String,
        field_name: String,
    },
    ImmutableAssignment(String),
    InvalidOperator {
        operator: String,
        type_name: String,
    },
    DuplicateDefinition(String),
    InvalidReturnType {
        expected: Vec<Type>,
        found: Vec<Type>,
    },
    BreakContinueOutsideLoop,
    InvalidAssignmentTarget,
}

#[derive(Clone)]
struct Scope {
    variables: HashMap<String, (Type, bool)>, // (type, is_mutable)
    parent: Option<Box<Scope>>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Box<Scope>) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn lookup_variable(&self, name: &str) -> Option<(Type, bool)> {
        if let Some(var) = self.variables.get(name) {
            Some(var.clone())
        } else if let Some(parent) = &self.parent {
            parent.lookup_variable(name)
        } else {
            None
        }
    }
}

pub struct SemanticAnalyzer {
    current_scope: Scope,
    struct_definitions: HashMap<String, HashMap<String, Type>>,
    function_definitions: HashMap<String, (Vec<Type>, Vec<Type>)>, // (param_types, return_types)
    in_loop: bool,
    current_function_name: Option<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            current_scope: Scope::new(),
            struct_definitions: HashMap::new(),
            function_definitions: HashMap::new(),
            in_loop: false,
            current_function_name: None,
        };
        
        analyzer.function_definitions.insert(
            "print".to_string(),
            (vec![Type::String], vec![])
        );
        analyzer.function_definitions.insert(
            "printl".to_string(), 
            (vec![Type::String], vec![])
        );
        
        analyzer
    }

    pub fn analyze(&mut self, statements: &[Statement]) -> Result<(), SemanticError> {
        for stmt in statements {
            self.analyze_statement(stmt)?;
        }
        Ok(())
    }

    fn analyze_statement(&mut self, stmt: &Statement) -> Result<(), SemanticError> {
        match stmt {
            Statement::VariableDeclaration { variables } => {
                self.analyze_variable_declaration(variables)
            }
            Statement::FunctionDeclaration { name, parameters, return_types, body } => {
                self.analyze_function_declaration(name, parameters, return_types, body)
            }
            Statement::StructDeclaration { name, fields } => {
                self.analyze_struct_declaration(name, fields)
            }
            Statement::If { condition, then_branch, else_branch } => {
                self.analyze_if_statement(condition, then_branch, else_branch)
            }
            Statement::While { condition, body } => {
                self.analyze_while_statement(condition, body)
            }
            Statement::For { initializer, condition, increment, body } => {
                self.analyze_for_statement(initializer, condition, increment, body)
            }
            Statement::ForIn { item, iterator, body } => {
                self.analyze_for_in_statement(item, iterator, body)
            }
            Statement::Return(expressions) => {
                self.analyze_return_statement(expressions)
            }
            Statement::Break => {
                if !self.in_loop {
                    Err(SemanticError::BreakContinueOutsideLoop)
                } else {
                    Ok(())
                }
            }
            Statement::Continue => {
                if !self.in_loop {
                    Err(SemanticError::BreakContinueOutsideLoop)
                } else {
                    Ok(())
                }
            }
            Statement::Expression(expr) => {
                self.infer_type(expr)?;
                Ok(())
            }
            Statement::Import { path: _, alias: _ } => {
                // Import handling would depend on module system implementation
                Ok(())
            }
            Statement::Unsafe { body } => {
                for stmt in body {
                    self.analyze_statement(stmt)?;
                }
                Ok(())
            }
        }
    }

    fn analyze_variable_declaration(&mut self, variables: &[Variable]) -> Result<(), SemanticError> {
        for var in variables {
            let initial_type = if let Some(init) = &var.initializer {
                self.infer_type(init)?
            } else {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Void,
                    found: Type::Void,
                    context: "Variable declaration requires initializer".to_string(),
                });
            };

            if let Some(type_name) = &var.type_annotation {
                let declared_type = self.resolve_type(type_name)?;
                if declared_type != initial_type {
                    return Err(SemanticError::TypeMismatch {
                        expected: declared_type,
                        found: initial_type,
                        context: format!("Variable declaration for '{}'", var.name),
                    });
                }
            }

            if self.current_scope.variables.contains_key(&var.name) {
                return Err(SemanticError::DuplicateDefinition(var.name.clone()));
            }

            self.current_scope.variables.insert(
                var.name.clone(),
                (initial_type, var.mutable),
            );
        }
        Ok(())
    }

    fn analyze_function_declaration(
        &mut self,
        name: &str,
        parameters: &[(String, String)],
        return_types: &[String],
        body: &[Statement]
    ) -> Result<(), SemanticError> {
        // Check for duplicate function definition
        if self.function_definitions.contains_key(name) {
            return Err(SemanticError::DuplicateDefinition(name.to_string()));
        }

        // Create new scope for function body
        let parent_scope = Box::new(self.current_scope.clone());
        self.current_scope = Scope::with_parent(parent_scope);
        self.current_function_name = Some(name.to_string());

        // Process parameters
        let mut param_types = Vec::new();
        for (param_name, type_name) in parameters {
            let param_type = self.resolve_type(type_name)?;
            param_types.push(param_type.clone());
            self.current_scope.variables.insert(param_name.clone(), (param_type, false));
        }

        // Process return types
        let mut resolved_return_types = Vec::new();
        for type_name in return_types {
            resolved_return_types.push(self.resolve_type(type_name)?);
        }

        // Add function to definitions before analyzing body to allow recursion
        self.function_definitions.insert(
            name.to_string(),
            (param_types, resolved_return_types.clone())
        );

        // Analyze function body
        for stmt in body {
            self.analyze_statement(stmt)?;
        }

        // Restore parent scope
        self.current_scope = *self.current_scope.parent.clone().unwrap();
        self.current_function_name = None;

        Ok(())
    }

    fn analyze_struct_declaration(
        &mut self,
        name: &str,
        fields: &[StructField]
    ) -> Result<(), SemanticError> {
        if self.struct_definitions.contains_key(name) {
            return Err(SemanticError::DuplicateDefinition(name.to_string()));
        }

        let mut field_types = HashMap::new();
        for field in fields {
            let field_type = self.resolve_type(&field.type_name)?;
            field_types.insert(field.name.clone(), field_type);
        }

        self.struct_definitions.insert(name.to_string(), field_types);
        Ok(())
    }

    fn analyze_if_statement(
        &mut self,
        condition: &Expression,
        then_branch: &[Statement],
        else_branch: &Option<Vec<Statement>>
    ) -> Result<(), SemanticError> {
        // Condition must be boolean
        let condition_type = self.infer_type(condition)?;
        if condition_type != Type::Bool {
            return Err(SemanticError::TypeMismatch {
                expected: Type::Bool,
                found: condition_type,
                context: "If condition".to_string(),
            });
        }

        // Create new scope for then branch
        let parent_scope = Box::new(self.current_scope.clone());
        self.current_scope = Scope::with_parent(parent_scope);

        // Analyze then branch
        for stmt in then_branch {
            self.analyze_statement(stmt)?;
        }

        // Restore parent scope
        self.current_scope = *self.current_scope.parent.clone().unwrap();

        // Analyze else branch if it exists
        if let Some(else_statements) = else_branch {
            let parent_scope = Box::new(self.current_scope.clone());
            self.current_scope = Scope::with_parent(parent_scope);

            for stmt in else_statements {
                self.analyze_statement(stmt)?;
            }

            self.current_scope = *self.current_scope.parent.clone().unwrap();
        }

        Ok(())
    }

    fn analyze_while_statement(
        &mut self,
        condition: &Expression,
        body: &[Statement]
    ) -> Result<(), SemanticError> {
        let condition_type = self.infer_type(condition)?;
        if condition_type != Type::Bool {
            return Err(SemanticError::TypeMismatch {
                expected: Type::Bool,
                found: condition_type,
                context: "While condition".to_string(),
            });
        }

        let parent_scope = Box::new(self.current_scope.clone());
        self.current_scope = Scope::with_parent(parent_scope);
        
        let previous_in_loop = self.in_loop;
        self.in_loop = true;

        for stmt in body {
            self.analyze_statement(stmt)?;
        }

        self.in_loop = previous_in_loop;
        self.current_scope = *self.current_scope.parent.clone().unwrap();

        Ok(())
    }

    fn analyze_for_statement(
        &mut self,
        initializer: &Statement,
        condition: &Expression,
        increment: &Expression,
        body: &[Statement]
    ) -> Result<(), SemanticError> {
        let parent_scope = Box::new(self.current_scope.clone());
        self.current_scope = Scope::with_parent(parent_scope);

        // Analyze initializer
        self.analyze_statement(initializer)?;

        // Check condition type
        let condition_type = self.infer_type(condition)?;
        if condition_type != Type::Bool {
            return Err(SemanticError::TypeMismatch {
                expected: Type::Bool,
                found: condition_type,
                context: "For loop condition".to_string(),
            });
        }

        // Analyze increment expression
        self.infer_type(increment)?;

        let previous_in_loop = self.in_loop;
        self.in_loop = true;

        // Analyze body
        for stmt in body {
            self.analyze_statement(stmt)?;
        }

        self.in_loop = previous_in_loop;
        self.current_scope = *self.current_scope.parent.clone().unwrap();

        Ok(())
    }

    fn analyze_for_in_statement(
        &mut self,
        item: &str,
        iterator: &Expression,
        body: &[Statement]
    ) -> Result<(), SemanticError> {
        let iterator_type = self.infer_type(iterator)?;
        
        let element_type = match iterator_type {
            Type::Array(element_type) => *element_type,
            _ => return Err(SemanticError::TypeMismatch {
                expected: Type::Array(Box::new(Type::Void)), // Just for error message
                found: iterator_type,
                context: "For-in loop iterator".to_string(),
            }),
        };

        let parent_scope = Box::new(self.current_scope.clone());
        self.current_scope = Scope::with_parent(parent_scope);

        // Add loop variable to scope
        self.current_scope.variables.insert(item.to_string(), (element_type, false));

        let previous_in_loop = self.in_loop;
        self.in_loop = true;

        for stmt in body {
            self.analyze_statement(stmt)?;
        }

        self.in_loop = previous_in_loop;
        self.current_scope = *self.current_scope.parent.clone().unwrap();

        Ok(())
    }

    fn analyze_return_statement(&self, expressions: &[Expression]) -> Result<(), SemanticError> {
        let current_function = self.current_function_name.as_deref().unwrap_or("main");
        let expected_return_types = match self.function_definitions.get(current_function) {
            Some((_, return_types)) => return_types.clone(),
            None => return Err(SemanticError::UndefinedFunction(current_function.to_string())),
        };

        let mut actual_types = Vec::new();
        for (expr, expected_type) in expressions.iter().zip(expected_return_types.iter()) {
            let expr_type = self.infer_type(expr)?;
            actual_types.push(expr_type.clone());
            
            if expr_type != *expected_type {
                return Err(SemanticError::InvalidReturnType {
                    expected: expected_return_types,
                    found: actual_types,
                });
            }
        }

        Ok(())
    }

    fn infer_type(&self, expr: &Expression) -> Result<Type, SemanticError> {
        match expr {
            Expression::Integer(_) => Ok(Type::Int),
            Expression::Float(_) => Ok(Type::Float),
            Expression::Boolean(_) => Ok(Type::Bool),
            Expression::Char(_) => Ok(Type::Char),
            Expression::String(_) => Ok(Type::String),
            Expression::Identifier(name) => {
                if let Some((type_, _)) = self.current_scope.lookup_variable(name) {
                    Ok(type_)
                } else {
                    Err(SemanticError::UndefinedVariable(name.clone()))
                }
            }
            Expression::BinaryOp { left, operator, right } => {
                let left_type = self.infer_type(left)?;
                let right_type = self.infer_type(right)?;

                match operator {
                    BinaryOperator::Add | BinaryOperator::Subtract | 
                    BinaryOperator::Multiply | BinaryOperator::Divide |
                    BinaryOperator::Modulo => {
                        match (left_type.clone(), right_type.clone()) {
                            (Type::Int, Type::Int) => Ok(Type::Int),
                            (Type::Float, Type::Float) => Ok(Type::Float),
                            (Type::Int, Type::Float) => Ok(Type::Float),
                            (Type::Float, Type::Int) => Ok(Type::Float),
                            _ => Err(SemanticError::TypeMismatch {
                                expected: left_type,
                                found: right_type,
                                context: "Arithmetic operation".to_string(),
                            }),
                        }
                    }
                    BinaryOperator::Equals | BinaryOperator::NotEquals => Ok(Type::Bool),
                    BinaryOperator::GreaterThan | BinaryOperator::LessThan |
                    BinaryOperator::GreaterThanEquals | BinaryOperator::LessThanEquals => {
                        if left_type != right_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: left_type,
                                found: right_type,
                                context: "Comparison operation".to_string(),
                            });
                        }
                        Ok(Type::Bool)
                    }
                    BinaryOperator::And | BinaryOperator::Or => {
                        if left_type != Type::Bool {
                            return Err(SemanticError::TypeMismatch { 
                                expected: Type::Bool, 
                                found: left_type,
                                context: "Logical operation".to_string()
                            });
                        }
                        else if right_type != Type::Bool {
                            return Err(SemanticError::TypeMismatch { 
                                expected: Type::Bool, 
                                found: right_type, 
                                context: "Logical operation".to_string()
                            });
                        }
                        Ok(Type::Bool)
                    }
                    BinaryOperator::BitwiseOr | BinaryOperator::BitwiseAnd | 
                    BinaryOperator::BitwiseXor | BinaryOperator::LeftShift | 
                    BinaryOperator::RightShift => {
                        if left_type != Type::Int {
                            return Err(SemanticError::TypeMismatch { 
                                expected: Type::Int, 
                                found: left_type,
                                context: "Bitwise operation".to_string()
                            });
                        }
                        else if right_type != Type::Int {
                            return Err(SemanticError::TypeMismatch { 
                                expected: Type::Int, 
                                found: right_type,
                                context: "Bitwise operation".to_string()
                            });
                        }
                        Ok(Type::Int)
                    }
                    BinaryOperator::Assign | BinaryOperator::AddAssign => {
                        if let Expression::Identifier(name) = &**left {
                            if let Some((var_type, is_mutable)) = self.current_scope.lookup_variable(name) {
                                if !is_mutable {
                                    return Err(SemanticError::ImmutableAssignment(name.clone()));
                                }

                                match operator {
                                    BinaryOperator::Assign => {
                                        if var_type != right_type {
                                            return Err(SemanticError::TypeMismatch {
                                                expected: var_type,
                                                found: right_type,
                                                context: format!("Assignment to variable '{}'", name),
                                            });
                                        }
                                    }
                                    BinaryOperator::AddAssign => {
                                        match (&var_type, &right_type) {
                                            (Type::Int, Type::Int) |
                                            (Type::Float, Type::Float) |
                                            (Type::Int, Type::Float) |
                                            (Type::Float, Type::Int) |
                                            (Type::String, Type::String) => (),
                                            _ => return Err(SemanticError::InvalidOperator {
                                                operator: "+=".to_string(),
                                                type_name: format!("{:?}", var_type),
                                            }),
                                        }
                                    }
                                    _ => unreachable!(),
                                }
                                Ok(var_type)
                            } else {
                                Err(SemanticError::UndefinedVariable(name.clone()))
                            }
                        } else {
                            Err(SemanticError::InvalidAssignmentTarget)
                        }
                    }
                }
            }
            Expression::UnaryOp { operator, operand } => {
                let operand_type = self.infer_type(operand)?;
                match operator {
                    UnaryOperator::Negate => {
                        match operand_type {
                            Type::Int => Ok(Type::Int),
                            Type::Float => Ok(Type::Float),
                            _ => Err(SemanticError::InvalidOperator {
                                operator: "unary -".to_string(),
                                type_name: format!("{:?}", operand_type),
                            }),
                        }
                    }
                    UnaryOperator::Not => {
                        if operand_type != Type::Bool {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Bool,
                                found: operand_type,
                                context: "Logical not operation".to_string(),
                            });
                        }
                        Ok(Type::Bool)
                    }
                    UnaryOperator::BitwiseNot => {
                        if operand_type != Type::Int {
                            return Err(SemanticError::TypeMismatch {
                                expected: Type::Int,
                                found: operand_type,
                                context: "Bitwise not operation".to_string(),
                            });
                        }
                        Ok(Type::Int)
                    }
                }
            }
            Expression::FunctionCall { name, arguments } => {
                if let Some((param_types, return_types)) = self.function_definitions.get(name) {
                    if arguments.len() != param_types.len() {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Function {
                                parameters: param_types.clone(),
                                return_types: return_types.clone(),
                            },
                            found: Type::Function {
                                parameters: vec![],
                                return_types: vec![],
                            },
                            context: format!("Function call to '{}'", name),
                        });
                    }

                    // Check argument types
                    for (arg, expected_type) in arguments.iter().zip(param_types.iter()) {
                        let arg_type = self.infer_type(arg)?;
                        if arg_type != *expected_type {
                            return Err(SemanticError::TypeMismatch {
                                expected: expected_type.clone(),
                                found: arg_type,
                                context: format!("Argument in function call to '{}'", name),
                            });
                        }
                    }

                    // Return the first return type or Void if none
                    Ok(return_types.first().cloned().unwrap_or(Type::Void))
                } else {
                    Err(SemanticError::UndefinedFunction(name.clone()))
                }
            }
            Expression::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return Ok(Type::Array(Box::new(Type::Void)));
                }

                let first_type = self.infer_type(&elements[0])?;
                for element in elements.iter().skip(1) {
                    let element_type = self.infer_type(element)?;
                    if element_type != first_type {
                        return Err(SemanticError::TypeMismatch {
                            expected: first_type,
                            found: element_type,
                            context: "Array literal element".to_string(),
                        });
                    }
                }

                Ok(Type::Array(Box::new(first_type)))
            }
            Expression::IndexAccess { array, index } => {
                let array_type = self.infer_type(array)?;
                let index_type = self.infer_type(index)?;

                if index_type != Type::Int {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Int,
                        found: index_type,
                        context: "Array index".to_string(),
                    });
                }

                match array_type {
                    Type::Array(element_type) => Ok(*element_type),
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Type::Array(Box::new(Type::Void)),
                        found: array_type,
                        context: "Array indexing".to_string(),
                    }),
                }
            }
            Expression::FieldAccess { object, field } => {
                let object_type = self.infer_type(object)?;
                match object_type {
                    Type::Struct(struct_name, fields) => {
                        if let Some(field_type) = fields.get(field) {
                            Ok(field_type.clone())
                        } else {
                            Err(SemanticError::InvalidFieldAccess {
                                struct_name,
                                field_name: field.clone(),
                            })
                        }
                    }
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Type::Struct("any".to_string(), HashMap::new()),
                        found: object_type,
                        context: format!("Field access '{}'", field),
                    }),
                }
            }
            Expression::StructLiteral { name, fields } => {
                if let Some(struct_fields) = self.struct_definitions.get(name) {
                    // Check that all required fields are present
                    for (field_name, expected_type) in struct_fields {
                        if let Some((_, expr)) = fields.iter().find(|(name, _)| name == field_name) {
                            let field_type = self.infer_type(expr)?;
                            if field_type != *expected_type {
                                return Err(SemanticError::TypeMismatch {
                                    expected: expected_type.clone(),
                                    found: field_type,
                                    context: format!("Field '{}' in struct '{}'", field_name, name),
                                });
                            }
                        } else {
                            return Err(SemanticError::InvalidFieldAccess {
                                struct_name: name.clone(),
                                field_name: field_name.clone(),
                            });
                        }
                    }

                    // Check for extra fields
                    for (field_name, _) in fields {
                        if !struct_fields.contains_key(field_name) {
                            return Err(SemanticError::InvalidFieldAccess {
                                struct_name: name.clone(),
                                field_name: field_name.clone(),
                            });
                        }
                    }

                    Ok(Type::Struct(name.clone(), struct_fields.clone()))
                } else {
                    Err(SemanticError::UndefinedStruct(name.clone()))
                }
            }
            Expression::Range { start, end, inclusive: _ } => {
                let start_type = self.infer_type(start)?;
                let end_type = self.infer_type(end)?;

                if start_type != end_type {
                    return Err(SemanticError::TypeMismatch {
                        expected: start_type,
                        found: end_type,
                        context: "Range bounds".to_string(),
                    });
                }

                match start_type {
                    Type::Int => Ok(Type::Array(Box::new(Type::Int))),
                    _ => Err(SemanticError::TypeMismatch {
                        expected: Type::Int,
                        found: start_type,
                        context: "Range bounds".to_string(),
                    }),
                }
            }
        }
    }
    
    fn resolve_type(&self, type_name: &str) -> Result<Type, SemanticError> {
        match type_name {
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            "string" => Ok(Type::String),
            _ => {
                if let Some(fields) = self.struct_definitions.get(type_name) {
                    Ok(Type::Struct(type_name.to_string(), fields.clone()))
                } else {
                    Err(SemanticError::UndefinedType(type_name.to_string()))
                }
            }
        }
    }
}
