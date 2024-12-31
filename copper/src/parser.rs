use crate::lexer::TokenKind;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(String),
    MismatchedInitializerCount(String),
    UnexpectedEOF,
    InvalidExpression,
    InvalidAssignmentTarget,
}

#[derive(Debug)]
pub enum Expression {
    Integer(i32),
    Float(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Identifier(String),
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
    ArrayLiteral(Vec<Expression>),
    IndexAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    FieldAccess {
    object: Box<Expression>,
    field: String,
    },
    StructLiteral {
        name: String,
        fields: Vec<(String, Expression)>,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        inclusive: bool,
    },
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanEquals,
    LessThanEquals,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    Assign,
    AddAssign,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Negate,
    Not,
    BitwiseNot,
}

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        variables: Vec<Variable>,
    },
    FunctionDeclaration {
        name: String,
        parameters: Vec<(String, String)>, // (name, type)
        return_types: Vec<String>,
        body: Vec<Statement>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<StructField>,
    },
    For {
        initializer: Box<Statement>,
        condition: Expression,
        increment: Box<Expression>,
        body: Vec<Statement>,
    },
    ForIn {
        item: String,
        iterator: Expression,
        body: Vec<Statement>,
    },
    If {
        condition: Expression,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        body: Vec<Statement>,
    },
    Return(Vec<Expression>),
    Break,
    Continue,   
    Expression(Expression),
    Import {
        path: Vec<String>,
        alias: Option<String>,
    },
    Unsafe {
        body: Vec<Statement>
    },
}

#[derive(Debug)]
pub struct Variable {
    pub name: String,
    pub mutable: bool,
    pub type_annotation: Option<String>,
    pub initializer: Option<Expression>,
}

#[derive(Debug)]
pub struct StructField {
    pub name: String,
    pub type_name: String,
}

pub struct Parser {
    tokens: Vec<TokenKind>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<TokenKind>) -> Parser {
        Parser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            Some(TokenKind::Var) => self.variable_declaration(),
            Some(TokenKind::Const) => self.variable_declaration(),
            Some(TokenKind::Func) => self.function_declaration(),
            Some(TokenKind::Struct) => self.struct_declaration(),
            _ => self.statement(),
        }
    }

    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let is_mutable = match self.advance() {
            Some(TokenKind::Var) => true,
            Some(TokenKind::Const) => false,
            _ => return Err(ParseError::UnexpectedToken("Expected var or const".to_string())),
        };

        let mut variables = Vec::new();

        loop {
            let name = match self.advance() {
                Some(TokenKind::Identifier(name)) => name,
                _ => return Err(ParseError::UnexpectedToken("Expected identifier".to_string()))
            };

            let type_annotation = if let Some(TokenKind::Colon) = self.peek() {
            self.advance();
            match self.advance() {
                Some(TokenKind::Identifier(type_name)) => Some(type_name),
                _ => return Err(ParseError::UnexpectedToken("Expected type name".to_string())),
            }
            } else {
                None
            };

            variables.push((name, type_annotation));

            if let Some(TokenKind::Comma) = self.peek() {
                self.advance(); // consume comma
                continue;
            } else {
                break;
            }
        }
        


        // Parse optional initializer
        let initializers = if let Some(TokenKind::Equals) = self.peek() {
            self.advance(); // consume equals
            let mut expressions = Vec::new();

            loop {
                expressions.push(self.expression()?);

                if let Some(TokenKind::Comma) = self.peek() {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }

            Some(expressions)
        } else {
            None
        };

        self.consume_string(TokenKind::Semicolon, format!("Expected ';' after variable declaration, got '{:?}'", self.tokens.get(self.current)))?;

        if let Some(initializers) = &initializers {
            if initializers.len() > variables.len() {
                return Err(ParseError::MismatchedInitializerCount("Too many initializers in variable declaration".to_string()));
            } else if initializers.len() < variables.len() {
                return Err(ParseError::MismatchedInitializerCount("Not enough initializers in variable declaration".to_string()))
            }
        }

        Ok(Statement::VariableDeclaration {
        variables: variables
            .into_iter()
            .zip(initializers.unwrap_or_default())
            .map(|((name, type_annotation), initializer)| Variable {
                name,
                mutable: is_mutable,
                type_annotation,
                initializer: Some(initializer),
            })
            .collect(),
        })
    }

    fn function_declaration(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // consume 'func'
        
        let name = match self.advance() {
            Some(TokenKind::Identifier(name)) => name,
            _ => return Err(ParseError::UnexpectedToken("Expected function name".to_string())),
        };

        self.consume(TokenKind::LeftParenthesis, "Expected '(' after function name")?;
        
        let mut parameters = Vec::new();
        if !matches!(self.peek(), Some(TokenKind::RightParanthesis)) {
            loop {
                let param_name = match self.advance() {
                    Some(TokenKind::Identifier(name)) => name,
                    _ => return Err(ParseError::UnexpectedToken("Expected parameter name".to_string())),
                };

                self.consume(TokenKind::Colon, "Expected ':' after parameter name")?;

                let param_type = match self.advance() {
                    Some(TokenKind::Identifier(type_name)) => type_name,
                    _ => return Err(ParseError::UnexpectedToken("Expected parameter type".to_string())),
                };

                parameters.push((param_name, param_type));

                if !matches!(self.peek(), Some(TokenKind::Comma)) {
                    break;
                }
                self.advance(); // consume comma
            }
        }

        self.consume(TokenKind::RightParanthesis, "Expected ')' after parameters")?;

        let mut return_types = Vec::new();

        if !matches!(self.peek(), Some(TokenKind::LeftBrace)) {
            loop {
                let type_name = match self.advance() {
                    Some(TokenKind::Identifier(name)) => name,
                    _ => return Err(ParseError::UnexpectedToken("Expected return type".to_string())),
                };

                return_types.push(type_name);

                if !matches!(self.peek(), Some(TokenKind::Comma)) {
                    break;
                }
                self.advance(); 
            }
        }

        self.consume(TokenKind::LeftBrace, "Expected '{' before function body")?;
        
        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            body.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after function body")?;

        Ok(Statement::FunctionDeclaration {
            name,
            parameters,
            return_types,
            body,
        })
    }

            
    fn struct_declaration(&mut self) -> Result<Statement, ParseError> {
        self.advance();

        let name = match self.advance() {
            Some(TokenKind::Identifier(name)) => name,
            _ => return Err(ParseError::UnexpectedToken("Expected identifier after struct".to_string())),
        };

        self.consume(TokenKind::LeftBrace, "Expected '{' after struct name")?;

        let mut fields = Vec::new();

        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            let field_name = match self.advance() {
                Some(TokenKind::Identifier(name)) => name,
                _ => return Err(ParseError::UnexpectedToken("Expected field name".to_string())),
            };

            self.consume(TokenKind::Colon, "Expected ':' after field name")?;

            let field_type = match self.advance() {
                Some(TokenKind::Identifier(type_name)) => type_name,
                _ => return Err(ParseError::UnexpectedToken("Expected field type".to_string())),
            };

            fields.push(StructField {
                name: field_name,
                type_name: field_type,
            });

            if matches!(self.peek(), Some(TokenKind::Comma)) {
                self.advance();
            } else if !matches!(self.peek(), Some(TokenKind::RightBrace)) {
                return Err(ParseError::UnexpectedToken("Expected ',' or '}' after field".to_string()));
            }
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after struct fields")?;

        Ok(Statement::StructDeclaration { name, fields })
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            Some(TokenKind::If) => self.if_statement(),
            Some(TokenKind::While) => self.while_statement(),
            Some(TokenKind::For) => self.for_statement(),
            Some(TokenKind::Break) => self.break_statement(),
            Some(TokenKind::Continue) => self.continue_statement(),
            Some(TokenKind::Return) => self.return_statement(),
            Some(TokenKind::Import) => self.import_statement(),
            Some(TokenKind::Unsafe) => self.unsafe_statement(),
            _ => self.expression_statement(),
        }
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // consume 'if'
        
        let condition = self.expression()?;

        self.consume(TokenKind::LeftBrace, "Expected '{' before if body")?;
        let mut then_branch = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            then_branch.push(self.declaration()?);
        }
        self.consume(TokenKind::RightBrace, "Expected '}' after if body")?;

        let else_branch = if matches!(self.peek(), Some(TokenKind::Else)) {
            self.advance(); // consume 'else'
            self.consume(TokenKind::LeftBrace, "Expected '{' before else body")?;
            let mut else_statements = Vec::new();
            while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
                else_statements.push(self.declaration()?);
            }
            self.consume(TokenKind::RightBrace, "Expected '}' after else body")?;
            Some(else_statements)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance();
        
        let condition = if matches!(self.peek(), Some(TokenKind::LeftBrace)) {
            Expression::Boolean(true) // Represent infinite loop as 'true'
        } else {
            self.expression()?
        };
        
        self.consume(TokenKind::LeftBrace, "Expected '{' before while body")?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            body.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after while body")?;

        Ok(Statement::While { condition, body })
    }

    fn break_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // consume 'break'
        
        self.consume(TokenKind::Semicolon, "Expected ';' after break statement")?;
        Ok(Statement::Break)
    }

    fn continue_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // consume 'break'
        
        self.consume(TokenKind::Semicolon, "Expected ';' after continue statement")?;
        Ok(Statement::Continue)
    }

    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance(); // consume 'return'
        
        let mut values = Vec::new();
        
        // Handle empty return
        if !matches!(self.peek(), Some(TokenKind::Semicolon)) {
            loop {
                values.push(self.expression()?); // Parse an expression
            
                if !matches!(self.peek(), Some(TokenKind::Comma)) {
                    break; // Stop if no more commas
                }
                self.advance(); // Consume comma
            }
        }
        
        self.consume(TokenKind::Semicolon, "Expected ';' after return statement")?;
        Ok(Statement::Return(values))
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance();

        if let Some(TokenKind::Identifier(item)) = self.advance() { // for-in loop
            if matches!(self.peek(), Some(TokenKind::In)) {
                self.advance();
                let iterator = self.expression()?;
                
                self.consume(TokenKind::LeftBrace, "Expected '{' after for-in clause")?;
                
                let mut body = Vec::new();
                while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
                    body.push(self.declaration()?);
                }
                
                self.consume(TokenKind::RightBrace, "Expected '}' after for body")?;

                return Ok(Statement::ForIn {
                    item,
                    iterator,
                    body,
                });
            }
        }

        let initializer = if matches!(self.peek(), Some(TokenKind::Semicolon)) {
            Box::new(Statement::Expression(Expression::Integer(0))) // Empty initializer
        } else {
            Box::new(self.declaration()?)
        };

        let condition = if matches!(self.peek(), Some(TokenKind::Semicolon)) {
            Expression::Boolean(true) // Empty condition = true
        } else {
            self.expression()?
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after for condition")?;

        let increment = if matches!(self.peek(), Some(TokenKind::LeftBrace)) {
            Box::new(Expression::Integer(0)) // Empty increment
        } else {
            Box::new(self.expression()?)
        };
        
        self.consume(TokenKind::LeftBrace, "Expected '{' after for clause")?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            body.push(self.declaration()?);
        }
        
        self.consume(TokenKind::RightBrace, "Expected '}' after for body")?;

        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn import_statement(&mut self) -> Result<Statement, ParseError> {

        let mut path = Vec::new();

        self.advance();

        match self.advance() {
            Some(TokenKind::Identifier(name)) => path.push(name),
            _ => { 
                return Err(ParseError::UnexpectedToken(format!("Expected identifier in import path, got {:?}", self.tokens.get(self.current))))
            }
        }
        

        while matches!(self.peek(), Some(TokenKind::Dot)) {
            self.advance();
            match self.advance() {
                Some(TokenKind::Identifier(name)) => path.push(name),
                _ => return Err(ParseError::UnexpectedToken("Expected identifier after '.' in import path".to_string())),
            }
        }

        let alias = if matches!(self.peek(), Some(TokenKind::As)) {
            self.advance();
            match self.advance() {
                Some(TokenKind::Identifier(name)) => Some(name),
                _ => return Err(ParseError::UnexpectedToken("Expected identifier after 'as'".to_string())),
            }
        } else {
            None
        };

        self.consume(TokenKind::Semicolon, "Expected ';' after import statement")?;
        
        Ok(Statement::Import { path, alias })
    }

    fn unsafe_statement(&mut self) -> Result<Statement, ParseError> {
        self.advance();

        self.consume(TokenKind::LeftBrace, "Expected '{' after unsafe statement")?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            body.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after unsafe block")?;

        Ok(Statement::Unsafe { body })
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        
        match &expr {
            Expression::FunctionCall { .. } |   
            Expression::Identifier(_) |          
            Expression::BinaryOp { .. } |       
            Expression::UnaryOp { .. } => {
                self.consume(TokenKind::Semicolon, "Expected ';' after expression")?;
            },
            _ => return Err(ParseError::InvalidExpression),
        }
        
        Ok(Statement::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let expr = self.equality()?;

        if matches!(self.peek(), Some(TokenKind::Equals) | Some(TokenKind::PlusEquals)) {
            let operator = match self.advance() {
                Some(TokenKind::Equals) => BinaryOperator::Assign,
                Some(TokenKind::PlusEquals) => BinaryOperator::AddAssign,
                _ => unreachable!(),
            };

            match expr {
                Expression::Identifier(_) => {},
                _ => return Err(ParseError::InvalidAssignmentTarget),
            }

            let value = self.assignment()?;
            return Ok(Expression::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(value),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.logical_or()?;

        while let Some(operator) = match self.peek() {
            Some(TokenKind::DoubleEquals) => Some(BinaryOperator::Equals),
            Some(TokenKind::NotEquals) => Some(BinaryOperator::NotEquals),
            _ => None,
        } {
            self.advance(); // consume operator
            let right = self.logical_or()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.logical_and()?;

        while let Some(TokenKind::OrOr) = self.peek() {
            self.advance();
            let right = self.logical_and()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::Or, 
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bitwise_or()?;

        while let Some(TokenKind::AndAnd) = self.peek() {
            self.advance();
            let right = self.logical_and()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::And, 
                right: Box::new(right) 
            }
        }
        Ok(expr)
    }

    fn bitwise_or(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bitwise_and()?;

        while let Some(TokenKind::Or) = self.peek() {
            self.advance();
            let right = self.bitwise_and()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::BitwiseOr, 
                right: Box::new(right) 
            }
        }
        Ok(expr)
    }

    fn bitwise_and(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.bitwise_xor()?;

        while let Some(TokenKind::And) = self.peek() {
            self.advance();
            let right = self.bitwise_and()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::BitwiseAnd, 
                right: Box::new(right) 
            }
        }
        Ok(expr)
    }

    fn bitwise_xor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.left_shift()?;

        while let Some(TokenKind::Xor) = self.peek() {
            self.advance();
            let right = self.left_shift()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr), operator: BinaryOperator::BitwiseXor, right: Box::new(right) 
            }
        }
        Ok(expr)
    }

    fn left_shift(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.right_shift()?;

        while let Some(TokenKind::LeftShift) = self.peek() {
            self.advance();
            let right = self.right_shift()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::LeftShift, 
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn right_shift(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(TokenKind::LeftShift) = self.peek() {
            self.advance();
            let right = self.comparison()?;
            expr = Expression::BinaryOp { 
                left: Box::new(expr), 
                operator: BinaryOperator::RightShift, 
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.range()?;

        while let Some(operator) = match self.peek() {
            Some(TokenKind::GreaterThan) => Some(BinaryOperator::GreaterThan),
            Some(TokenKind::LessThan) => Some(BinaryOperator::LessThan),
            Some(TokenKind::GreaterThanEquals) => Some(BinaryOperator::GreaterThanEquals),
            Some(TokenKind::LessThanEquals) => Some(BinaryOperator::LessThanEquals),
            _ => None,
        } {
            self.advance(); // consume operator
            let right = self.term()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn range(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.term()?;

        while let Some(operator) = match self.peek() {
            Some(TokenKind::DotDot) => Some(false),
            Some(TokenKind::DotDotEquals) => Some(true), 
            _ => None,
        } {
            self.advance(); // consume operator
            let end_expr = self.term()?;

            expr = Expression::Range {
                start: Box::new(expr),
                end: Box::new(end_expr),
                inclusive: operator,
            };
        }

        Ok(expr)
    }
    
    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.factor()?;

        while let Some(operator) = match self.peek() {
            Some(TokenKind::Plus) => Some(BinaryOperator::Add),
            Some(TokenKind::Minus) => Some(BinaryOperator::Subtract),
            _ => None,
        } {
            self.advance(); // consume operator
            let right = self.factor()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.unary()?;

        while let Some(operator) = match self.peek() {
            Some(TokenKind::Star) => Some(BinaryOperator::Multiply),
            Some(TokenKind::Slash) => Some(BinaryOperator::Divide),
            Some(TokenKind::Procent) => Some(BinaryOperator::Modulo),
            _ => None,
        } {
            self.advance(); // consume operator
            let right = self.unary()?;
            expr = Expression::BinaryOp {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, ParseError> {
        if let Some(operator) = match self.peek() {
            Some(TokenKind::Minus) => Some(UnaryOperator::Negate),
            Some(TokenKind::Not) => Some(UnaryOperator::Not),
            Some(TokenKind::BwNot) => Some(UnaryOperator::BitwiseNot),
            _ => None,
        } {
            self.advance(); // consume operator
            let right = self.unary()?;
            return Ok(Expression::UnaryOp {
                operator,
                operand: Box::new(right),
            });
        }

        self.primary()
    }

     fn primary(&mut self) -> Result<Expression, ParseError> {
        match self.advance() {
            Some(TokenKind::IntLiteral(value)) => Ok(Expression::Integer(value)),
            Some(TokenKind::FloatLiteral(value)) => Ok(Expression::Float(value)),
            Some(TokenKind::BoolLiteral(value)) => Ok(Expression::Boolean(value)),
            Some(TokenKind::CharLiteral(value)) => Ok(Expression::Char(value)),
            Some(TokenKind::StrLiteral(value)) => Ok(Expression::String(value)),
            Some(TokenKind::LeftBracket) => self.array_literal(),

            Some(TokenKind::Identifier(name)) => {
                
                let expr = Expression::Identifier(name.clone());
                    
                if matches!(self.peek(), Some(TokenKind::LeftBrace)) {
                    if matches!(self.peek_previous(), Some(TokenKind::Equals | TokenKind::Comma | TokenKind::LeftParenthesis)) {
                        Ok(self.struct_literal(name)?)
                    } else {
                        Ok(Expression::Identifier(name))
                    }
                } else if matches!(self.peek(), Some(TokenKind::LeftBracket)) {
                    self.advance();
                    let index = self.expression()?;
                    self.consume(TokenKind::RightBracket, "Expected ']' after index")?;

                    return Ok(Expression::IndexAccess { 
                        array: Box::new(expr), 
                        index: Box::new(index) 
                    });
                } else if matches!(self.peek(), Some(TokenKind::Dot)) {
                    self.advance();
                    let field = match self.advance() {
                        Some(TokenKind::Identifier(name)) => name,
                        _ => return Err(ParseError::UnexpectedToken(
                            "Expected field anme after '.'".to_string()    
                        )),
                    };

                    return Ok(Expression::FieldAccess { 
                        object: Box::new(expr), 
                        field,
                    });
                } else if matches!(self.peek(), Some(TokenKind::LeftParenthesis)) {
                    self.advance();
                    let mut arguments = Vec::new();

                    if !matches!(self.peek(), Some(TokenKind::RightParanthesis)) {
                        loop {
                            arguments.push(self.expression()?);
                                
                            if !matches!(self.peek(), Some(TokenKind::Comma)) {
                                 break;
                            }
                            self.advance();                             
                        }
                    }

                    self.consume(TokenKind::RightParanthesis, "Expected ')' after fieldname.")?;

                    return Ok(Expression:: FunctionCall { 
                        name, 
                        arguments, 
                    });

                } else {
                    Ok(Expression::Identifier(name))
                }
            },
            Some(TokenKind::LeftParenthesis) => {
                let expr = self.expression()?;
                self.consume(TokenKind::RightParanthesis, "Expected ')' after expression")?;
                Ok(expr)
            },
            Some(TokenKind::DotDot) | Some(TokenKind::DotDotEquals) => {
                Err(ParseError::UnexpectedToken("Range operator without start value".to_string()))
            },
            Some(token) => Err(ParseError::UnexpectedToken(format!("Unexpected token: {:?}", token))),
            None => Err(ParseError::UnexpectedEOF),
        }
    }


    fn array_literal(&mut self) -> Result<Expression, ParseError> {
        let mut elements = Vec::new();

        if !matches!(self.peek(), Some(TokenKind::RightBracket)) {

            loop {
                elements.push(self.expression()?);
            
                if matches!(self.peek(), Some(TokenKind::RightBracket)) {
                    break;
                }
            
                self.consume(TokenKind::Comma, "Expected ',' or ']' after array.")?;
                }
        }      

        self.consume(TokenKind::RightBracket, "Expected ']' after array elements")?;
        Ok(Expression::ArrayLiteral(elements))
    }
    

    fn struct_literal(&mut self, name: String) -> Result<Expression, ParseError> {
        self.consume(TokenKind::LeftBrace, "Expected '{' after struct identifier")?;

        let mut fields = Vec::new();

        if !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            loop {
                let field_name = match self.advance() {
                    Some(TokenKind::Identifier(field_name)) => 
                    {   
                        field_name 
                    },
                    _ => return Err(ParseError::UnexpectedToken("Expected field name".to_string())),
                };

                self.consume(TokenKind::Colon, "Expected ':' after field name")?;

                let value = self.expression()?;

                fields.push((field_name, value));
                
                println!("{:?}", self.tokens.get(self.current));
                if matches!(self.peek(), Some(TokenKind::Comma)) {
                    self.advance();
                    if matches!(self.peek(), Some(TokenKind::RightBrace)) {
                        break;
                    }
                } else {
                    break; 
                }
            }
        }
    
        self.consume(TokenKind::RightBrace, "Expected '}' after struct fields")?;

        Ok(Expression::StructLiteral { name, fields })
    }
   

    // Helper methods

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.get(self.current)
    }

    fn peek_previous(&self) -> Option<&TokenKind> {
        self.tokens.get(self.current - 2)
    }

    fn advance(&mut self) -> Option<TokenKind> {
        if self.is_at_end() {
            None
        } else {
            let token = self.tokens.get(self.current).cloned();
            self.current += 1;
            token
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn consume(&mut self, expected: TokenKind, error_message: &str) -> Result<TokenKind, ParseError> {
        if let Some(token) = self.peek() {
            if token == &expected {
                Ok(self.advance().unwrap())
            } else {
                Err(ParseError::UnexpectedToken(error_message.to_string()))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }

    fn consume_string(&mut self, expected: TokenKind, error_message: String) -> Result<TokenKind, ParseError> {
        if let Some(token) = self.peek() {
            if token == &expected {
                Ok(self.advance().unwrap())
            } else {
                Err(ParseError::UnexpectedToken(error_message))
            }
        } else {
            Err(ParseError::UnexpectedEOF)
        }
    }
}
