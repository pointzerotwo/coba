/// Recursive descent parser for Coba

use coba_ast::{
    expr::{BinaryOp, Expr, ExprKind, UnaryOp},
    program::{Declaration, Parameter, Program},
    stmt::{Stmt, StmtKind},
    types::Type,
};
use coba_lexer::token::{Token, TokenKind};

use crate::error::ParseError;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            errors: Vec::new(),
        }
    }

    /// Parse the entire program
    pub fn parse(&mut self) -> Result<Program, Vec<ParseError>> {
        let mut declarations = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => declarations.push(decl),
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize();
                }
            }
        }

        if self.errors.is_empty() {
            Ok(Program::new(declarations))
        } else {
            Err(self.errors.clone())
        }
    }

    /// Parse a declaration (variable or function)
    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        if self.match_token(&TokenKind::Function) {
            self.function_decl()
        } else if self.is_type() {
            self.var_decl()
        } else {
            Err(self.error("Expected declaration"))
        }
    }

    /// Parse a variable declaration
    fn var_decl(&mut self) -> Result<Declaration, ParseError> {
        let type_ = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name")?;

        let initializer = if self.match_token(&TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        Ok(Declaration::Variable {
            name,
            type_,
            initializer,
        })
    }

    /// Parse a function declaration
    fn function_decl(&mut self) -> Result<Declaration, ParseError> {
        let name = self.consume_identifier("Expected function name")?;

        self.consume(&TokenKind::LeftParen, "Expected '(' after function name")?;

        let mut parameters = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.consume_identifier("Expected parameter name")?;
                parameters.push(Parameter::new(param_name, param_type));

                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expected ')' after parameters")?;

        self.consume(&TokenKind::LeftBrace, "Expected '{' before function body")?;

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&TokenKind::RightBrace, "Expected '}' after function body")?;

        Ok(Declaration::Procedure {
            name,
            parameters,
            body,
        })
    }

    /// Parse a type
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let base_type = if self.match_token(&TokenKind::Dec) {
            self.consume(&TokenKind::LeftParen, "Expected '(' after 'dec'")?;
            let precision = self.consume_number("Expected precision")?;
            self.consume(&TokenKind::Comma, "Expected ',' after precision")?;
            let scale = self.consume_number("Expected scale")?;
            self.consume(&TokenKind::RightParen, "Expected ')' after scale")?;

            Type::decimal(precision as u8, scale as u8)
        } else if self.match_token(&TokenKind::Text) {
            self.consume(&TokenKind::LeftParen, "Expected '(' after 'text'")?;
            let length = self.consume_number("Expected length")?;
            self.consume(&TokenKind::RightParen, "Expected ')' after length")?;

            Type::text(length as u16)
        } else if self.match_token(&TokenKind::Int) {
            Type::Integer
        } else if self.match_token(&TokenKind::Bool) {
            Type::Boolean
        } else {
            return Err(self.error("Expected type"));
        };

        // Check for array syntax: type[size]
        if self.match_token(&TokenKind::LeftBracket) {
            let size = self.consume_number("Expected array size")?;
            self.consume(&TokenKind::RightBracket, "Expected ']' after array size")?;

            Ok(Type::Array {
                element_type: Box::new(base_type),
                size: size as usize,
            })
        } else {
            Ok(base_type)
        }
    }

    /// Parse a statement
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let line = self.peek().line;
        let column = self.peek().column;

        let kind = if self.match_token(&TokenKind::Set) {
            self.assign_stmt()?
        } else if self.match_token(&TokenKind::Call) {
            self.call_stmt()?
        } else if self.match_token(&TokenKind::Print) {
            self.print_stmt()?
        } else if self.match_token(&TokenKind::Accept) {
            self.accept_stmt()?
        } else if self.match_token(&TokenKind::If) {
            self.if_stmt()?
        } else if self.match_token(&TokenKind::Evaluate) {
            self.evaluate_stmt()?
        } else if self.match_token(&TokenKind::While) {
            self.while_stmt()?
        } else if self.match_token(&TokenKind::For) {
            self.for_stmt()?
        } else if self.match_token(&TokenKind::Return) {
            self.return_stmt()?
        } else if self.is_type() {
            // Local variable declaration
            let type_ = self.parse_type()?;
            let name = self.consume_identifier("Expected variable name")?;
            let initializer = if self.match_token(&TokenKind::Equal) {
                Some(self.expression()?)
            } else {
                None
            };
            StmtKind::VarDecl {
                name,
                type_,
                initializer,
            }
        } else {
            return Err(self.error("Expected statement"));
        };

        Ok(Stmt::new(kind, line, column))
    }

    fn assign_stmt(&mut self) -> Result<StmtKind, ParseError> {
        // Parse the assignment target (variable or array element)
        let name_token = self.peek().clone();
        let name = self.consume_identifier("Expected variable name")?;

        let target = if self.match_token(&TokenKind::LeftBracket) {
            // Array element assignment: arr[index] = value
            let index = self.expression()?;
            self.consume(&TokenKind::RightBracket, "Expected ']' after array index")?;

            Expr::new(
                ExprKind::Index {
                    array: Box::new(Expr::new(
                        ExprKind::Variable(name),
                        name_token.line,
                        name_token.column,
                    )),
                    index: Box::new(index),
                },
                name_token.line,
                name_token.column,
            )
        } else {
            // Simple variable assignment
            Expr::new(
                ExprKind::Variable(name),
                name_token.line,
                name_token.column,
            )
        };

        self.consume(&TokenKind::Equal, "Expected '=' in assignment")?;
        let value = self.expression()?;

        Ok(StmtKind::Assign { target, value })
    }

    fn call_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let name = self.consume_identifier("Expected procedure name")?;
        self.consume(&TokenKind::LeftParen, "Expected '(' after procedure name")?;

        let mut arguments = Vec::new();
        if !self.check(&TokenKind::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_token(&TokenKind::Comma) {
                    break;
                }
            }
        }

        self.consume(&TokenKind::RightParen, "Expected ')' after arguments")?;

        Ok(StmtKind::Call { name, arguments })
    }

    fn print_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let mut values = Vec::new();

        // Parse first value
        values.push(self.expression()?);

        // Parse additional values separated by commas
        while self.match_token(&TokenKind::Comma) {
            values.push(self.expression()?);
        }

        Ok(StmtKind::Print { values })
    }

    fn if_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let condition = self.expression()?;
        self.consume(&TokenKind::LeftBrace, "Expected '{' after if condition")?;

        let mut then_branch = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            then_branch.push(self.statement()?);
        }

        self.consume(&TokenKind::RightBrace, "Expected '}' after if body")?;

        let mut elif_branches = Vec::new();
        while self.match_token(&TokenKind::Elif) {
            let elif_condition = self.expression()?;
            self.consume(&TokenKind::LeftBrace, "Expected '{' after elif condition")?;

            let mut elif_body = Vec::new();
            while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                elif_body.push(self.statement()?);
            }

            self.consume(&TokenKind::RightBrace, "Expected '}' after elif body")?;

            elif_branches.push((elif_condition, elif_body));
        }

        let else_branch = if self.match_token(&TokenKind::Else) {
            self.consume(&TokenKind::LeftBrace, "Expected '{' after else")?;

            let mut else_body = Vec::new();
            while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                else_body.push(self.statement()?);
            }

            self.consume(&TokenKind::RightBrace, "Expected '}' after else body")?;

            Some(else_body)
        } else {
            None
        };

        Ok(StmtKind::If {
            condition,
            then_branch,
            elif_branches,
            else_branch,
        })
    }

    fn while_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let condition = self.expression()?;
        self.consume(&TokenKind::LeftBrace, "Expected '{' after while condition")?;

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&TokenKind::RightBrace, "Expected '}' after while body")?;

        Ok(StmtKind::While { condition, body })
    }

    fn for_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let variable = self.consume_identifier("Expected loop variable")?;
        self.consume(&TokenKind::Equal, "Expected '=' after loop variable")?;
        let start = self.expression()?;
        self.consume(&TokenKind::To, "Expected 'to' after start value")?;
        let end = self.expression()?;
        self.consume(&TokenKind::Step, "Expected 'step' after end value")?;
        let step = self.expression()?;
        self.consume(&TokenKind::LeftBrace, "Expected '{' after for loop")?;

        let mut body = Vec::new();
        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            body.push(self.statement()?);
        }

        self.consume(&TokenKind::RightBrace, "Expected '}' after for body")?;

        Ok(StmtKind::For {
            variable,
            start,
            end,
            step,
            body,
        })
    }

    fn return_stmt(&mut self) -> Result<StmtKind, ParseError> {
        let value = if !self.is_at_end() && !self.check(&TokenKind::RightBrace) {
            Some(self.expression()?)
        } else {
            None
        };

        Ok(StmtKind::Return { value })
    }

    fn evaluate_stmt(&mut self) -> Result<StmtKind, ParseError> {
        use coba_ast::stmt::{WhenBranch, WhenCondition};

        let subject = self.expression()?;
        self.consume(&TokenKind::LeftBrace, "Expected '{' after evaluate expression")?;

        let mut when_branches = Vec::new();
        let mut other_branch = None;

        while !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
            if self.match_token(&TokenKind::When) {
                let condition = if self.match_token(&TokenKind::Other) {
                    // "when other" case
                    self.consume(&TokenKind::Colon, "Expected ':' after 'other'")?;
                    let mut body = Vec::new();
                    while !self.check(&TokenKind::When) && !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                        body.push(self.statement()?);
                    }
                    other_branch = Some(body);
                    continue;
                } else {
                    // Parse value or range
                    let start = self.expression()?;
                    if self.match_token(&TokenKind::DotDot) {
                        // Range: when 10..20
                        let end = self.expression()?;
                        WhenCondition::Range(start, end)
                    } else {
                        // Single value: when 1
                        WhenCondition::Value(start)
                    }
                };

                self.consume(&TokenKind::Colon, "Expected ':' after when condition")?;

                let mut body = Vec::new();
                while !self.check(&TokenKind::When) && !self.check(&TokenKind::RightBrace) && !self.is_at_end() {
                    body.push(self.statement()?);
                }

                when_branches.push(WhenBranch { condition, body });
            } else {
                return Err(self.error("Expected 'when' in evaluate block"));
            }
        }

        self.consume(&TokenKind::RightBrace, "Expected '}' after evaluate block")?;

        Ok(StmtKind::Evaluate {
            subject,
            when_branches,
            other_branch,
        })
    }

    fn accept_stmt(&mut self) -> Result<StmtKind, ParseError> {
        use coba_ast::stmt::AcceptSource;

        let variable = self.consume_identifier("Expected variable name after 'accept'")?;

        let source = if self.match_token(&TokenKind::From) {
            let source_token = self.peek().clone();
            let source_name = self.consume_identifier("Expected source after 'from'")?;

            match source_name.as_str() {
                "date" => AcceptSource::Date,
                "time" => AcceptSource::Time,
                "day" => AcceptSource::Day,
                "dayofweek" => AcceptSource::DayOfWeek,
                _ => {
                    return Err(ParseError::new(
                        format!("Unknown accept source: {}", source_name),
                        source_token.line,
                        source_token.column,
                    ))
                }
            }
        } else {
            AcceptSource::User
        };

        Ok(StmtKind::Accept { variable, source })
    }

    /// Parse an expression
    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while self.match_token(&TokenKind::PipePipe) {
            let line = self.previous().line;
            let column = self.previous().column;
            let right = self.logical_and()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator: BinaryOp::Or,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.match_token(&TokenKind::AmpersandAmpersand) {
            let line = self.previous().line;
            let column = self.previous().column;
            let right = self.equality()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator: BinaryOp::And,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        while let Some(op) = self.match_tokens(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
            let line = self.previous().line;
            let column = self.previous().column;
            let operator = match op {
                TokenKind::EqualEqual => BinaryOp::Equal,
                TokenKind::BangEqual => BinaryOp::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        while let Some(op) = self.match_tokens(&[
            TokenKind::Less,
            TokenKind::LessEqual,
            TokenKind::Greater,
            TokenKind::GreaterEqual,
        ]) {
            let line = self.previous().line;
            let column = self.previous().column;
            let operator = match op {
                TokenKind::Less => BinaryOp::Less,
                TokenKind::LessEqual => BinaryOp::LessEqual,
                TokenKind::Greater => BinaryOp::Greater,
                TokenKind::GreaterEqual => BinaryOp::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        while let Some(op) = self.match_tokens(&[TokenKind::Plus, TokenKind::Minus]) {
            let line = self.previous().line;
            let column = self.previous().column;
            let operator = match op {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Subtract,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.power()?;

        while let Some(op) = self.match_tokens(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
            let line = self.previous().line;
            let column = self.previous().column;
            let operator = match op {
                TokenKind::Star => BinaryOp::Multiply,
                TokenKind::Slash => BinaryOp::Divide,
                TokenKind::Percent => BinaryOp::Modulo,
                _ => unreachable!(),
            };
            let right = self.power()?;
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn power(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        // Right-associative: 2**3**2 = 2**(3**2)
        if self.match_token(&TokenKind::StarStar) {
            let line = self.previous().line;
            let column = self.previous().column;
            let right = self.power()?; // Recursive call for right-associativity
            expr = Expr::new(
                ExprKind::Binary {
                    left: Box::new(expr),
                    operator: BinaryOp::Power,
                    right: Box::new(right),
                },
                line,
                column,
            );
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(op) = self.match_tokens(&[TokenKind::Bang, TokenKind::Minus]) {
            let line = self.previous().line;
            let column = self.previous().column;
            let operator = match op {
                TokenKind::Bang => UnaryOp::Not,
                TokenKind::Minus => UnaryOp::Negate,
                _ => unreachable!(),
            };
            let operand = self.unary()?;
            return Ok(Expr::new(
                ExprKind::Unary {
                    operator,
                    operand: Box::new(operand),
                },
                line,
                column,
            ));
        }

        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(&TokenKind::LeftBracket) {
                // Array indexing
                let index = self.expression()?;
                let line = self.previous().line;
                let column = self.previous().column;
                self.consume(&TokenKind::RightBracket, "Expected ']' after array index")?;

                expr = Expr::new(
                    ExprKind::Index {
                        array: Box::new(expr),
                        index: Box::new(index),
                    },
                    line,
                    column,
                );
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek().clone();

        if self.match_token(&TokenKind::Number) {
            let value = token.lexeme.parse::<f64>()
                .map_err(|_| ParseError::new("Invalid number".to_string(), token.line, token.column))?;
            Ok(Expr::new(ExprKind::Number(value), token.line, token.column))
        } else if self.match_token(&TokenKind::String) {
            // Remove quotes
            let value = token.lexeme[1..token.lexeme.len() - 1].to_string();
            Ok(Expr::new(ExprKind::String(value), token.line, token.column))
        } else if self.match_token(&TokenKind::True) {
            Ok(Expr::new(ExprKind::Boolean(true), token.line, token.column))
        } else if self.match_token(&TokenKind::False) {
            Ok(Expr::new(ExprKind::Boolean(false), token.line, token.column))
        } else if self.match_token(&TokenKind::Identifier) {
            Ok(Expr::new(ExprKind::Variable(token.lexeme.clone()), token.line, token.column))
        } else if self.match_token(&TokenKind::LeftParen) {
            let expr = self.expression()?;
            self.consume(&TokenKind::RightParen, "Expected ')' after expression")?;
            Ok(expr)
        } else {
            Err(self.error("Expected expression"))
        }
    }

    // Helper methods

    fn is_type(&self) -> bool {
        matches!(
            self.peek().kind,
            TokenKind::Dec | TokenKind::Text | TokenKind::Int | TokenKind::Bool
        )
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tokens(&mut self, kinds: &[TokenKind]) -> Option<TokenKind> {
        for kind in kinds {
            if self.check(kind) {
                let matched = kind.clone();
                self.advance();
                return Some(matched);
            }
        }
        None
    }

    fn check(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().kind) == std::mem::discriminant(kind)
        }
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, kind: &TokenKind, message: &str) -> Result<(), ParseError> {
        if self.check(kind) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(message))
        }
    }

    fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        if self.check(&TokenKind::Identifier) {
            let name = self.peek().lexeme.clone();
            self.advance();
            Ok(name)
        } else {
            Err(self.error(message))
        }
    }

    fn consume_number(&mut self, message: &str) -> Result<f64, ParseError> {
        if self.check(&TokenKind::Number) {
            let token = self.peek();
            let value = token.lexeme.parse::<f64>()
                .map_err(|_| ParseError::new("Invalid number".to_string(), token.line, token.column))?;
            self.advance();
            Ok(value)
        } else {
            Err(self.error(message))
        }
    }

    fn error(&self, message: &str) -> ParseError {
        let token = self.peek();
        ParseError::new(message.to_string(), token.line, token.column)
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == TokenKind::RightBrace {
                return;
            }

            match self.peek().kind {
                TokenKind::Function
                | TokenKind::Dec
                | TokenKind::Text
                | TokenKind::Int
                | TokenKind::Bool => return,
                _ => {}
            }

            self.advance();
        }
    }
}
