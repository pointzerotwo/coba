/// Type checking for Coba expressions and statements

use coba_ast::{
    expr::{BinaryOp, Expr, ExprKind, UnaryOp},
    types::Type,
};

use crate::symbol_table::SymbolTable;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable(String),
    UndefinedFunction(String),
    TypeMismatch {
        expected: Type,
        found: Type,
    },
    InvalidOperation {
        op: String,
        left: Type,
        right: Type,
    },
    InvalidUnaryOperation {
        op: String,
        operand: Type,
    },
    WrongArgumentCount {
        function: String,
        expected: usize,
        got: usize,
    },
    InvalidArgumentType {
        function: String,
        argument: usize,
        expected: Type,
        got: Type,
    },
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            TypeError::UndefinedFunction(name) => write!(f, "Undefined function: {}", name),
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {:?}, found {:?}", expected, found)
            }
            TypeError::InvalidOperation { op, left, right } => {
                write!(f, "Invalid operation {} for types {:?} and {:?}", op, left, right)
            }
            TypeError::InvalidUnaryOperation { op, operand } => {
                write!(f, "Invalid unary operation {} for type {:?}", op, operand)
            }
            TypeError::WrongArgumentCount { function, expected, got } => {
                write!(f, "Wrong number of arguments to function {}: expected {}, got {}", function, expected, got)
            }
            TypeError::InvalidArgumentType { function, argument, expected, got } => {
                write!(f, "Invalid type for argument {} to function {}: expected {:?}, got {:?}", argument, function, expected, got)
            }
        }
    }
}

pub struct TypeChecker<'a> {
    symbol_table: &'a SymbolTable,
}

impl<'a> TypeChecker<'a> {
    pub fn new(symbol_table: &'a SymbolTable) -> Self {
        Self { symbol_table }
    }

    /// Infer the type of an expression
    pub fn check_expr(&self, expr: &Expr) -> Result<Type, TypeError> {
        match &expr.kind {
            ExprKind::Number(_) => {
                // Default to decimal(9,2) for numeric literals
                Ok(Type::decimal(9, 2))
            }

            ExprKind::String(_) => {
                // Default to text(50) for string literals
                Ok(Type::text(50))
            }

            ExprKind::Boolean(_) => Ok(Type::Boolean),

            ExprKind::Variable(name) => {
                let symbol = self.symbol_table.lookup(name)
                    .ok_or_else(|| TypeError::UndefinedVariable(name.clone()))?;
                Ok(symbol.type_.clone())
            }

            ExprKind::Binary { left, operator, right } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;

                match operator {
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo | BinaryOp::Power => {
                        // Arithmetic operations - both operands must be numeric
                        if !is_numeric(&left_type) || !is_numeric(&right_type) {
                            return Err(TypeError::InvalidOperation {
                                op: operator.to_str().to_string(),
                                left: left_type,
                                right: right_type,
                            });
                        }
                        // Result type is the wider of the two
                        Ok(widen_type(&left_type, &right_type))
                    }

                    BinaryOp::Equal | BinaryOp::NotEqual => {
                        // Comparison operations - types must be compatible
                        if !types_compatible(&left_type, &right_type) {
                            return Err(TypeError::InvalidOperation {
                                op: operator.to_str().to_string(),
                                left: left_type,
                                right: right_type,
                            });
                        }
                        Ok(Type::Boolean)
                    }

                    BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                        // Relational operations - both must be numeric
                        if !is_numeric(&left_type) || !is_numeric(&right_type) {
                            return Err(TypeError::InvalidOperation {
                                op: operator.to_str().to_string(),
                                left: left_type,
                                right: right_type,
                            });
                        }
                        Ok(Type::Boolean)
                    }

                    BinaryOp::And | BinaryOp::Or => {
                        // Logical operations - both must be boolean
                        if left_type != Type::Boolean || right_type != Type::Boolean {
                            return Err(TypeError::InvalidOperation {
                                op: operator.to_str().to_string(),
                                left: left_type,
                                right: right_type,
                            });
                        }
                        Ok(Type::Boolean)
                    }
                }
            }

            ExprKind::Unary { operator, operand } => {
                let operand_type = self.check_expr(operand)?;

                match operator {
                    UnaryOp::Negate => {
                        if !is_numeric(&operand_type) {
                            return Err(TypeError::InvalidUnaryOperation {
                                op: operator.to_str().to_string(),
                                operand: operand_type,
                            });
                        }
                        Ok(operand_type)
                    }

                    UnaryOp::Not => {
                        if operand_type != Type::Boolean {
                            return Err(TypeError::InvalidUnaryOperation {
                                op: operator.to_str().to_string(),
                                operand: operand_type,
                            });
                        }
                        Ok(Type::Boolean)
                    }
                }
            }

            ExprKind::Call { .. } => {
                // Procedures don't return values in current spec
                Ok(Type::integer()) // Placeholder
            }

            ExprKind::FunctionCall { name, arguments } => {
                use coba_ast::intrinsics;

                // Look up the intrinsic function
                let func = intrinsics::lookup_intrinsic(name)
                    .ok_or_else(|| TypeError::UndefinedFunction(name.clone()))?;

                // Check argument count
                let arg_count = arguments.len();
                if arg_count < func.min_args {
                    return Err(TypeError::WrongArgumentCount {
                        function: name.clone(),
                        expected: func.min_args,
                        got: arg_count,
                    });
                }
                if let Some(max_args) = func.max_args {
                    if arg_count > max_args {
                        return Err(TypeError::WrongArgumentCount {
                            function: name.clone(),
                            expected: max_args,
                            got: arg_count,
                        });
                    }
                }

                // Type check each argument
                for (i, arg) in arguments.iter().enumerate() {
                    let arg_type = self.check_expr(arg)?;

                    // Get expected parameter type (use first parameter type for variadic functions)
                    let param_type = if i < func.parameter_types.len() {
                        &func.parameter_types[i]
                    } else if !func.parameter_types.is_empty() {
                        &func.parameter_types[0]
                    } else {
                        continue; // No type checking for zero-parameter functions
                    };

                    // Check type compatibility (relaxed for numeric types)
                    if !types_compatible(param_type, &arg_type) {
                        return Err(TypeError::InvalidArgumentType {
                            function: name.clone(),
                            argument: i + 1,
                            expected: param_type.clone(),
                            got: arg_type,
                        });
                    }
                }

                Ok(func.return_type.clone())
            }

            ExprKind::Index { array, index } => {
                let array_type = self.check_expr(array)?;
                let index_type = self.check_expr(index)?;

                // Index must be numeric (integer)
                if !is_numeric(&index_type) {
                    return Err(TypeError::InvalidOperation {
                        op: "[]".to_string(),
                        left: array_type.clone(),
                        right: index_type,
                    });
                }

                // Array must be an Array type
                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => Err(TypeError::InvalidUnaryOperation {
                        op: "[]".to_string(),
                        operand: array_type,
                    }),
                }
            }
        }
    }
}

/// Check if a type is numeric
fn is_numeric(type_: &Type) -> bool {
    matches!(type_, Type::Decimal { .. } | Type::Integer { .. })
}

/// Check if two types are compatible for comparison
fn types_compatible(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Decimal { .. }, Type::Decimal { .. }) => true,
        (Type::Decimal { .. }, Type::Integer { .. }) => true,
        (Type::Integer { .. }, Type::Decimal { .. }) => true,
        (Type::Integer { .. }, Type::Integer { .. }) => true,
        (Type::Text { .. }, Type::Text { .. }) => true,
        (Type::Boolean, Type::Boolean) => true,
        _ => false,
    }
}

/// Widen two numeric types to their common type
fn widen_type(left: &Type, right: &Type) -> Type {
    match (left, right) {
        (Type::Decimal { precision: p1, scale: s1, .. }, Type::Decimal { precision: p2, scale: s2, .. }) => {
            Type::decimal(*p1.max(p2), *s1.max(s2))
        }
        (Type::Decimal { .. }, Type::Integer { .. }) => left.clone(),
        (Type::Integer { .. }, Type::Decimal { .. }) => right.clone(),
        (Type::Integer { .. }, Type::Integer { .. }) => left.clone(),
        _ => left.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use coba_ast::expr::{Expr, ExprKind};

    #[test]
    fn test_number_literal() {
        let symbol_table = SymbolTable::new();
        let checker = TypeChecker::new(&symbol_table);

        let expr = Expr::new(ExprKind::Number(42.0), 1, 1);
        let type_ = checker.check_expr(&expr).unwrap();

        assert_eq!(type_, Type::decimal(9, 2));
    }

    #[test]
    fn test_boolean_literal() {
        let symbol_table = SymbolTable::new();
        let checker = TypeChecker::new(&symbol_table);

        let expr = Expr::new(ExprKind::Boolean(true), 1, 1);
        let type_ = checker.check_expr(&expr).unwrap();

        assert_eq!(type_, Type::Boolean);
    }

    #[test]
    fn test_arithmetic_operation() {
        let symbol_table = SymbolTable::new();
        let checker = TypeChecker::new(&symbol_table);

        let left = Expr::new(ExprKind::Number(10.0), 1, 1);
        let right = Expr::new(ExprKind::Number(20.0), 1, 1);
        let expr = Expr::new(
            ExprKind::Binary {
                left: Box::new(left),
                operator: BinaryOp::Add,
                right: Box::new(right),
            },
            1,
            1,
        );

        let type_ = checker.check_expr(&expr).unwrap();
        assert_eq!(type_, Type::decimal(9, 2));
    }
}
