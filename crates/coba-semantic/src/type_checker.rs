/// Type checking for Coba expressions and statements

use coba_ast::{
    expr::{BinaryOp, Expr, ExprKind, UnaryOp},
    types::Type,
};

use crate::symbol_table::SymbolTable;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable(String),
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
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {:?}, found {:?}", expected, found)
            }
            TypeError::InvalidOperation { op, left, right } => {
                write!(f, "Invalid operation {} for types {:?} and {:?}", op, left, right)
            }
            TypeError::InvalidUnaryOperation { op, operand } => {
                write!(f, "Invalid unary operation {} for type {:?}", op, operand)
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
                Ok(Type::Integer) // Placeholder
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
    matches!(type_, Type::Decimal { .. } | Type::Integer)
}

/// Check if two types are compatible for comparison
fn types_compatible(left: &Type, right: &Type) -> bool {
    match (left, right) {
        (Type::Decimal { .. }, Type::Decimal { .. }) => true,
        (Type::Decimal { .. }, Type::Integer) => true,
        (Type::Integer, Type::Decimal { .. }) => true,
        (Type::Integer, Type::Integer) => true,
        (Type::Text { .. }, Type::Text { .. }) => true,
        (Type::Boolean, Type::Boolean) => true,
        _ => false,
    }
}

/// Widen two numeric types to their common type
fn widen_type(left: &Type, right: &Type) -> Type {
    match (left, right) {
        (Type::Decimal { precision: p1, scale: s1 }, Type::Decimal { precision: p2, scale: s2 }) => {
            Type::decimal(*p1.max(p2), *s1.max(s2))
        }
        (Type::Decimal { .. }, Type::Integer) => left.clone(),
        (Type::Integer, Type::Decimal { .. }) => right.clone(),
        (Type::Integer, Type::Integer) => Type::Integer,
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
