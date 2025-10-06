/// Intrinsic function definitions for Coba
/// Maps to COBOL intrinsic functions

use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct IntrinsicFunction {
    pub name: &'static str,
    pub parameter_types: Vec<Type>,
    pub return_type: Type,
    pub min_args: usize,
    pub max_args: Option<usize>, // None = variadic
    pub cobol_name: &'static str,
}

impl IntrinsicFunction {
    pub fn new(
        name: &'static str,
        parameter_types: Vec<Type>,
        return_type: Type,
        cobol_name: &'static str,
    ) -> Self {
        let arg_count = parameter_types.len();
        Self {
            name,
            parameter_types,
            return_type,
            min_args: arg_count,
            max_args: Some(arg_count),
            cobol_name,
        }
    }

    pub fn variadic(
        name: &'static str,
        parameter_types: Vec<Type>,
        return_type: Type,
        min_args: usize,
        cobol_name: &'static str,
    ) -> Self {
        Self {
            name,
            parameter_types,
            return_type,
            min_args,
            max_args: None,
            cobol_name,
        }
    }
}

/// Get all intrinsic functions
pub fn get_intrinsic_functions() -> Vec<IntrinsicFunction> {
    vec![
        // String Functions
        IntrinsicFunction::new(
            "length",
            vec![Type::text(9999)],
            Type::integer(),
            "LENGTH",
        ),
        IntrinsicFunction::new(
            "upper_case",
            vec![Type::text(9999)],
            Type::text(9999),
            "UPPER-CASE",
        ),
        IntrinsicFunction::new(
            "lower_case",
            vec![Type::text(9999)],
            Type::text(9999),
            "LOWER-CASE",
        ),
        IntrinsicFunction::new(
            "reverse",
            vec![Type::text(9999)],
            Type::text(9999),
            "REVERSE",
        ),
        IntrinsicFunction::new(
            "trim",
            vec![Type::text(9999)],
            Type::text(9999),
            "TRIM",
        ),

        // Numeric Conversion
        IntrinsicFunction::new(
            "numval",
            vec![Type::text(9999)],
            Type::decimal(18, 2),
            "NUMVAL",
        ),
        IntrinsicFunction::new(
            "numval_c",
            vec![Type::text(9999)],
            Type::decimal(18, 2),
            "NUMVAL-C",
        ),
        IntrinsicFunction::new(
            "integer",
            vec![Type::decimal(18, 2)],
            Type::integer(),
            "INTEGER",
        ),
        IntrinsicFunction::new(
            "integer_part",
            vec![Type::decimal(18, 2)],
            Type::integer(),
            "INTEGER-PART",
        ),
        IntrinsicFunction::new(
            "abs",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            "ABS",
        ),

        // Math Functions
        IntrinsicFunction::variadic(
            "max",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            2,
            "MAX",
        ),
        IntrinsicFunction::variadic(
            "min",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            2,
            "MIN",
        ),
        IntrinsicFunction::new(
            "sqrt",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            "SQRT",
        ),
        IntrinsicFunction::new(
            "mod",
            vec![Type::decimal(18, 2), Type::decimal(18, 2)],
            Type::decimal(18, 2),
            "MOD",
        ),
        IntrinsicFunction::new(
            "rem",
            vec![Type::decimal(18, 2), Type::decimal(18, 2)],
            Type::decimal(18, 2),
            "REM",
        ),
        IntrinsicFunction::new(
            "random",
            vec![],
            Type::decimal(18, 15),
            "RANDOM",
        ),

        // Trigonometric Functions
        IntrinsicFunction::new(
            "sin",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "SIN",
        ),
        IntrinsicFunction::new(
            "cos",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "COS",
        ),
        IntrinsicFunction::new(
            "tan",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "TAN",
        ),
        IntrinsicFunction::new(
            "asin",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "ASIN",
        ),
        IntrinsicFunction::new(
            "acos",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "ACOS",
        ),
        IntrinsicFunction::new(
            "atan",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "ATAN",
        ),

        // Logarithmic Functions
        IntrinsicFunction::new(
            "log",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "LOG",
        ),
        IntrinsicFunction::new(
            "log10",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 15),
            "LOG10",
        ),
        IntrinsicFunction::new(
            "factorial",
            vec![Type::integer()],
            Type::integer(),
            "FACTORIAL",
        ),

        // Date/Time Functions
        IntrinsicFunction::new(
            "current_date",
            vec![],
            Type::text(21),
            "CURRENT-DATE",
        ),
        IntrinsicFunction::new(
            "when_compiled",
            vec![],
            Type::text(16),
            "WHEN-COMPILED",
        ),
        IntrinsicFunction::new(
            "date_of_integer",
            vec![Type::integer()],
            Type::integer(),
            "DATE-OF-INTEGER",
        ),
        IntrinsicFunction::new(
            "day_of_integer",
            vec![Type::integer()],
            Type::integer(),
            "DAY-OF-INTEGER",
        ),

        // Statistical Functions
        IntrinsicFunction::variadic(
            "sum",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "SUM",
        ),
        IntrinsicFunction::variadic(
            "mean",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "MEAN",
        ),
        IntrinsicFunction::variadic(
            "median",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "MEDIAN",
        ),
        IntrinsicFunction::variadic(
            "range",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "RANGE",
        ),
        IntrinsicFunction::variadic(
            "standard_deviation",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "STANDARD-DEVIATION",
        ),
        IntrinsicFunction::variadic(
            "variance",
            vec![Type::decimal(18, 2)],
            Type::decimal(18, 2),
            1,
            "VARIANCE",
        ),

        // Financial Functions
        IntrinsicFunction::new(
            "annuity",
            vec![Type::decimal(18, 2), Type::integer()],
            Type::decimal(18, 2),
            "ANNUITY",
        ),
        IntrinsicFunction::new(
            "present_value",
            vec![Type::decimal(18, 2), Type::integer()],
            Type::decimal(18, 2),
            "PRESENT-VALUE",
        ),

        // Character Functions
        IntrinsicFunction::new(
            "char",
            vec![Type::integer()],
            Type::text(1),
            "CHAR",
        ),
        IntrinsicFunction::new(
            "ord",
            vec![Type::text(1)],
            Type::integer(),
            "ORD",
        ),
        IntrinsicFunction::variadic(
            "ord_max",
            vec![Type::text(1)],
            Type::integer(),
            1,
            "ORD-MAX",
        ),
        IntrinsicFunction::variadic(
            "ord_min",
            vec![Type::text(1)],
            Type::integer(),
            1,
            "ORD-MIN",
        ),
    ]
}

/// Look up an intrinsic function by name
pub fn lookup_intrinsic(name: &str) -> Option<&'static IntrinsicFunction> {
    // Convert to lowercase and replace hyphens with underscores for matching
    let normalized = name.to_lowercase().replace('-', "_");

    // This is a placeholder - in a real implementation, we'd use a lazy_static HashMap
    // for O(1) lookup. For now, we'll use linear search.
    get_intrinsic_functions()
        .iter()
        .find(|f| f.name == normalized)
        .map(|f| unsafe {
            // SAFETY: This is safe because IntrinsicFunction instances are statically allocated
            // in get_intrinsic_functions() and their lifetimes are 'static
            std::mem::transmute::<&IntrinsicFunction, &'static IntrinsicFunction>(f)
        })
}
