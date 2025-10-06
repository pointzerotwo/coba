/// Type system for Coba

#[derive(Debug, Clone, PartialEq)]
pub enum StorageFormat {
    /// DISPLAY - character storage (default)
    Display,
    /// COMP / COMPUTATIONAL - binary storage
    Comp,
    /// COMP-3 / PACKED-DECIMAL - packed decimal storage
    Comp3,
}

impl Default for StorageFormat {
    fn default() -> Self {
        StorageFormat::Display
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// decimal(precision, scale) with optional storage format
    Decimal {
        precision: u8,
        scale: u8,
        storage: StorageFormat,
    },

    /// text(length)
    Text { length: u16 },

    /// integer (shorthand for decimal(9, 0)) with optional storage format
    Integer { storage: StorageFormat },

    /// boolean
    Boolean,

    /// Array of elements: int[10], dec(9,2)[50]
    Array {
        element_type: Box<Type>,
        size: usize,
    },
}

impl Type {
    /// Create a decimal type with default DISPLAY storage
    pub fn decimal(precision: u8, scale: u8) -> Self {
        Type::Decimal {
            precision,
            scale,
            storage: StorageFormat::Display,
        }
    }

    /// Create a decimal type with specified storage format
    pub fn decimal_with_storage(precision: u8, scale: u8, storage: StorageFormat) -> Self {
        Type::Decimal {
            precision,
            scale,
            storage,
        }
    }

    /// Create an integer type with default DISPLAY storage
    pub fn integer() -> Self {
        Type::Integer {
            storage: StorageFormat::Display,
        }
    }

    /// Create an integer type with specified storage format
    pub fn integer_with_storage(storage: StorageFormat) -> Self {
        Type::Integer { storage }
    }

    /// Create a text type
    pub fn text(length: u16) -> Self {
        Type::Text { length }
    }

    /// Get the maximum value for a decimal type
    pub fn max_decimal_value(&self) -> Option<f64> {
        match self {
            Type::Decimal { precision, scale, .. } => {
                let max = 10_f64.powi(*precision as i32) - 10_f64.powi(-(*scale as i32));
                Some(max)
            }
            Type::Integer { .. } => {
                Some(999_999_999.0)
            }
            Type::Array { element_type, .. } => {
                // Return max value of element type
                element_type.max_decimal_value()
            }
            _ => None,
        }
    }

    /// Convert to COBOL PICTURE clause with storage format
    pub fn to_cobol_picture(&self) -> String {
        match self {
            Type::Decimal { precision, scale, storage } => {
                let pic = if *scale == 0 {
                    format!("PIC 9({})", precision)
                } else {
                    let int_digits = precision - scale;
                    if int_digits == 0 {
                        format!("PIC V9({})", scale)
                    } else {
                        format!("PIC 9({})V9({})", int_digits, scale)
                    }
                };
                match storage {
                    StorageFormat::Display => pic,
                    StorageFormat::Comp => format!("{} COMP", pic),
                    StorageFormat::Comp3 => format!("{} COMP-3", pic),
                }
            }
            Type::Text { length } => {
                if *length == 1 {
                    "PIC X".to_string()
                } else {
                    format!("PIC X({})", length)
                }
            }
            Type::Integer { storage } => {
                let pic = "PIC 9(9)";
                match storage {
                    StorageFormat::Display => pic.to_string(),
                    StorageFormat::Comp => format!("{} COMP", pic),
                    StorageFormat::Comp3 => format!("{} COMP-3", pic),
                }
            }
            Type::Boolean => "PIC 9".to_string(),
            Type::Array { element_type, size } => {
                // For arrays, generate element type's picture with OCCURS
                format!("{} OCCURS {} TIMES", element_type.to_cobol_picture(), size)
            }
        }
    }

    /// Parse COBOL PICTURE clause to Coba type
    pub fn from_cobol_picture(picture: &str) -> Option<Self> {
        let pic = picture.trim().to_uppercase();

        // Check for storage format suffixes
        let (pic, storage) = if pic.ends_with("COMP-3") || pic.ends_with("PACKED-DECIMAL") {
            (pic.trim_end_matches("COMP-3").trim_end_matches("PACKED-DECIMAL").trim(), StorageFormat::Comp3)
        } else if pic.ends_with("COMP") || pic.ends_with("COMPUTATIONAL") {
            (pic.trim_end_matches("COMP").trim_end_matches("COMPUTATIONAL").trim(), StorageFormat::Comp)
        } else {
            (pic.as_str(), StorageFormat::Display)
        };

        // Remove "PIC" or "PICTURE" prefix
        let pic = pic.strip_prefix("PIC").or_else(|| pic.strip_prefix("PICTURE"))
            .unwrap_or(pic)
            .trim();

        // Match patterns
        if pic == "X" {
            Some(Type::text(1))
        } else if let Some(rest) = pic.strip_prefix("X(") {
            if let Some(len_str) = rest.strip_suffix(')') {
                if let Ok(len) = len_str.parse::<u16>() {
                    return Some(Type::text(len));
                }
            }
            None
        } else if pic == "9" {
            Some(Type::Boolean)
        } else if pic == "9(9)" {
            Some(Type::Integer { storage })
        } else if let Some(rest) = pic.strip_prefix("9(") {
            if let Some(digits_str) = rest.strip_suffix(')') {
                if let Ok(precision) = digits_str.parse::<u8>() {
                    return Some(Type::decimal_with_storage(precision, 0, storage));
                }
            }
            None
        } else if pic.contains('V') {
            // Handle decimal with V separator
            let parts: Vec<&str> = pic.split('V').collect();
            if parts.len() != 2 {
                return None;
            }

            let int_part = parts[0];
            let dec_part = parts[1];

            let int_digits = if int_part == "9" {
                1
            } else if let Some(rest) = int_part.strip_prefix("9(") {
                if let Some(d) = rest.strip_suffix(')') {
                    d.parse::<u8>().ok()?
                } else {
                    return None;
                }
            } else if int_part.is_empty() {
                0
            } else {
                return None;
            };

            let dec_digits = if dec_part == "9" {
                1
            } else if let Some(rest) = dec_part.strip_prefix("9(") {
                if let Some(d) = rest.strip_suffix(')') {
                    d.parse::<u8>().ok()?
                } else {
                    return None;
                }
            } else {
                return None;
            };

            Some(Type::decimal_with_storage(int_digits + dec_digits, dec_digits, storage))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_to_picture() {
        assert_eq!(Type::decimal(9, 2).to_cobol_picture(), "PIC 9(7)V9(2)");
        assert_eq!(Type::decimal(5, 0).to_cobol_picture(), "PIC 9(5)");
        assert_eq!(Type::decimal(3, 3).to_cobol_picture(), "PIC V9(3)");
    }

    #[test]
    fn test_text_to_picture() {
        assert_eq!(Type::text(50).to_cobol_picture(), "PIC X(50)");
        assert_eq!(Type::text(1).to_cobol_picture(), "PIC X");
    }

    #[test]
    fn test_from_cobol_picture() {
        assert_eq!(Type::from_cobol_picture("PIC 9(7)V9(2)"), Some(Type::decimal(9, 2)));
        assert_eq!(Type::from_cobol_picture("PIC X(50)"), Some(Type::text(50)));
        assert_eq!(Type::from_cobol_picture("PIC 9"), Some(Type::Boolean));
    }
}
