/// Name mangling utilities for COBOL reserved words and scoping

use std::collections::HashSet;

lazy_static::lazy_static! {
    /// COBOL reserved words that need to be escaped
    static ref COBOL_RESERVED_WORDS: HashSet<&'static str> = {
        let mut set = HashSet::new();

        // Common COBOL reserved words
        set.insert("ACCEPT");
        set.insert("ADD");
        set.insert("CALL");
        set.insert("COMPUTE");
        set.insert("DISPLAY");
        set.insert("DIVIDE");
        set.insert("ELSE");
        set.insert("END");
        set.insert("END-IF");
        set.insert("EXIT");
        set.insert("GO");
        set.insert("GOTO");
        set.insert("IF");
        set.insert("MOVE");
        set.insert("MULTIPLY");
        set.insert("PERFORM");
        set.insert("STOP");
        set.insert("SUBTRACT");
        set.insert("THEN");

        // Division and section names
        set.insert("IDENTIFICATION");
        set.insert("ENVIRONMENT");
        set.insert("DATA");
        set.insert("PROCEDURE");
        set.insert("WORKING-STORAGE");
        set.insert("LOCAL-STORAGE");
        set.insert("LINKAGE");
        set.insert("FILE");

        // Other keywords
        set.insert("PICTURE");
        set.insert("PIC");
        set.insert("VALUE");
        set.insert("OCCURS");
        set.insert("REDEFINES");
        set.insert("TO");
        set.insert("FROM");
        set.insert("BY");
        set.insert("GIVING");
        set.insert("UNTIL");
        set.insert("VARYING");
        set.insert("TIMES");

        set
    };
}

/// Check if a name is a COBOL reserved word
pub fn is_reserved_word(name: &str) -> bool {
    COBOL_RESERVED_WORDS.contains(name.to_uppercase().as_str())
}

/// Mangle a name to avoid COBOL reserved words
pub fn mangle_if_reserved(name: &str) -> String {
    if is_reserved_word(name) {
        format!("COBA-{}", name.to_uppercase().replace('_', "-"))
    } else {
        name.to_uppercase().replace('_', "-")
    }
}

/// Mangle a global variable name
pub fn mangle_global(name: &str) -> String {
    let base = mangle_if_reserved(name);
    format!("GLOBAL-{}", base)
}

/// Mangle a local variable name
pub fn mangle_local(proc_name: &str, var_name: &str) -> String {
    let proc = proc_name.to_uppercase().replace('_', "-");
    let var = mangle_if_reserved(var_name);
    format!("LOCAL-{}-{}", proc, var)
}

/// Mangle a parameter name
pub fn mangle_parameter(proc_name: &str, index: usize) -> String {
    let proc = proc_name.to_uppercase().replace('_', "-");
    format!("PARAM-{}-{}", proc, index)
}

/// Demangle a name (reverse the mangling)
pub fn demangle(name: &str) -> String {
    if let Some(rest) = name.strip_prefix("GLOBAL-") {
        demangle_name_part(rest)
    } else if let Some(rest) = name.strip_prefix("LOCAL-") {
        let parts: Vec<&str> = rest.splitn(2, '-').collect();
        if parts.len() == 2 {
            demangle_name_part(parts[1])
        } else {
            demangle_name_part(rest)
        }
    } else if let Some(rest) = name.strip_prefix("PARAM-") {
        let parts: Vec<&str> = rest.splitn(2, '-').collect();
        if parts.len() == 2 {
            format!("param_{}", parts[1])
        } else {
            demangle_name_part(rest)
        }
    } else if let Some(rest) = name.strip_prefix("COBA-") {
        demangle_name_part(rest)
    } else {
        demangle_name_part(name)
    }
}

fn demangle_name_part(name: &str) -> String {
    name.to_lowercase().replace('-', "_")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reserved_word_detection() {
        assert!(is_reserved_word("PERFORM"));
        assert!(is_reserved_word("perform"));
        assert!(is_reserved_word("Move"));
        assert!(!is_reserved_word("customer_name"));
    }

    #[test]
    fn test_mangle_reserved() {
        assert_eq!(mangle_if_reserved("PERFORM"), "COBA-PERFORM");
        assert_eq!(mangle_if_reserved("customer_name"), "CUSTOMER-NAME");
    }

    #[test]
    fn test_mangle_global() {
        assert_eq!(mangle_global("total_price"), "GLOBAL-TOTAL-PRICE");
        assert_eq!(mangle_global("PERFORM"), "GLOBAL-COBA-PERFORM");
    }

    #[test]
    fn test_mangle_local() {
        assert_eq!(mangle_local("calculate", "result"), "LOCAL-CALCULATE-RESULT");
    }

    #[test]
    fn test_mangle_parameter() {
        assert_eq!(mangle_parameter("calculate", 0), "PARAM-CALCULATE-0");
    }

    #[test]
    fn test_demangle() {
        assert_eq!(demangle("GLOBAL-TOTAL-PRICE"), "total_price");
        assert_eq!(demangle("LOCAL-CALCULATE-RESULT"), "result");
        assert_eq!(demangle("PARAM-CALCULATE-0"), "param_0");
        assert_eq!(demangle("COBA-PERFORM"), "perform");
    }
}
