/// Symbol table for managing variable scopes and declarations

use coba_ast::types::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub type_: Type,
    pub scope: SymbolScope,
    pub mangled_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolScope {
    Global,
    Local(String), // procedure name
    Parameter(String, usize), // procedure name, parameter index
}

impl Symbol {
    pub fn new(name: String, type_: Type, scope: SymbolScope, mangled_name: String) -> Self {
        Self {
            name,
            type_,
            scope,
            mangled_name,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    /// Global symbols
    globals: HashMap<String, Symbol>,

    /// Current scope (procedure name, if any)
    current_scope: Option<String>,

    /// Scoped symbols (procedure_name -> symbol_name -> Symbol)
    scoped: HashMap<String, HashMap<String, Symbol>>,

    /// Procedure signatures (name -> parameters)
    procedures: HashMap<String, Vec<(String, Type)>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            current_scope: None,
            scoped: HashMap::new(),
            procedures: HashMap::new(),
        }
    }

    /// Enter a procedure scope
    pub fn enter_scope(&mut self, procedure_name: String) {
        self.current_scope = Some(procedure_name.clone());
        self.scoped.entry(procedure_name).or_insert_with(HashMap::new);
    }

    /// Exit the current scope
    pub fn exit_scope(&mut self) {
        self.current_scope = None;
    }

    /// Add a global variable
    pub fn add_global(&mut self, name: String, type_: Type) -> Result<String, String> {
        if self.globals.contains_key(&name) {
            return Err(format!("Variable '{}' already declared in global scope", name));
        }

        let mangled_name = format!("GLOBAL-{}", name.to_uppercase().replace('_', "-"));
        let symbol = Symbol::new(name.clone(), type_, SymbolScope::Global, mangled_name.clone());
        self.globals.insert(name, symbol);

        Ok(mangled_name)
    }

    /// Add a local variable to the current scope
    pub fn add_local(&mut self, name: String, type_: Type) -> Result<String, String> {
        let proc_name = self.current_scope.as_ref()
            .ok_or("Not in a procedure scope")?;

        let scope_map = self.scoped.get_mut(proc_name)
            .ok_or("Scope map not found")?;

        if scope_map.contains_key(&name) {
            return Err(format!("Variable '{}' already declared in procedure '{}'", name, proc_name));
        }

        let mangled_name = format!("LOCAL-{}-{}",
            proc_name.to_uppercase().replace('_', "-"),
            name.to_uppercase().replace('_', "-")
        );

        let symbol = Symbol::new(
            name.clone(),
            type_,
            SymbolScope::Local(proc_name.clone()),
            mangled_name.clone(),
        );

        scope_map.insert(name, symbol);
        Ok(mangled_name)
    }

    /// Add a parameter to a procedure
    pub fn add_parameter(&mut self, proc_name: &str, index: usize, name: String, type_: Type) -> Result<String, String> {
        let scope_map = self.scoped.get_mut(proc_name)
            .ok_or("Procedure scope not found")?;

        if scope_map.contains_key(&name) {
            return Err(format!("Parameter '{}' already declared in procedure '{}'", name, proc_name));
        }

        let mangled_name = format!("PARAM-{}-{}",
            proc_name.to_uppercase().replace('_', "-"),
            index
        );

        let symbol = Symbol::new(
            name.clone(),
            type_,
            SymbolScope::Parameter(proc_name.to_string(), index),
            mangled_name.clone(),
        );

        scope_map.insert(name, symbol);
        Ok(mangled_name)
    }

    /// Register a procedure signature
    pub fn add_procedure(&mut self, name: String, parameters: Vec<(String, Type)>) -> Result<(), String> {
        if self.procedures.contains_key(&name) {
            return Err(format!("Procedure '{}' already declared", name));
        }

        self.procedures.insert(name, parameters);
        Ok(())
    }

    /// Look up a symbol by name in current scope
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        // Check current scope first
        if let Some(proc_name) = &self.current_scope {
            if let Some(scope_map) = self.scoped.get(proc_name) {
                if let Some(symbol) = scope_map.get(name) {
                    return Some(symbol);
                }
            }
        }

        // Fall back to global scope
        self.globals.get(name)
    }

    /// Look up a procedure signature
    pub fn lookup_procedure(&self, name: &str) -> Option<&Vec<(String, Type)>> {
        self.procedures.get(name)
    }

    /// Get mangled name for a variable
    pub fn get_mangled_name(&self, name: &str) -> Option<String> {
        self.lookup(name).map(|s| s.mangled_name.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_global_variables() {
        let mut table = SymbolTable::new();

        let mangled = table.add_global("totalPrice".to_string(), Type::decimal(9, 2)).unwrap();
        assert_eq!(mangled, "GLOBAL-TOTALPRICE");

        let symbol = table.lookup("totalPrice").unwrap();
        assert_eq!(symbol.name, "totalPrice");
        assert_eq!(symbol.type_, Type::decimal(9, 2));
    }

    #[test]
    fn test_local_variables() {
        let mut table = SymbolTable::new();
        table.enter_scope("calculate".to_string());

        let mangled = table.add_local("result".to_string(), Type::decimal(5, 2)).unwrap();
        assert_eq!(mangled, "LOCAL-CALCULATE-RESULT");

        let symbol = table.lookup("result").unwrap();
        assert_eq!(symbol.name, "result");
    }

    #[test]
    fn test_parameters() {
        let mut table = SymbolTable::new();
        table.enter_scope("calculate".to_string());

        let mangled = table.add_parameter("calculate", 0, "amount".to_string(), Type::decimal(9, 2)).unwrap();
        assert_eq!(mangled, "PARAM-CALCULATE-0");

        let symbol = table.lookup("amount").unwrap();
        assert_eq!(symbol.name, "amount");
    }

    #[test]
    fn test_scope_shadowing() {
        let mut table = SymbolTable::new();

        // Global variable
        table.add_global("value".to_string(), Type::Integer).unwrap();

        // Enter scope and add local with same name
        table.enter_scope("test".to_string());
        table.add_local("value".to_string(), Type::decimal(5, 2)).unwrap();

        // Local should shadow global
        let symbol = table.lookup("value").unwrap();
        match &symbol.scope {
            SymbolScope::Local(_) => {},
            _ => panic!("Expected local scope"),
        }

        // Exit scope - should see global again
        table.exit_scope();
        let symbol = table.lookup("value").unwrap();
        assert!(matches!(symbol.scope, SymbolScope::Global));
    }
}
