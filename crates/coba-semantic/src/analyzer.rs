/// Semantic analyzer for Coba programs

use coba_ast::{
    program::{Declaration, Program},
    stmt::{Stmt, StmtKind},
};

use crate::{
    symbol_table::SymbolTable,
    type_checker::{TypeChecker, TypeError},
};

#[derive(Debug)]
pub struct SemanticAnalyzer {
    pub symbol_table: SymbolTable,
    errors: Vec<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    /// Analyze a program and build symbol table
    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: collect all global variables and procedure signatures
        for decl in &program.declarations {
            match decl {
                Declaration::Variable { name, type_, .. } => {
                    if let Err(e) = self.symbol_table.add_global(name.clone(), type_.clone()) {
                        self.errors.push(e);
                    }
                }
                Declaration::Procedure { name, parameters, .. } => {
                    let params: Vec<(String, _)> = parameters
                        .iter()
                        .map(|p| (p.name.clone(), p.type_.clone()))
                        .collect();

                    if let Err(e) = self.symbol_table.add_procedure(name.clone(), params) {
                        self.errors.push(e);
                    }
                }
                Declaration::File { name, record_type, status_var, .. } => {
                    // Register file as a global
                    if let Err(e) = self.symbol_table.add_global(name.clone(), record_type.clone()) {
                        self.errors.push(e);
                    }
                    // Register status variable if present
                    if let Some(status) = status_var {
                        if let Err(e) = self.symbol_table.add_global(status.clone(), coba_ast::types::Type::text(2)) {
                            self.errors.push(e);
                        }
                    }
                }
            }
        }

        // Second pass: analyze procedure bodies
        for decl in &program.declarations {
            if let Declaration::Procedure { name, parameters, body } = decl {
                self.analyze_procedure(name, parameters, body);
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_procedure(
        &mut self,
        name: &str,
        parameters: &[coba_ast::program::Parameter],
        body: &[Stmt],
    ) {
        self.symbol_table.enter_scope(name.to_string());

        // Add parameters to scope
        for (index, param) in parameters.iter().enumerate() {
            if let Err(e) = self.symbol_table.add_parameter(
                name,
                index,
                param.name.clone(),
                param.type_.clone(),
            ) {
                self.errors.push(e);
            }
        }

        // Analyze statements
        for stmt in body {
            self.analyze_statement(stmt);
        }

        self.symbol_table.exit_scope();
    }

    fn analyze_statement(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::VarDecl { name, type_, initializer } => {
                // Add local variable
                if let Err(e) = self.symbol_table.add_local(name.clone(), type_.clone()) {
                    self.errors.push(e);
                }

                // Type check initializer if present
                if let Some(init_expr) = initializer {
                    let checker = TypeChecker::new(&self.symbol_table);
                    match checker.check_expr(init_expr) {
                        Ok(expr_type) => {
                            // Check type compatibility
                            if !types_compatible(type_, &expr_type) {
                                self.errors.push(format!(
                                    "Type mismatch in variable declaration '{}': expected {:?}, found {:?}",
                                    name, type_, expr_type
                                ));
                            }
                        }
                        Err(e) => {
                            self.errors.push(format!("Type error: {}", e));
                        }
                    }
                }
            }

            StmtKind::Assign { target, value } => {
                let checker = TypeChecker::new(&self.symbol_table);

                // Get the type of the target (variable or array element)
                match checker.check_expr(target) {
                    Ok(target_type) => {
                        // Type check the value
                        match checker.check_expr(value) {
                            Ok(value_type) => {
                                if !types_compatible(&target_type, &value_type) {
                                    self.errors.push(format!(
                                        "Type mismatch in assignment: expected {:?}, found {:?}",
                                        target_type, value_type
                                    ));
                                }
                            }
                            Err(e) => {
                                self.errors.push(format!("Type error: {}", e));
                            }
                        }
                    }
                    Err(e) => {
                        self.errors.push(format!("Type error in assignment target: {}", e));
                    }
                }
            }

            StmtKind::Call { name, arguments } => {
                // Check that procedure exists
                if let Some(params) = self.symbol_table.lookup_procedure(name) {
                    // Check argument count
                    if arguments.len() != params.len() {
                        self.errors.push(format!(
                            "Procedure '{}' expects {} arguments, got {}",
                            name,
                            params.len(),
                            arguments.len()
                        ));
                    } else {
                        // Type check arguments
                        let checker = TypeChecker::new(&self.symbol_table);
                        for (i, (arg, (_, param_type))) in arguments.iter().zip(params.iter()).enumerate() {
                            match checker.check_expr(arg) {
                                Ok(arg_type) => {
                                    if !types_compatible(param_type, &arg_type) {
                                        self.errors.push(format!(
                                            "Type mismatch in argument {} of call to '{}': expected {:?}, found {:?}",
                                            i + 1, name, param_type, arg_type
                                        ));
                                    }
                                }
                                Err(e) => {
                                    self.errors.push(format!("Type error: {}", e));
                                }
                            }
                        }
                    }
                } else {
                    self.errors.push(format!("Undefined procedure: {}", name));
                }
            }

            StmtKind::If { condition, then_branch, elif_branches, else_branch } => {
                // Check condition is boolean
                {
                    let checker = TypeChecker::new(&self.symbol_table);
                    match checker.check_expr(condition) {
                        Ok(cond_type) => {
                            if cond_type != coba_ast::types::Type::Boolean {
                                self.errors.push(format!(
                                    "If condition must be boolean, found {:?}",
                                    cond_type
                                ));
                            }
                        }
                        Err(e) => {
                            self.errors.push(format!("Type error: {}", e));
                        }
                    }
                }

                // Analyze branches
                for stmt in then_branch {
                    self.analyze_statement(stmt);
                }

                for (elif_cond, elif_body) in elif_branches {
                    {
                        let checker = TypeChecker::new(&self.symbol_table);
                        match checker.check_expr(elif_cond) {
                            Ok(cond_type) => {
                                if cond_type != coba_ast::types::Type::Boolean {
                                    self.errors.push(format!(
                                        "Elif condition must be boolean, found {:?}",
                                        cond_type
                                    ));
                                }
                            }
                            Err(e) => {
                                self.errors.push(format!("Type error: {}", e));
                            }
                        }
                    }

                    for stmt in elif_body {
                        self.analyze_statement(stmt);
                    }
                }

                if let Some(else_body) = else_branch {
                    for stmt in else_body {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::While { condition, body } => {
                // Check condition is boolean
                {
                    let checker = TypeChecker::new(&self.symbol_table);
                    match checker.check_expr(condition) {
                        Ok(cond_type) => {
                            if cond_type != coba_ast::types::Type::Boolean {
                                self.errors.push(format!(
                                    "While condition must be boolean, found {:?}",
                                    cond_type
                                ));
                            }
                        }
                        Err(e) => {
                            self.errors.push(format!("Type error: {}", e));
                        }
                    }
                }

                for stmt in body {
                    self.analyze_statement(stmt);
                }
            }

            StmtKind::For { body, .. } => {
                // TODO: Type check loop bounds
                for stmt in body {
                    self.analyze_statement(stmt);
                }
            }

            StmtKind::Print { values } => {
                // Type check each expression to be printed
                let checker = TypeChecker::new(&self.symbol_table);
                for value in values {
                    match checker.check_expr(value) {
                        Ok(_) => {}, // Any type is printable
                        Err(e) => {
                            self.errors.push(format!("Type error in print: {}", e));
                        }
                    }
                }
            }

            StmtKind::Return { .. } => {
                // TODO: Implement return type checking
            }

            StmtKind::Evaluate { subject, when_branches, other_branch } => {
                // Type check the subject expression
                {
                    let checker = TypeChecker::new(&self.symbol_table);
                    if let Err(e) = checker.check_expr(subject) {
                        self.errors.push(format!("Type error in evaluate subject: {}", e));
                    }
                }

                // Type check when branch conditions and bodies
                for branch in when_branches {
                    {
                        let checker = TypeChecker::new(&self.symbol_table);
                        match &branch.condition {
                            coba_ast::stmt::WhenCondition::Value(expr) => {
                                if let Err(e) = checker.check_expr(expr) {
                                    self.errors.push(format!("Type error in when condition: {}", e));
                                }
                            }
                            coba_ast::stmt::WhenCondition::Range(start, end) => {
                                if let Err(e) = checker.check_expr(start) {
                                    self.errors.push(format!("Type error in when range start: {}", e));
                                }
                                if let Err(e) = checker.check_expr(end) {
                                    self.errors.push(format!("Type error in when range end: {}", e));
                                }
                            }
                        }
                    }

                    for stmt in &branch.body {
                        self.analyze_statement(stmt);
                    }
                }

                // Type check other branch body
                if let Some(other_branch) = other_branch {
                    for stmt in other_branch {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::Accept { variable, .. } => {
                // Check that variable exists
                if self.symbol_table.lookup(variable).is_none() {
                    self.errors.push(format!("Undefined variable in accept: {}", variable));
                }
            }

            StmtKind::Initialize { variables } => {
                // Check that all variables exist
                for var in variables {
                    if self.symbol_table.lookup(var).is_none() {
                        self.errors.push(format!("Undefined variable in initialize: {}", var));
                    }
                }
            }

            StmtKind::Continue => {
                // No-op, no validation needed
            }

            StmtKind::Exit => {
                // No-op, no validation needed
            }

            StmtKind::StringConcat { sources, destination, pointer } => {
                let checker = TypeChecker::new(&self.symbol_table);

                // Check all source expressions
                for src in sources {
                    if let Err(e) = checker.check_expr(&src.value) {
                        self.errors.push(format!("Type error in string source: {}", e));
                    }
                    if let Some(delim) = &src.delimiter {
                        if let Err(e) = checker.check_expr(delim) {
                            self.errors.push(format!("Type error in string delimiter: {}", e));
                        }
                    }
                }

                // Check destination variable exists
                if self.symbol_table.lookup(destination).is_none() {
                    self.errors.push(format!("Undefined destination variable: {}", destination));
                }

                // Check pointer variable if present
                if let Some(ptr) = pointer {
                    if self.symbol_table.lookup(ptr).is_none() {
                        self.errors.push(format!("Undefined pointer variable: {}", ptr));
                    }
                }
            }

            StmtKind::StringSplit { source, delimiter, destinations, pointer, tallying } => {
                // Check source variable exists
                if self.symbol_table.lookup(source).is_none() {
                    self.errors.push(format!("Undefined source variable: {}", source));
                }

                // Check delimiter expression if present
                if let Some(delim) = delimiter {
                    let checker = TypeChecker::new(&self.symbol_table);
                    if let Err(e) = checker.check_expr(delim) {
                        self.errors.push(format!("Type error in unstring delimiter: {}", e));
                    }
                }

                // Check all destination variables exist
                for dest in destinations {
                    if self.symbol_table.lookup(dest).is_none() {
                        self.errors.push(format!("Undefined destination variable: {}", dest));
                    }
                }

                // Check pointer variable if present
                if let Some(ptr) = pointer {
                    if self.symbol_table.lookup(ptr).is_none() {
                        self.errors.push(format!("Undefined pointer variable: {}", ptr));
                    }
                }

                // Check tallying variable if present
                if let Some(tally) = tallying {
                    if self.symbol_table.lookup(tally).is_none() {
                        self.errors.push(format!("Undefined tallying variable: {}", tally));
                    }
                }
            }

            StmtKind::StringInspect { target, operation } => {
                use coba_ast::stmt::InspectOperation;

                // Check target variable exists
                if self.symbol_table.lookup(target).is_none() {
                    self.errors.push(format!("Undefined target variable: {}", target));
                }

                // Check operation expressions
                let checker = TypeChecker::new(&self.symbol_table);
                match operation {
                    InspectOperation::Replacing { pattern, replacement } |
                    InspectOperation::ReplacingAll { pattern, replacement } => {
                        if let Err(e) = checker.check_expr(pattern) {
                            self.errors.push(format!("Type error in inspect pattern: {}", e));
                        }
                        if let Err(e) = checker.check_expr(replacement) {
                            self.errors.push(format!("Type error in inspect replacement: {}", e));
                        }
                    }
                    InspectOperation::Tallying { counter, pattern } |
                    InspectOperation::TallyingAll { counter, pattern } => {
                        if self.symbol_table.lookup(counter).is_none() {
                            self.errors.push(format!("Undefined counter variable: {}", counter));
                        }
                        if let Err(e) = checker.check_expr(pattern) {
                            self.errors.push(format!("Type error in inspect pattern: {}", e));
                        }
                    }
                }
            }

            StmtKind::SearchLinear { array, index, at_end, when_clauses } => {
                // Check array variable exists
                if self.symbol_table.lookup(array).is_none() {
                    self.errors.push(format!("Undefined array variable: {}", array));
                }

                // Check index variable if present
                if let Some(idx) = index {
                    if self.symbol_table.lookup(idx).is_none() {
                        self.errors.push(format!("Undefined index variable: {}", idx));
                    }
                }

                // Check at_end clause
                if let Some(stmts) = at_end {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }

                // Check when clauses
                for (condition, body) in when_clauses {
                    {
                        let checker = TypeChecker::new(&self.symbol_table);
                        if let Err(e) = checker.check_expr(condition) {
                            self.errors.push(format!("Type error in search condition: {}", e));
                        }
                    }
                    for stmt in body {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::SearchBinary { array, index, at_end, when_condition, when_body } => {
                // Check array variable exists
                if self.symbol_table.lookup(array).is_none() {
                    self.errors.push(format!("Undefined array variable: {}", array));
                }

                // Check index variable if present
                if let Some(idx) = index {
                    if self.symbol_table.lookup(idx).is_none() {
                        self.errors.push(format!("Undefined index variable: {}", idx));
                    }
                }

                // Check at_end clause
                if let Some(stmts) = at_end {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }

                // Check when condition and body
                {
                    let checker = TypeChecker::new(&self.symbol_table);
                    if let Err(e) = checker.check_expr(when_condition) {
                        self.errors.push(format!("Type error in search all condition: {}", e));
                    }
                }
                for stmt in when_body {
                    self.analyze_statement(stmt);
                }
            }

            StmtKind::SetIndex { index, operation } => {
                use coba_ast::stmt::SetOperation;

                // Check index variable exists
                if self.symbol_table.lookup(index).is_none() {
                    self.errors.push(format!("Undefined index variable: {}", index));
                }

                // Check operation expression
                let checker = TypeChecker::new(&self.symbol_table);
                let expr = match operation {
                    SetOperation::To(e) | SetOperation::UpBy(e) | SetOperation::DownBy(e) => e,
                };
                if let Err(e) = checker.check_expr(expr) {
                    self.errors.push(format!("Type error in set index operation: {}", e));
                }
            }

            StmtKind::OpenFile { file, .. } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }
            }

            StmtKind::CloseFile { file } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }
            }

            StmtKind::ReadFile { file, record, at_end } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }

                // Check record variable exists
                if self.symbol_table.lookup(record).is_none() {
                    self.errors.push(format!("Undefined record variable: {}", record));
                }

                // Check at_end clause
                if let Some(stmts) = at_end {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::WriteFile { file, record } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }

                // Check record variable exists
                if self.symbol_table.lookup(record).is_none() {
                    self.errors.push(format!("Undefined record variable: {}", record));
                }
            }

            StmtKind::RewriteFile { file, record } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }

                // Check record variable exists
                if self.symbol_table.lookup(record).is_none() {
                    self.errors.push(format!("Undefined record variable: {}", record));
                }
            }

            StmtKind::DeleteFile { file } => {
                // Check file exists
                if self.symbol_table.lookup(file).is_none() {
                    self.errors.push(format!("Undefined file: {}", file));
                }
            }

            StmtKind::Add { operands, to, giving, on_size_error } => {
                let checker = TypeChecker::new(&self.symbol_table);
                for operand in operands {
                    if let Err(e) = checker.check_expr(operand) {
                        self.errors.push(format!("Type error in arithmetic: {}", e));
                    }
                }
                if let Some(targets) = to {
                    for target in targets {
                        if self.symbol_table.lookup(target).is_none() {
                            self.errors.push(format!("Undefined variable: {}", target));
                        }
                    }
                }
                if let Some(target) = giving {
                    if self.symbol_table.lookup(target).is_none() {
                        self.errors.push(format!("Undefined variable: {}", target));
                    }
                }
                if let Some(stmts) = on_size_error {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::Subtract { operands, from, giving, on_size_error } => {
                let checker = TypeChecker::new(&self.symbol_table);
                for operand in operands {
                    if let Err(e) = checker.check_expr(operand) {
                        self.errors.push(format!("Type error in arithmetic: {}", e));
                    }
                }
                if let Err(e) = checker.check_expr(from) {
                    self.errors.push(format!("Type error in FROM expression: {}", e));
                }
                if let Some(target) = giving {
                    if self.symbol_table.lookup(target).is_none() {
                        self.errors.push(format!("Undefined variable: {}", target));
                    }
                }
                if let Some(stmts) = on_size_error {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::Multiply { operand1, operand2, giving, on_size_error } |
            StmtKind::Divide { dividend: operand1, divisor: operand2, giving, on_size_error, .. } => {
                let checker = TypeChecker::new(&self.symbol_table);
                if let Err(e) = checker.check_expr(operand1) {
                    self.errors.push(format!("Type error in arithmetic: {}", e));
                }
                if let Err(e) = checker.check_expr(operand2) {
                    self.errors.push(format!("Type error in arithmetic: {}", e));
                }
                if let Some(target) = giving {
                    if self.symbol_table.lookup(target).is_none() {
                        self.errors.push(format!("Undefined variable: {}", target));
                    }
                }
                if let Some(stmts) = on_size_error {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::Compute { target, expression, on_size_error } => {
                if self.symbol_table.lookup(target).is_none() {
                    self.errors.push(format!("Undefined variable: {}", target));
                }
                let checker = TypeChecker::new(&self.symbol_table);
                if let Err(e) = checker.check_expr(expression) {
                    self.errors.push(format!("Type error in compute: {}", e));
                }
                if let Some(stmts) = on_size_error {
                    for stmt in stmts {
                        self.analyze_statement(stmt);
                    }
                }
            }

            StmtKind::PerformTimes { procedure, times } => {
                // Validate procedure exists (will be checked at scope level)
                // For now, just ensure times expression is valid
                let checker = TypeChecker::new(&self.symbol_table);
                if let Err(e) = checker.check_expr(times) {
                    self.errors.push(format!("Type error in perform times: {}", e));
                }
                // Note: procedure name validation happens at higher level
                let _ = procedure;
            }

            StmtKind::PerformThru { start_procedure, end_procedure, times } => {
                // Validate procedures exist (will be checked at scope level)
                if let Some(times_expr) = times {
                    let checker = TypeChecker::new(&self.symbol_table);
                    if let Err(e) = checker.check_expr(times_expr) {
                        self.errors.push(format!("Type error in perform thru: {}", e));
                    }
                }
                // Note: procedure name validation happens at higher level
                let _ = (start_procedure, end_procedure);
            }
        }
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn get_errors(&self) -> &[String] {
        &self.errors
    }
}

fn types_compatible(left: &coba_ast::types::Type, right: &coba_ast::types::Type) -> bool {
    use coba_ast::types::Type;

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
