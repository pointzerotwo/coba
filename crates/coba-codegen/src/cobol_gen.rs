/// COBOL code generator (Coba → COBOL)

use coba_ast::{
    expr::{BinaryOp, Expr, ExprKind, UnaryOp},
    program::{Declaration, Program},
    stmt::{Stmt, StmtKind},
    types::Type,
};
use coba_semantic::SymbolTable;

pub struct CobolGenerator {
    output: String,
    indent_level: usize,
    symbol_table: SymbolTable,
}

impl CobolGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent_level: 0,
            symbol_table: SymbolTable::new(),
        }
    }

    /// Generate COBOL code from a Coba program
    pub fn generate(&mut self, program: &Program) -> String {
        self.output.clear();

        // Generate IDENTIFICATION DIVISION
        self.write_line("       IDENTIFICATION DIVISION.");
        self.write_line("       PROGRAM-ID. COBA-PROGRAM.");
        self.write_line("");

        // Generate ENVIRONMENT DIVISION
        self.write_line("       ENVIRONMENT DIVISION.");

        // Check if there are file declarations
        let has_files = program.declarations.iter().any(|d| matches!(d, Declaration::File { .. }));

        if has_files {
            self.write_line("       INPUT-OUTPUT SECTION.");
            self.write_line("       FILE-CONTROL.");

            for decl in &program.declarations {
                if let Declaration::File { name, status_var, .. } = decl {
                    let file_name = name.to_uppercase().replace('_', "-");
                    self.write_line(&format!("           SELECT {} ASSIGN TO \"{}\"", file_name, name));
                    self.write_line("               ORGANIZATION IS SEQUENTIAL");
                    if let Some(status) = status_var {
                        let status_name = status.to_uppercase().replace('_', "-");
                        self.write_line(&format!("               FILE STATUS IS {}", status_name));
                    }
                    self.write_line("           .");
                }
            }
        }

        self.write_line("");

        // Generate DATA DIVISION
        self.write_line("       DATA DIVISION.");

        // Generate FD section for files
        if has_files {
            self.write_line("       FILE SECTION.");

            for decl in &program.declarations {
                if let Declaration::File { name, record_type, .. } = decl {
                    let file_name = name.to_uppercase().replace('_', "-");
                    self.write_line(&format!("       FD {}.", file_name));

                    let picture = record_type.to_cobol_picture();
                    self.write_line(&format!("       01 {}-RECORD {}.", file_name, picture));
                }
            }

            self.write_line("");
        }

        self.write_line("       WORKING-STORAGE SECTION.");

        // Collect global variables
        for decl in &program.declarations {
            if let Declaration::Variable { name, type_, initializer } = decl {
                let mangled_name = self.symbol_table.add_global(name.clone(), type_.clone())
                    .unwrap_or_else(|_| format!("GLOBAL-{}", name.to_uppercase()));

                self.generate_data_item(&mangled_name, type_, initializer.as_ref());
            }
        }

        self.write_line("");

        // Generate PROCEDURE DIVISION
        self.write_line("       PROCEDURE DIVISION.");
        self.write_line("");

        // Generate main entry point
        self.write_line("       MAIN-PROGRAM.");

        // Call each procedure in order (if not already called)
        for decl in &program.declarations {
            if let Declaration::Procedure { name, .. } = decl {
                let cobol_name = name.to_uppercase().replace('_', "-");
                self.write_line(&format!("           PERFORM {}.", cobol_name));
            }
        }

        self.write_line("           STOP RUN.");
        self.write_line("");

        // Generate procedures
        for decl in &program.declarations {
            if let Declaration::Procedure { name, parameters, body } = decl {
                self.generate_procedure(name, parameters, body);
            }
        }

        self.output.clone()
    }

    fn generate_data_item(&mut self, name: &str, type_: &Type, initializer: Option<&Expr>) {
        let picture = type_.to_cobol_picture();

        // Arrays cannot have VALUE clause in COBOL
        if matches!(type_, Type::Array { .. }) {
            self.write_line(&format!("       01 {} {}.", name, picture));
        } else if let Some(init) = initializer {
            let value = self.expr_to_literal(init);
            self.write_line(&format!("       01 {} {} VALUE {}.", name, picture, value));
        } else {
            // Default values
            let default_value = match type_ {
                Type::Decimal { .. } | Type::Integer => "0",
                Type::Text { .. } => "SPACES",
                Type::Boolean => "0",
                Type::Array { .. } => unreachable!(), // Handled above
            };
            self.write_line(&format!("       01 {} {} VALUE {}.", name, picture, default_value));
        }
    }

    fn generate_procedure(&mut self, name: &str, parameters: &[coba_ast::program::Parameter], body: &[Stmt]) {
        let cobol_name = name.to_uppercase().replace('_', "-");
        self.write_line(&format!("       {}.", cobol_name));

        // Enter procedure scope
        self.symbol_table.enter_scope(name.to_string());

        // Generate local variable declarations for parameters
        for (index, param) in parameters.iter().enumerate() {
            let mangled_name = self.symbol_table.add_parameter(name, index, param.name.clone(), param.type_.clone())
                .unwrap_or_else(|_| format!("PARAM-{}-{}", cobol_name, index));

            let picture = param.type_.to_cobol_picture();
            self.write_line(&format!("           01 {} {}.", mangled_name, picture));
        }

        // Generate statements
        for stmt in body {
            self.generate_statement(stmt);
        }

        self.symbol_table.exit_scope();
        self.write_line("");
    }

    fn generate_statement(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::VarDecl { name, type_, initializer } => {
                let mangled_name = self.symbol_table.add_local(name.clone(), type_.clone())
                    .unwrap_or_else(|_| format!("LOCAL-{}", name.to_uppercase()));

                let picture = type_.to_cobol_picture();

                if let Some(init) = initializer {
                    let value = self.expr_to_literal(init);
                    self.write_line(&format!("           01 {} {} VALUE {}.", mangled_name, picture, value));
                } else {
                    self.write_line(&format!("           01 {} {}.", mangled_name, picture));
                }
            }

            StmtKind::Assign { target, value } => {
                let target_str = self.generate_expr(target);
                let expr_str = self.generate_expr(value);
                self.write_line(&format!("           MOVE {} TO {}.", expr_str, target_str));
            }

            StmtKind::Call { name, arguments: _ } => {
                let cobol_name = name.to_uppercase().replace('_', "-");
                self.write_line(&format!("           PERFORM {}.", cobol_name));
            }

            StmtKind::If { condition, then_branch, elif_branches, else_branch } => {
                let cond_str = self.generate_condition(condition);
                self.write_line(&format!("           IF {}", cond_str));

                for stmt in then_branch {
                    self.generate_statement(stmt);
                }

                for (elif_cond, elif_body) in elif_branches {
                    let elif_cond_str = self.generate_condition(elif_cond);
                    self.write_line(&format!("           ELSE IF {}", elif_cond_str));

                    for stmt in elif_body {
                        self.generate_statement(stmt);
                    }
                }

                if let Some(else_body) = else_branch {
                    self.write_line("           ELSE");
                    for stmt in else_body {
                        self.generate_statement(stmt);
                    }
                }

                self.write_line("           END-IF.");
            }

            StmtKind::While { condition, body } => {
                let cond_str = self.generate_condition(condition);
                self.write_line(&format!("           PERFORM UNTIL NOT ({})", cond_str));

                for stmt in body {
                    self.generate_statement(stmt);
                }

                self.write_line("           END-PERFORM.");
            }

            StmtKind::For { variable, start, end, step, body } => {
                let var_mangled = self.symbol_table.get_mangled_name(variable)
                    .unwrap_or_else(|| variable.to_uppercase().replace('_', "-"));

                let start_str = self.generate_expr(start);
                let end_str = self.generate_expr(end);
                let step_str = self.generate_expr(step);

                self.write_line(&format!(
                    "           PERFORM VARYING {} FROM {} BY {} UNTIL {} > {}",
                    var_mangled, start_str, step_str, var_mangled, end_str
                ));

                for stmt in body {
                    self.generate_statement(stmt);
                }

                self.write_line("           END-PERFORM.");
            }

            StmtKind::Print { values } => {
                let mut display_items = Vec::new();

                for value in values {
                    let expr_str = self.generate_expr(value);
                    display_items.push(expr_str);
                }

                // Generate DISPLAY statement with all values
                let items = display_items.join(" ");
                self.write_line(&format!("           DISPLAY {}.", items));
            }

            StmtKind::Return { .. } => {
                // Return is not supported in current COBOL generation
                self.write_line("           EXIT.");
            }

            StmtKind::Evaluate { subject, when_branches, other_branch } => {
                use coba_ast::stmt::WhenCondition;

                let subject_str = self.generate_expr(subject);
                self.write_line(&format!("           EVALUATE {}", subject_str));

                for branch in when_branches {
                    let when_str = match &branch.condition {
                        WhenCondition::Value(expr) => {
                            self.generate_expr(expr)
                        }
                        WhenCondition::Range(start, end) => {
                            let start_str = self.generate_expr(start);
                            let end_str = self.generate_expr(end);
                            format!("{} THRU {}", start_str, end_str)
                        }
                    };

                    self.write_line(&format!("           WHEN {}", when_str));

                    for stmt in &branch.body {
                        self.generate_statement(stmt);
                    }
                }

                if let Some(other_body) = other_branch {
                    self.write_line("           WHEN OTHER");
                    for stmt in other_body {
                        self.generate_statement(stmt);
                    }
                }

                self.write_line("           END-EVALUATE.");
            }

            StmtKind::Accept { variable, source } => {
                use coba_ast::stmt::AcceptSource;

                let mangled_name = self.symbol_table.get_mangled_name(variable)
                    .unwrap_or_else(|| variable.to_uppercase().replace('_', "-"));

                let accept_str = match source {
                    AcceptSource::User => format!("           ACCEPT {}.", mangled_name),
                    AcceptSource::Date => format!("           ACCEPT {} FROM DATE YYYYMMDD.", mangled_name),
                    AcceptSource::Time => format!("           ACCEPT {} FROM TIME.", mangled_name),
                    AcceptSource::Day => format!("           ACCEPT {} FROM DAY YYYYDDD.", mangled_name),
                    AcceptSource::DayOfWeek => format!("           ACCEPT {} FROM DAY-OF-WEEK.", mangled_name),
                };

                self.write_line(&accept_str);
            }

            StmtKind::Initialize { variables } => {
                for var in variables {
                    let mangled = self.symbol_table.get_mangled_name(var)
                        .unwrap_or_else(|| var.to_uppercase().replace('_', "-"));
                    self.write_line(&format!("           INITIALIZE {}.", mangled));
                }
            }

            StmtKind::Continue => {
                self.write_line("           CONTINUE.");
            }

            StmtKind::Exit => {
                self.write_line("           EXIT.");
            }

            StmtKind::SetIndex { index, operation } => {
                use coba_ast::stmt::SetOperation;
                let idx_name = index.to_uppercase().replace('_', "-");

                match operation {
                    SetOperation::To(expr) => {
                        let value = self.generate_expr(expr);
                        self.write_line(&format!("           SET {} TO {}.", idx_name, value));
                    }
                    SetOperation::UpBy(expr) => {
                        let value = self.generate_expr(expr);
                        self.write_line(&format!("           SET {} UP BY {}.", idx_name, value));
                    }
                    SetOperation::DownBy(expr) => {
                        let value = self.generate_expr(expr);
                        self.write_line(&format!("           SET {} DOWN BY {}.", idx_name, value));
                    }
                }
            }

            StmtKind::StringConcat { sources, destination, pointer } => {
                let dest = self.symbol_table.get_mangled_name(destination)
                    .unwrap_or_else(|| destination.to_uppercase().replace('_', "-"));

                let mut string_line = String::from("           STRING ");

                for (i, src) in sources.iter().enumerate() {
                    if i > 0 {
                        string_line.push_str(" ");
                    }
                    let src_str = self.generate_expr(&src.value);
                    string_line.push_str(&src_str);

                    if let Some(delim) = &src.delimiter {
                        let delim_str = self.generate_expr(delim);
                        string_line.push_str(&format!(" DELIMITED BY {}", delim_str));
                    } else {
                        string_line.push_str(" DELIMITED BY SIZE");
                    }
                }

                string_line.push_str(&format!(" INTO {}", dest));

                if let Some(ptr) = pointer {
                    let ptr_name = ptr.to_uppercase().replace('_', "-");
                    string_line.push_str(&format!(" WITH POINTER {}", ptr_name));
                }

                string_line.push('.');
                self.write_line(&string_line);
            }

            StmtKind::StringSplit { source, delimiter, destinations, pointer, tallying } => {
                let src = self.symbol_table.get_mangled_name(source)
                    .unwrap_or_else(|| source.to_uppercase().replace('_', "-"));

                let mut unstring_line = format!("           UNSTRING {}", src);

                if let Some(delim) = delimiter {
                    let delim_str = self.generate_expr(delim);
                    unstring_line.push_str(&format!(" DELIMITED BY {}", delim_str));
                }

                unstring_line.push_str(" INTO");

                for (i, dest) in destinations.iter().enumerate() {
                    if i > 0 {
                        unstring_line.push(',');
                    }
                    let dest_name = dest.to_uppercase().replace('_', "-");
                    unstring_line.push_str(&format!(" {}", dest_name));
                }

                if let Some(ptr) = pointer {
                    let ptr_name = ptr.to_uppercase().replace('_', "-");
                    unstring_line.push_str(&format!(" WITH POINTER {}", ptr_name));
                }

                if let Some(counter) = tallying {
                    let counter_name = counter.to_uppercase().replace('_', "-");
                    unstring_line.push_str(&format!(" TALLYING IN {}", counter_name));
                }

                unstring_line.push('.');
                self.write_line(&unstring_line);
            }

            StmtKind::StringInspect { target, operation } => {
                use coba_ast::stmt::InspectOperation;

                let tgt = self.symbol_table.get_mangled_name(target)
                    .unwrap_or_else(|| target.to_uppercase().replace('_', "-"));

                match operation {
                    InspectOperation::Replacing { pattern, replacement } => {
                        let pat = self.generate_expr(pattern);
                        let rep = self.generate_expr(replacement);
                        self.write_line(&format!("           INSPECT {} REPLACING {} BY {}.", tgt, pat, rep));
                    }
                    InspectOperation::ReplacingAll { pattern, replacement } => {
                        let pat = self.generate_expr(pattern);
                        let rep = self.generate_expr(replacement);
                        self.write_line(&format!("           INSPECT {} REPLACING ALL {} BY {}.", tgt, pat, rep));
                    }
                    InspectOperation::Tallying { counter, pattern } => {
                        let cnt = counter.to_uppercase().replace('_', "-");
                        let pat = self.generate_expr(pattern);
                        self.write_line(&format!("           INSPECT {} TALLYING {} FOR {}.", tgt, cnt, pat));
                    }
                    InspectOperation::TallyingAll { counter, pattern } => {
                        let cnt = counter.to_uppercase().replace('_', "-");
                        let pat = self.generate_expr(pattern);
                        self.write_line(&format!("           INSPECT {} TALLYING {} FOR ALL {}.", tgt, cnt, pat));
                    }
                }
            }

            StmtKind::SearchLinear { array, index, at_end, when_clauses } => {
                let arr = array.to_uppercase().replace('_', "-");

                if let Some(idx) = index {
                    let idx_name = idx.to_uppercase().replace('_', "-");
                    self.write_line(&format!("           SEARCH {} VARYING {}", arr, idx_name));
                } else {
                    self.write_line(&format!("           SEARCH {}", arr));
                }

                if let Some(end_stmts) = at_end {
                    self.write_line("               AT END");
                    for stmt in end_stmts {
                        self.generate_statement(stmt);
                    }
                }

                for (cond, body) in when_clauses {
                    let cond_str = self.generate_condition(cond);
                    self.write_line(&format!("               WHEN {}", cond_str));
                    for stmt in body {
                        self.generate_statement(stmt);
                    }
                }

                self.write_line("           END-SEARCH.");
            }

            StmtKind::SearchBinary { array, index, at_end, when_condition, when_body } => {
                let arr = array.to_uppercase().replace('_', "-");

                if let Some(idx) = index {
                    let idx_name = idx.to_uppercase().replace('_', "-");
                    self.write_line(&format!("           SEARCH ALL {} VARYING {}", arr, idx_name));
                } else {
                    self.write_line(&format!("           SEARCH ALL {}", arr));
                }

                if let Some(end_stmts) = at_end {
                    self.write_line("               AT END");
                    for stmt in end_stmts {
                        self.generate_statement(stmt);
                    }
                }

                let cond_str = self.generate_condition(when_condition);
                self.write_line(&format!("               WHEN {}", cond_str));
                for stmt in when_body {
                    self.generate_statement(stmt);
                }

                self.write_line("           END-SEARCH.");
            }

            StmtKind::OpenFile { file, mode } => {
                use coba_ast::stmt::FileMode;
                let file_name = file.to_uppercase().replace('_', "-");

                let mode_str = match mode {
                    FileMode::Input => "INPUT",
                    FileMode::Output => "OUTPUT",
                    FileMode::Extend => "EXTEND",
                };

                self.write_line(&format!("           OPEN {} {}.", mode_str, file_name));
            }

            StmtKind::CloseFile { file } => {
                let file_name = file.to_uppercase().replace('_', "-");
                self.write_line(&format!("           CLOSE {}.", file_name));
            }

            StmtKind::ReadFile { file, record, at_end } => {
                let file_name = file.to_uppercase().replace('_', "-");
                let record_name = self.symbol_table.get_mangled_name(record)
                    .unwrap_or_else(|| record.to_uppercase().replace('_', "-"));

                self.write_line(&format!("           READ {} INTO {}", file_name, record_name));

                if let Some(stmts) = at_end {
                    self.write_line("               AT END");
                    for stmt in stmts {
                        self.generate_statement(stmt);
                    }
                    self.write_line("           END-READ.");
                } else {
                    self.write_line("           .");
                }
            }

            StmtKind::WriteFile { file, record } => {
                let file_name = file.to_uppercase().replace('_', "-");
                let record_name = self.symbol_table.get_mangled_name(record)
                    .unwrap_or_else(|| record.to_uppercase().replace('_', "-"));

                self.write_line(&format!("           WRITE {}-RECORD FROM {}.", file_name, record_name));
            }
        }
    }

    fn generate_expr(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Number(n) => {
                if n.fract() == 0.0 {
                    format!("{}", *n as i64)
                } else {
                    format!("{}", n)
                }
            }

            ExprKind::String(s) => format!("\"{}\"", s),

            ExprKind::Boolean(b) => if *b { "1" } else { "0" }.to_string(),

            ExprKind::Variable(name) => {
                self.symbol_table.get_mangled_name(name)
                    .unwrap_or_else(|| name.to_uppercase().replace('_', "-"))
            }

            ExprKind::Binary { left, operator, right } => {
                let left_str = self.generate_expr(left);
                let right_str = self.generate_expr(right);

                match operator {
                    BinaryOp::Add => format!("{} + {}", left_str, right_str),
                    BinaryOp::Subtract => format!("{} - {}", left_str, right_str),
                    BinaryOp::Multiply => format!("{} * {}", left_str, right_str),
                    BinaryOp::Divide => format!("{} / {}", left_str, right_str),
                    BinaryOp::Power => format!("{} ** {}", left_str, right_str),
                    BinaryOp::Modulo => format!("FUNCTION MOD({}, {})", left_str, right_str),
                    _ => format!("({} {} {})", left_str, operator.to_str(), right_str),
                }
            }

            ExprKind::Unary { operator, operand } => {
                let operand_str = self.generate_expr(operand);
                match operator {
                    UnaryOp::Negate => format!("-({})", operand_str),
                    UnaryOp::Not => format!("NOT ({})", operand_str),
                }
            }

            ExprKind::Call { .. } => {
                // Not supported in current implementation
                "0".to_string()
            }

            ExprKind::FunctionCall { name, arguments } => {
                use coba_ast::intrinsics;

                // Look up the intrinsic function to get the COBOL name
                let cobol_name = intrinsics::lookup_intrinsic(name)
                    .map(|f| f.cobol_name.to_string())
                    .unwrap_or_else(|| name.to_uppercase().replace('_', "-"));

                // Generate arguments
                if arguments.is_empty() {
                    format!("FUNCTION {}", cobol_name)
                } else {
                    let args: Vec<String> = arguments
                        .iter()
                        .map(|arg| self.generate_expr(arg))
                        .collect();
                    format!("FUNCTION {}({})", cobol_name, args.join(", "))
                }
            }

            ExprKind::Index { array, index } => {
                let array_str = self.generate_expr(array);
                let index_str = self.generate_expr(index);
                // COBOL arrays are 1-indexed, Coba arrays are 0-indexed
                // So we need to add 1 to the index
                format!("{}({} + 1)", array_str, index_str)
            }
        }
    }

    fn generate_condition(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Binary { left, operator, right } => {
                let left_str = self.generate_expr(left);
                let right_str = self.generate_expr(right);

                match operator {
                    BinaryOp::Equal => format!("{} = {}", left_str, right_str),
                    BinaryOp::NotEqual => format!("{} NOT = {}", left_str, right_str),
                    BinaryOp::Less => format!("{} < {}", left_str, right_str),
                    BinaryOp::LessEqual => format!("{} <= {}", left_str, right_str),
                    BinaryOp::Greater => format!("{} > {}", left_str, right_str),
                    BinaryOp::GreaterEqual => format!("{} >= {}", left_str, right_str),
                    BinaryOp::And => format!("({}) AND ({})",
                        self.generate_condition(left),
                        self.generate_condition(right)
                    ),
                    BinaryOp::Or => format!("({}) OR ({})",
                        self.generate_condition(left),
                        self.generate_condition(right)
                    ),
                    _ => self.generate_expr(expr),
                }
            }

            ExprKind::Unary { operator: UnaryOp::Not, operand } => {
                format!("NOT ({})", self.generate_condition(operand))
            }

            _ => self.generate_expr(expr),
        }
    }

    fn expr_to_literal(&self, expr: &Expr) -> String {
        match &expr.kind {
            ExprKind::Number(n) => format!("{}", n),
            ExprKind::String(s) => format!("\"{}\"", s),
            ExprKind::Boolean(b) => if *b { "1" } else { "0" }.to_string(),
            _ => "0".to_string(),
        }
    }

    fn write_line(&mut self, line: &str) {
        self.output.push_str(line);
        self.output.push('\n');
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_program() {
        let program = Program::new(vec![
            Declaration::Variable {
                name: "total".to_string(),
                type_: Type::decimal(9, 2),
                initializer: Some(Expr::new(ExprKind::Number(0.0), 1, 1)),
            },
        ]);

        let mut generator = CobolGenerator::new();
        let cobol = generator.generate(&program);

        assert!(cobol.contains("IDENTIFICATION DIVISION"));
        assert!(cobol.contains("DATA DIVISION"));
        assert!(cobol.contains("PROCEDURE DIVISION"));
        assert!(cobol.contains("GLOBAL-TOTAL"));
    }
}
