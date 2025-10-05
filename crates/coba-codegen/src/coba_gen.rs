/// Coba code generator (COBOL → Coba decompilation)

use coba_ast::{
    cobol_ast::{CobolProgram, CobolStatement, Paragraph},
    program::Program,
};

pub struct CobaGenerator {
    output: String,
}

impl CobaGenerator {
    pub fn new() -> Self {
        Self {
            output: String::new(),
        }
    }

    /// Generate Coba code from a COBOL program
    pub fn generate(&mut self, cobol_program: &CobolProgram) -> String {
        self.output.clear();

        // Generate global variables from DATA DIVISION
        if let Some(data_div) = &cobol_program.data_division {
            for item in &data_div.working_storage {
                if let Some(type_) = item.infer_type() {
                    let initializer = if let Some(val) = &item.value {
                        format!(" = {}", val)
                    } else {
                        String::new()
                    };

                    self.output.push_str(&format!(
                        "{:?} {}{}\n",
                        type_,
                        self.demangle_name(&item.name),
                        initializer
                    ));
                }
            }
        }

        self.output.push('\n');

        // Generate procedures from PROCEDURE DIVISION
        if let Some(proc_div) = &cobol_program.procedure_division {
            for paragraph in &proc_div.paragraphs {
                self.generate_procedure(paragraph);
            }
        }

        self.output.clone()
    }

    fn generate_procedure(&mut self, paragraph: &Paragraph) {
        let proc_name = self.demangle_name(&paragraph.name);

        self.output.push_str(&format!("procedure {}()\n", proc_name));

        for stmt in &paragraph.statements {
            self.generate_statement(stmt, 1);
        }

        self.output.push_str("end\n\n");
    }

    fn generate_statement(&mut self, stmt: &CobolStatement, indent: usize) {
        let indent_str = "    ".repeat(indent);

        match stmt {
            CobolStatement::Move { source, dest } => {
                self.output.push_str(&format!(
                    "{}set {} = {:?}\n",
                    indent_str,
                    self.demangle_name(dest),
                    source
                ));
            }

            CobolStatement::Display { items } => {
                self.output.push_str(&format!(
                    "{}// DISPLAY {:?}\n",
                    indent_str,
                    items
                ));
            }

            CobolStatement::If { condition, then_stmts, else_stmts } => {
                self.output.push_str(&format!("{}if {:?} then\n", indent_str, condition));

                for then_stmt in then_stmts {
                    self.generate_statement(then_stmt, indent + 1);
                }

                if let Some(else_body) = else_stmts {
                    self.output.push_str(&format!("{}else\n", indent_str));
                    for else_stmt in else_body {
                        self.generate_statement(else_stmt, indent + 1);
                    }
                }

                self.output.push_str(&format!("{}end\n", indent_str));
            }

            CobolStatement::Perform { target } => {
                self.output.push_str(&format!(
                    "{}// PERFORM {:?}\n",
                    indent_str,
                    target
                ));
            }

            CobolStatement::StopRun => {
                self.output.push_str(&format!("{}// STOP RUN\n", indent_str));
            }

            _ => {
                self.output.push_str(&format!("{}// {:?}\n", indent_str, stmt));
            }
        }
    }

    /// Reverse name mangling
    fn demangle_name(&self, name: &str) -> String {
        if let Some(rest) = name.strip_prefix("GLOBAL-") {
            rest.to_lowercase().replace('-', "_")
        } else if let Some(rest) = name.strip_prefix("LOCAL-") {
            let parts: Vec<&str> = rest.splitn(2, '-').collect();
            if parts.len() == 2 {
                parts[1].to_lowercase().replace('-', "_")
            } else {
                name.to_lowercase().replace('-', "_")
            }
        } else if let Some(rest) = name.strip_prefix("PARAM-") {
            let parts: Vec<&str> = rest.splitn(2, '-').collect();
            if parts.len() == 2 {
                format!("param_{}", parts[1])
            } else {
                name.to_lowercase().replace('-', "_")
            }
        } else if let Some(rest) = name.strip_prefix("COBA-") {
            // Reverse reserved word escaping
            rest.to_lowercase().replace('-', "_")
        } else {
            name.to_lowercase().replace('-', "_")
        }
    }
}
