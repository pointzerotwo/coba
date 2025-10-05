/// CLI tool for the coba compiler/decompiler
///
/// Provides command-line interface for:
/// - Compiling coba to COBOL
/// - Decompiling COBOL to coba
/// - Various diagnostic and utility commands

use std::env;
use std::fs;
use std::process;

use coba_lexer::Lexer;
use coba_parser::Parser;
use coba_semantic::SemanticAnalyzer;
use coba_codegen::CobolGenerator;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage();
        process::exit(1);
    }

    let command = &args[1];

    match command.as_str() {
        "compile" => {
            if args.len() != 4 {
                eprintln!("Usage: coba compile <input.coba> <output.cob>");
                process::exit(1);
            }
            compile(&args[2], &args[3]);
        }
        "decompile" => {
            if args.len() != 4 {
                eprintln!("Usage: coba decompile <input.cob> <output.coba>");
                process::exit(1);
            }
            decompile(&args[2], &args[3]);
        }
        "check" => {
            if args.len() != 3 {
                eprintln!("Usage: coba check <input.coba>");
                process::exit(1);
            }
            check(&args[2]);
        }
        "--help" | "-h" | "help" => {
            print_usage();
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            print_usage();
            process::exit(1);
        }
    }
}

fn print_usage() {
    println!("Coba Compiler/Decompiler v0.1.0");
    println!();
    println!("Usage:");
    println!("  coba compile <input.coba> <output.cob>    Compile Coba to COBOL");
    println!("  coba decompile <input.cob> <output.coba>  Decompile COBOL to Coba");
    println!("  coba check <input.coba>                   Check Coba program for errors");
    println!("  coba --help                               Show this help message");
}

fn compile(input_path: &str, output_path: &str) {
    println!("Compiling {} to {}...", input_path, output_path);

    // Read input file
    let source = match fs::read_to_string(input_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading input file: {}", e);
            process::exit(1);
        }
    };

    // Lex
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();

    // Parse
    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(prog) => prog,
        Err(errors) => {
            eprintln!("Parse errors:");
            for error in errors {
                eprintln!("  {}", error);
            }
            process::exit(1);
        }
    };

    // Semantic analysis
    let mut analyzer = SemanticAnalyzer::new();
    if let Err(errors) = analyzer.analyze(&program) {
        eprintln!("Semantic errors:");
        for error in errors {
            eprintln!("  {}", error);
        }
        process::exit(1);
    }

    // Code generation
    let mut generator = CobolGenerator::new();
    let cobol_code = generator.generate(&program);

    // Write output
    if let Err(e) = fs::write(output_path, cobol_code) {
        eprintln!("Error writing output file: {}", e);
        process::exit(1);
    }

    println!("✓ Compilation successful!");
}

fn decompile(input_path: &str, output_path: &str) {
    println!("Decompiling {} to {}...", input_path, output_path);

    eprintln!("Decompilation not yet fully implemented");
    process::exit(1);

    // TODO: Implement COBOL decompilation
    // - Lex COBOL source
    // - Parse COBOL AST
    // - Transform to Coba AST
    // - Generate Coba code
}

fn check(input_path: &str) {
    println!("Checking {}...", input_path);

    // Read input file
    let source = match fs::read_to_string(input_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading input file: {}", e);
            process::exit(1);
        }
    };

    // Lex
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();

    // Parse
    let mut parser = Parser::new(tokens);
    let program = match parser.parse() {
        Ok(prog) => prog,
        Err(errors) => {
            eprintln!("Parse errors:");
            for error in errors {
                eprintln!("  {}", error);
            }
            process::exit(1);
        }
    };

    // Semantic analysis
    let mut analyzer = SemanticAnalyzer::new();
    if let Err(errors) = analyzer.analyze(&program) {
        eprintln!("Semantic errors:");
        for error in errors {
            eprintln!("  {}", error);
        }
        process::exit(1);
    }

    println!("✓ No errors found!");
}
