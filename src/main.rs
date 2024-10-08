use std::env;
use std::fs;
use std::process;

use anyhow::Result;
use interpreter::Interpreter;
use parser::AstPrinter;
use parser::Parser;
use parser::Visitor;

use crate::scanner::Scanner;

mod scanner;
mod util;
mod parser;
mod interpreter;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return Ok(());
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                let mut scanner = Scanner::new(file_contents);
                let tokens = scanner.scan_tokens();
                for token in tokens {
                    println!("{}", token);
                }

                if scanner.has_errored {
                    process::exit(65);
                }

                return Ok(());
            } else {
                println!("EOF  null");
            }

        },
        "parse" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                let mut scanner = Scanner::new(file_contents);
                let tokens = scanner.scan_tokens();

                if scanner.has_errored {
                    process::exit(65);
                }

                let mut parser = Parser::new(tokens);
                let expr = match parser.parse() {
                    Ok(ex) => ex,
                    Err(e) => {
                        eprintln!("{}", e);
                        process::exit(65);
                    }
                };

                if parser.has_errored {
                    process::exit(65);
                }

                let mut p = AstPrinter;

                println!("{}", p.print(&expr));
            } else {
                println!("EOF  null");
            }
        },
        "evaluate" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });

            if !file_contents.is_empty() {
                let mut scanner = Scanner::new(file_contents);
                let tokens = scanner.scan_tokens();

                if scanner.has_errored {
                    process::exit(65);
                }

                let mut parser = Parser::new(tokens);
                let expr = match parser.parse() {
                    Ok(ex) => ex,
                    Err(e) => {
                        eprintln!("{}", e);
                        process::exit(65);
                    }
                };

                if parser.has_errored {
                    process::exit(65);
                }

                let mut i = Interpreter::default();

                let s = match i.expr_as_string(&expr) {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("{}", e);
                        process::exit(70);
                    }
                };

                println!("{}", s);
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }

    Ok(())
}
