use std::env;
use std::fs;
use std::process;

use anyhow::Result;
use parse::AstPrinter;
use parse::Expr;
use parse::Parser;
use parse::Visitor;
use scanner::Literal;
use scanner::Token;
use scanner::TokenType;

use crate::scanner::Scanner;

mod scanner;
mod util;
mod parse;

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
                let expr = parser.parse()?;

                if parser.has_errored {
                    process::exit(65);
                }

                let mut p = AstPrinter;

                println!("{}", p.visit(&expr));
            } else {
                println!("EOF  null");
            }

            // let expr = 
            // binary_expr!(
            //     unary_expr!("-", literal_expr!(123.0, Literal::Number)),
            //     "*",
            //     grouping_expr!("(", literal_expr!(45.67, Literal::Number), ")")
            // );
        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }

    Ok(())
}
