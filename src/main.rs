use std::env;
use std::fs;
use std::process;

use crate::scanner::Scanner;

mod scanner;
mod util;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
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

                if !scanner.errors.is_empty() {
                    process::exit(65);
                }
            } else {
                println!("EOF  null");
            }

        }
        _ => {
            eprintln!("Unknown command: {}", command);
        }
    }
}
