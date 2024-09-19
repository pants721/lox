use std::env;
use std::fs;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
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
                for c in file_contents.chars() {
                    match c {
                        '(' => println!("LEFT_PAREN ( null"),
                        ')' => println!("RIGHT_PAREN ) null"),
                        _ => (),
                    };
                }
            } else {
                println!("EOF  null");
            }

        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
}
