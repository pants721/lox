use std::env;
use std::fs;
use std::fmt;
use std::io::{self, Write};
use std::process;

pub enum LexerError {
    UnexpectedCharacter {
        line: usize,
        c: char,
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexerError::*;
        match *self {
            UnexpectedCharacter { line, c } => {
                f.write_str(&format!("[line {}] Error: Unexpected character: {}", line, c))
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    SemiColon,
    Eof,
}

impl TokenType {
    pub fn parse(s: &str) -> Option<TokenType> {
        use self::TokenType::*;
        match s {
            "(" => Some(LeftParen),
            ")" => Some(RightParen),
            "{" => Some(LeftBrace),
            "}" => Some(RightBrace),
            "," => Some(Comma),
            "." => Some(Dot),
            "-" => Some(Minus),
            "+" => Some(Plus),
            "*" => Some(Star),
            ";" => Some(SemiColon),
            _ => None
        }
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        match self {
            LeftParen => f.write_str("LEFT_PAREN"),
            RightParen => f.write_str("RIGHT_PAREN"),
            LeftBrace => f.write_str("LEFT_BRACE"),
            RightBrace => f.write_str("RIGHT_BRACE"),
            Comma => f.write_str("COMMA"),
            Dot => f.write_str("DOT"),
            Minus => f.write_str("MINUS"),
            Plus => f.write_str("PLUS"),
            Star => f.write_str("STAR"),
            SemiColon => f.write_str("SEMICOLON"),
            Eof => f.write_str("EOF"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    lexeme: String,
    t_type: TokenType,
    line: usize,
}

impl Token {
    pub fn new(lexeme: String, t_type: TokenType, line: usize) -> Self {
        Self {
            lexeme,
            t_type,
            line,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} {} null", self.t_type, self.lexeme))
    }
}

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    errors: Vec<LexerError>,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Self {
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            errors: vec![],
        }
    }

    pub fn scan_tokens(&mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens.push(Token::new("".to_string(), TokenType::Eof, self.line));
        self.tokens.clone()
    }

    pub fn scan_token(&mut self) {
        if let Some(c) = self.advance() {
            if let Some(t_type) = TokenType::parse(&c.to_string()) {
                self.tokens.push(Token {
                    lexeme: c.to_string(),
                    line: self.line,
                    t_type 
                });
            } else {
                let e = LexerError::UnexpectedCharacter { line: self.line, c };
                eprintln!("{}", e);
                self.errors.push(e);
            }
        }
    }

    fn advance(&mut self) -> Option<char>{
        let c = self.source.chars().nth(self.current);
        self.current += 1;
        c
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
}

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
            return;
        }
    }
}
