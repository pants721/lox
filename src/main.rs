use std::env;
use std::fs;
use std::fmt;
use std::io::{self, Write};
use std::process;

use itertools::Itertools;

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

struct CamelCaseSplit<'a> {
    char_indices: std::str::CharIndices<'a>,
    chunk_start: usize,
    s: &'a str
}

impl<'a> CamelCaseSplit<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut char_indices = s.char_indices();
        // We'll never want to split before the first char, so skip it.
        char_indices.next();
        Self {
            char_indices,
            chunk_start: 0,
            s,
        }
    }
}

impl<'a> Iterator for CamelCaseSplit<'a> {
    type Item = &'a str;
    
    fn next(&mut self) -> Option<Self::Item> {
        // The input is exhausted
        if self.chunk_start == self.s.len() {
            return None;
        }
        // Find the next uppercase letter position OR the end of the string
        let chunk_end = if let Some((chunk_end, _)) = self.char_indices.by_ref().skip_while(|(_, c)| !c.is_uppercase()).next() {
            chunk_end
        } else {
            self.s.len()
        };
        let chunk = &self.s[self.chunk_start..chunk_end];
        self.chunk_start = chunk_end;
        return Some(chunk);
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
    Semicolon,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,

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
            ";" => Some(Semicolon),
            "!" => Some(Bang),
            "!=" => Some(BangEqual),
            "=" => Some(Equal),
            "==" => Some(EqualEqual),
            "<" => Some(Less),
            "<=" => Some(LessEqual),
            ">" => Some(Greater),
            ">=" => Some(GreaterEqual),
            _ => None
        }
    }

    pub fn screaming_snake_case(&self) -> String {
        CamelCaseSplit::new(&self.to_string()).map(|s| s.to_uppercase()).collect_vec().join("_")
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(self, f)
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
        f.write_str(&format!("{} {} null", self.t_type.screaming_snake_case(), self.lexeme))
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
        use TokenType::*;
        if let Some(c) = self.advance() {
            let mut lexeme = c.to_string();
            let t_type = match c {
                '!' => if self.match_char('=') { lexeme = "!=".to_string(); Some(BangEqual) } else { Some(Bang) },
                '=' => if self.match_char('=') { lexeme = "==".to_string(); Some(EqualEqual) } else { Some(Equal) },
                '<' => if self.match_char('=') { lexeme = "<=".to_string(); Some(LessEqual) } else { Some(Less) },
                '>' => if self.match_char('=') { lexeme = ">=".to_string(); Some(GreaterEqual) } else { Some(Greater) },
                '/' => if self.match_char('/') {
                    while self.peek() != Some('\n') && !self.is_at_end() { 
                        self.advance();
                    }
                    None
                } else {
                    Some(Slash)
                }
                ' ' => return,
                '\r' => return,
                '\t' => return,
                '\n' => { self.line += 1; return; }
                _ => {
                    match TokenType::parse(&c.to_string()) {
                        Some(t) => Some(t),
                        None => {
                            let e = LexerError::UnexpectedCharacter { line: self.line, c };
                            eprintln!("{}", e);
                            self.errors.push(e);
                            None
                        }
                    }
                }
            };

            if let Some(t) = t_type {
                self.tokens.push(Token::new(lexeme, t, self.line));
            }
        }
    }

    fn char_at(&self, n: usize) -> Option<char> {
        self.source.chars().nth(n)
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() { return None }
        self.char_at(self.current)
    }

    fn advance(&mut self) -> Option<char>{
        let c = self.source.chars().nth(self.current);
        self.current += 1;
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false };
        if let Some(c) = self.char_at(self.current) {
            if c != expected { return false };
        } else {
            return false;
        }

        self.current += 1;
        true
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
