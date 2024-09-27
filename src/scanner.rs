use std::collections::HashMap;
use std::fmt::Display;
use std::fmt;

use itertools::Itertools;
use lazy_static::lazy_static;

use crate::util::CamelCaseSplit;

lazy_static! {
    pub static ref RESERVED_KEYWORDS: HashMap<&'static str, TokenType> = {
        use TokenType::*;
        let mut m = HashMap::new();

        m.insert("and",    And);
        m.insert("class",  Class);
        m.insert("else",   Else);
        m.insert("false",  False);
        m.insert("for",    For);
        m.insert("fun",    Fun);
        m.insert("if",     If);
        m.insert("nil",    Nil);
        m.insert("or",     Or);
        m.insert("print",  Print);
        m.insert("return", Return);
        m.insert("super",  Super);
        m.insert("this",   This);
        m.insert("true",   True);
        m.insert("var",    Var);
        m.insert("while",  While);

        m
    };
}

pub enum LexerError {
    UnexpectedCharacter {
        line: usize,
        c: char,
    },
    UnterminatedString {
        line: usize,
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LexerError::*;
        match *self {
            UnexpectedCharacter { line, c } => {
                f.write_str(&format!("[line {}] Error: Unexpected character: {}", line, c))
            },
            UnterminatedString { line } => {
                f.write_str(&format!("[line {}] Error: Unterminated string.", line))
            }
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
    String,
    Number,

    // Reserved Keywords
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Identifier,

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
pub enum Literal {
    String(String),
    Number(f64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(s) => f.write_str(s),
            Literal::Number(n) => {
                let s = n.to_string() + {
                    if n % 1.0 == 0.0 {
                        ".0"
                    } else {
                        ""
                    }
                    
                };
                f.write_str(&s)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    lexeme: String,
    t_type: TokenType,
    line: usize,
    literal: Option<Literal>,
}

impl Token {
    pub fn new(lexeme: String, t_type: TokenType, line: usize, literal: Option<Literal>) -> Self {
        Self {
            lexeme,
            t_type,
            line,
            literal,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{} {} {}", self.t_type.screaming_snake_case(), self.lexeme, 
            match &self.literal {
                Some(l) => l.to_string(),
                None => "null".to_string(),
            }))
    }
}

pub struct Scanner {
    source: String,
    pub tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    pub errors: Vec<LexerError>,
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

        self.tokens.push(Token::new("".to_string(), TokenType::Eof, self.line, None));
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
                },
                '"' => { self.string(); return; },
                ' ' => return,
                '\r' => return,
                '\t' => return,
                '\n' => { self.line += 1; return; }
                _ => {
                    match TokenType::parse(&c.to_string()) {
                        Some(t) => Some(t),
                        None => {
                            if c.is_ascii_digit() {
                                self.number();
                                return;
                            } else if c.is_alphanumeric() || c == '_' {
                                self.identifier();
                                return;
                            }
                            let e = LexerError::UnexpectedCharacter { line: self.line, c };
                            eprintln!("{}", e);
                            self.errors.push(e);
                            None
                        }
                    }
                }
            };

            if let Some(t) = t_type {
                self.tokens.push(Token::new(lexeme, t, self.line, None));
            }
        }
    }

    fn identifier(&mut self) {
        while self.peek().is_some_and(|c| c.is_alphanumeric() || c == '_') { self.advance(); }
        
        let text = &self.source[self.start..self.current];
        let t_type = match RESERVED_KEYWORDS.get(text) {
            Some(t) => t,
            None => &TokenType::Identifier,
        };

        self.tokens.push(Token::new(text.to_string(), *t_type, self.line, None));
    }

    fn string(&mut self) {
        while self.peek() != Some('"') && !self.is_at_end() {
            if self.peek() == Some('\n') { self.line += 1; }
            self.advance();
        }

        if self.is_at_end() {
            let e = LexerError::UnterminatedString { line: self.line };
            eprintln!("{}", e);
            self.errors.push(e);
            return;
        }

        self.advance();

        let val = &self.source[self.start+1..self.current-1];
        self.tokens.push(Token::new(self.source[self.start..self.current].to_string(), TokenType::String, self.line, Some(Literal::String(val.to_string()))));
    }

    fn number(&mut self) {
        while self.peek().is_some_and(|c| c.is_ascii_digit()) { self.advance(); }

        if self.peek() == Some('.') && self.peek_next().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit()) { self.advance(); }
        }

        let num = self.source[self.start..self.current].parse().expect("Failed to parse number");
        self.tokens.push(Token::new(self.source[self.start..self.current].to_string(), TokenType::Number, self.line, Some(Literal::Number(num))));
    }

    fn char_at(&self, n: usize) -> Option<char> {
        self.source.chars().nth(n)
    }

    fn peek(&self) -> Option<char> {
        if self.is_at_end() { return None; }
        self.char_at(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() { return None; }
        self.char_at(self.current + 1)
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
